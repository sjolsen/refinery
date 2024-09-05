#include <Library/BoraxInterpreter.h>

#include <Library/BaseLib.h>
#include <Library/BoraxMemory.h>
#include <Library/DebugLib.h>
#include <Library/MemoryAllocationLib.h>
#include <Library/UefiBootServicesTableLib.h>

#define GC_PAGE_THRESHOLD_MIN     10
#define GC_PAGE_THRESHOLD_FACTOR  2

#define STACK_PAGE_MIN     1
#define STACK_PAGE_FACTOR  2

#define MULTIPLE_VALUES_MIN  8

STATIC EFI_STATUS
EFIAPI
TaskStackInit (
  OUT BORAX_TASK_STACK  *Stack
  )
{
  EFI_STATUS    Status;
  BORAX_OBJECT  **Pages = NULL;
  UINTN         I;

  Pages = AllocateZeroPool (STACK_PAGE_MIN * sizeof (VOID *));
  if (Pages == NULL) {
    Status = EFI_OUT_OF_RESOURCES;
    goto cleanup;
  }

  for (I = 0; I < STACK_PAGE_MIN; ++I) {
    Pages[I] = AllocatePages (1);
    if (Pages[I] == NULL) {
      Status = EFI_OUT_OF_RESOURCES;
      goto cleanup;
    }
  }

  Stack->Pages         = Pages;
  Stack->PagesLength   = STACK_PAGE_MIN;
  Stack->PagesCapacity = STACK_PAGE_MIN;

  Pages  = NULL;
  Status = EFI_SUCCESS;

cleanup:
  if (Pages != NULL) {
    for (I = 0; I < STACK_PAGE_MIN; ++I) {
      if (Pages[I] != NULL) {
        FreePages (Pages[I], 1);
      }
    }

    FreePool (Pages);
  }

  return Status;
}

STATIC VOID
EFIAPI
TaskStackCleanup (
  IN BORAX_TASK_STACK  *Stack
  )
{
  UINTN  I;

  for (I = 0; I < Stack->PagesCapacity; ++I) {
    if (Stack->Pages[I] != NULL) {
      FreePages (Stack->Pages[I], 1);
    }
  }

  FreePool (Stack->Pages);
}

STATIC EFI_STATUS
EFIAPI
TaskStackEnsureCapacity (
  IN BORAX_TASK_STACK  *Stack,
  IN UINTN             Words
  )
{
  EFI_STATUS  Status;
  UINTN       Pages          = (Words + BORAX_WORDS_PER_PAGE - 1) / BORAX_WORDS_PER_PAGE;
  UINTN       NewPagesLength = Stack->PagesLength;
  UINTN       I;

  // Allocate space for the page pointers
  if (Stack->PagesCapacity < Pages) {
    UINTN  NewPagesCapacity = MAX (
                                Stack->PagesCapacity * STACK_PAGE_FACTOR,
                                Pages
                                );
    BORAX_OBJECT  **NewPages = ReallocatePool (
                                 Stack->PagesCapacity,
                                 NewPagesCapacity,
                                 Stack->Pages
                                 );

    if (NewPages == NULL) {
      Status = EFI_OUT_OF_RESOURCES;
      goto cleanup;
    }

    Stack->Pages         = NewPages;
    Stack->PagesCapacity = NewPagesCapacity;
  }

  // Allocate pages
  for ( ; NewPagesLength < Pages; ++NewPagesLength) {
    Stack->Pages[NewPagesLength] = AllocatePages (1);
    if (Stack->Pages[NewPagesLength] == NULL) {
      Status = EFI_OUT_OF_RESOURCES;
      goto cleanup;
    }
  }

  Stack->PagesLength = NewPagesLength;
  Status             = EFI_SUCCESS;

cleanup:
  // On error, any newly allocated pages will lie between PagesLength and
  // NewPagesLength. On success, these indices are equal. We could undo the
  // capacity increase as well, but this is less important (and not required for
  // correctness).
  for (I = Stack->PagesLength; I < NewPagesLength; ++I) {
    FreePages (Stack->Pages[I], 1);
    Stack->Pages[I] = NULL;
  }

  return Status;
}

STATIC EFI_STATUS
EFIAPI
TaskRun (
  IN BORAX_TASK  *Task
  )
{
  while (Task->State == BORAX_TASK_RUNNING) {
    EFI_STATUS               Status;
    UINTN                    BP   = Task->Registers.BP;
    BORAX_OBJECT             Code = BoraxTaskStackRead (Task, BP + 2);
    BORAX_BUILT_IN_FUNCTION  *F;

    // TODO: Non-built-ins
    if (BORAX_DISCRIMINATE (Code) != BORAX_DISCRIM_BUILT_IN_FUNCTION) {
      // TODO: Error reporting
      return EFI_INVALID_PARAMETER;
    }

    F = (BORAX_BUILT_IN_FUNCTION *)BORAX_GET_POINTER (Code);

    Status = F->Code (Task);
    if (EFI_ERROR (Status)) {
      return Status;
    }
  }

  return EFI_SUCCESS;
}

STATIC VOID
EFIAPI
TaskEnd (
  IN BORAX_TASK  *Task
  )
{
  if (Task->Result != NULL) {
    Task->Result->Object = Task->Registers.VR;
  }

  if (Task->Completion != NULL) {
    gBS->SignalEvent (Task->Completion);
  }

  TaskStackCleanup (&Task->Stack);
  RemoveEntryList (&Task->TaskList);
  BoraxReleasePinRecord (&Task->Record);
}

EFI_STATUS
EFIAPI
BoraxInterpreterInit (
  OUT BORAX_INTERPRETER  *Interp,
  IN BORAX_ALLOCATOR     *Alloc,
  IN BORAX_PIN           *GlobalEnvironment
  )
{
  Interp->Alloc             = Alloc;
  Interp->GlobalEnvironment = GlobalEnvironment;
  Interp->GcPageThreshold   = MAX (
                                GC_PAGE_THRESHOLD_MIN,
                                GC_PAGE_THRESHOLD_FACTOR * Alloc->UsedPages
                                );
  InitializeListHead (&Interp->TaskList);

  return EFI_SUCCESS;
}

VOID
EFIAPI
BoraxInterpreterCleanup (
  IN BORAX_INTERPRETER  *Interp
  )
{
  LIST_ENTRY  *Entry, *NextEntry;

  BASE_LIST_FOR_EACH_SAFE (Entry, NextEntry, &Interp->TaskList) {
    BORAX_TASK  *Task = BASE_CR (Entry, BORAX_TASK, TaskList);

    TaskEnd (Task);
  }
}

EFI_STATUS
EFIAPI
BoraxInterpreterSpawn (
  IN BORAX_INTERPRETER  *Interp,
  IN EFI_EVENT          Completion  OPTIONAL,
  IN OUT BORAX_PIN      *Result     OPTIONAL,
  IN BORAX_OBJECT       EntryPoint,
  IN BORAX_OBJECT       Args
  )
{
  EFI_STATUS        Status;
  BORAX_TASK        *Task  = NULL;
  BORAX_TASK_STACK  *Stack = NULL;

  if ((Completion == NULL) && (Result != NULL)) {
    return EFI_INVALID_PARAMETER;
  }

  Status = BoraxAllocatePinRecord (
             Interp->Alloc,
             BORAX_WIDETAG_TASK,
             sizeof (BORAX_TASK),
             (BORAX_PIN_RECORD **)&Task
             );
  if (EFI_ERROR (Status)) {
    goto cleanup;
  }

  Status = TaskStackInit (&Task->Stack);
  if (EFI_ERROR (Status)) {
    goto cleanup;
  }

  Stack = &Task->Stack;

  Task->State      = BORAX_TASK_RUNNING;
  Task->Completion = Completion;
  Task->Result     = Result;

  Task->Registers.BP = 0;
  Task->Registers.SP = 0;
  Task->Registers.PC = 0;
  Task->Registers.VR = Args;

  Status = BoraxTaskEnterFunction (Task, EntryPoint);
  if (EFI_ERROR (Status)) {
    goto cleanup;
  }

  InsertTailList (&Interp->TaskList, &Task->TaskList);
  Task   = NULL;
  Stack  = NULL;
  Status = EFI_SUCCESS;

cleanup:
  if (Stack != NULL) {
    TaskStackCleanup (Stack);
  }

  if (Task != NULL) {
    BoraxReleasePinRecord (&Task->Record);
  }

  return Status;
}

EFI_STATUS
EFIAPI
BoraxInterpreterRun (
  IN BORAX_INTERPRETER  *Interp,
  OUT BORAX_PIN         **IORequests
  )
{
  EFI_STATUS  Status;
  LIST_ENTRY  *Entry, *NextEntry;
  BOOLEAN     WorkDone;

  do {
    WorkDone = FALSE;

    BASE_LIST_FOR_EACH_SAFE (Entry, NextEntry, &Interp->TaskList) {
      BORAX_TASK  *Task = BASE_CR (Entry, BORAX_TASK, TaskList);

      if (Task->State == BORAX_TASK_RUNNING) {
        WorkDone = TRUE;
        Status   = TaskRun (Task);
        if (EFI_ERROR (Status)) {
          // TODO: principled error handling
          return Status;
        }
      }

      if (Task->State == BORAX_TASK_EXITED) {
        TaskEnd (Task);
      }
    }
  } while (WorkDone);

  // TODO
  *IORequests = NULL;
  return EFI_SUCCESS;
}

VOID
EFIAPI
BoraxInterpreterShutdown (
  IN BORAX_INTERPRETER  *Interp
  )
{
  // TODO
}

BORAX_OBJECT
EFIAPI
BoraxTaskStackRead (
  IN BORAX_TASK  *Task,
  IN UINTN       Index
  )
{
  UINTN  PageIndex  = Index / BORAX_WORDS_PER_PAGE;
  UINTN  PageOffset = Index % BORAX_WORDS_PER_PAGE;

  ASSERT (PageIndex < Task->Stack.PagesLength);
  return Task->Stack.Pages[PageIndex][PageOffset];
}

VOID
EFIAPI
BoraxTaskStackWrite (
  IN BORAX_TASK    *Task,
  IN UINTN         Index,
  IN BORAX_OBJECT  Value
  )
{
  UINTN  PageIndex  = Index / BORAX_WORDS_PER_PAGE;
  UINTN  PageOffset = Index % BORAX_WORDS_PER_PAGE;

  ASSERT (PageIndex < Task->Stack.PagesLength);
  Task->Stack.Pages[PageIndex][PageOffset] = Value;
}

EFI_STATUS
EFIAPI
BoraxTaskEnterFunction (
  IN BORAX_TASK    *Task,
  IN BORAX_OBJECT  Function
  )
{
  EFI_STATUS               Status;
  BORAX_BUILT_IN_FUNCTION  *F;
  UINTN                    Locals, Entry;
  UINTN                    NewBP, NewSP;
  UINTN                    I;

  // TODO: Non-built-ins
  if (BORAX_DISCRIMINATE (Function) != BORAX_DISCRIM_BUILT_IN_FUNCTION) {
    // TODO: Error reporting
    return EFI_INVALID_PARAMETER;
  }

  F = (BORAX_BUILT_IN_FUNCTION *)BORAX_GET_POINTER (Function);

  if (!BORAX_IS_FIXNUM (F->Locals)) {
    return EFI_INVALID_PARAMETER;
  }

  Locals = BORAX_GET_FIXNUM (F->Locals);

  if (!BORAX_IS_FIXNUM (F->Entry)) {
    return EFI_INVALID_PARAMETER;
  }

  Entry = BORAX_GET_FIXNUM (F->Entry);

  // TODO: Shared bindings
  NewBP = Task->Registers.SP;
  NewSP = NewBP + 4 + Locals;

  Status = TaskStackEnsureCapacity (&Task->Stack, NewSP);
  if (EFI_ERROR (Status)) {
    return Status;
  }

  BoraxTaskStackWrite (Task, NewBP, BORAX_MAKE_FIXNUM (Task->Registers.BP));
  BoraxTaskStackWrite (Task, NewBP + 1, BORAX_MAKE_FIXNUM (Task->Registers.PC));
  BoraxTaskStackWrite (Task, NewBP + 2, Function);
  BoraxTaskStackWrite (Task, NewBP + 3, BORAX_IMMEDIATE_UNBOUND);

  for (I = 0; I < Locals; ++I) {
    BoraxTaskStackWrite (Task, NewBP + 4 + I, BORAX_IMMEDIATE_UNBOUND);
  }

  Task->Registers.SP = NewSP;
  Task->Registers.BP = NewBP;
  Task->Registers.PC = Entry;

  return EFI_SUCCESS;
}

VOID
EFIAPI
BoraxTaskExitFunction (
  IN BORAX_TASK  *Task
  )
{
  UINTN  NewSP = Task->Registers.BP;

  Task->Registers.SP = NewSP;
  Task->Registers.BP = BORAX_GET_FIXNUM (BoraxTaskStackRead (Task, NewSP));
  Task->Registers.PC = BORAX_GET_FIXNUM (BoraxTaskStackRead (Task, NewSP + 1));

  // TODO: Shrink the stack if appropriate
}

STATIC EFI_STATUS
EFIAPI
TaskSubObjects (
  IN BORAX_OBJECT_HEADER          *Object,
  IN VOID                         *Ctx,
  IN BORAX_GC_SUBOBJECT_CALLBACK  Callback
  )
{
  EFI_STATUS  Status;
  BORAX_TASK  *Task = (BORAX_TASK *)Object;
  UINTN       I;

  for (I = 0; I < Task->Registers.SP; ++I) {
    UINTN  PageIndex  = I / BORAX_WORDS_PER_PAGE;
    UINTN  PageOffset = I % BORAX_WORDS_PER_PAGE;

    Status = Callback (Ctx, &Task->Stack.Pages[PageIndex][PageOffset]);
    if (EFI_ERROR (Status)) {
      return Status;
    }
  }

  return Callback (Ctx, &Task->Registers.VR);
}

CONST BORAX_GC_HOOKS  gTaskGcHooks = {
  .Copy       = &BoraxGcHookNoCopy,  // pin record
  .SubObjects = &TaskSubObjects,
};

EFI_STATUS
EFIAPI
BoraxMakeMultipleValues (
  IN BORAX_INTERPRETER       *Interp,
  IN UINTN                   ValuesLength,
  OUT BORAX_MULTIPLE_VALUES  **Values
  )
{
  EFI_STATUS                Status;
  UINTN                     Capacity;
  BORAX_MULTIPLE_VALUES     *NewMV;
  BORAX_GLOBAL_ENVIRONMENT  *Env;

  Capacity = MAX (MULTIPLE_VALUES_MIN, ValuesLength);

  Status = BORAX_GET_OBJECT_RECORD (Interp->GlobalEnvironment->Object, &Env);
  if (EFI_ERROR (Status)) {
    return Status;
  }

  Status = BoraxAllocateRecord (
             Interp->Alloc,
             BORAX_WIDETAG_OBJECT_RECORD,
             Env->ClassMultipleValues,
             BORAX_RECORD_LENGTH (BORAX_MULTIPLE_VALUES) + Capacity,
             0, // LengthAux
             BORAX_IMMEDIATE_UNBOUND,
             (BORAX_RECORD **)&NewMV
             );
  if (EFI_ERROR (Status)) {
    return Status;
  }

  NewMV->ValuesLength = BORAX_MAKE_FIXNUM (ValuesLength);
  *Values             = NewMV;
  return EFI_SUCCESS;
}

EFI_STATUS
EFIAPI
BoraxMakeBuiltInFunction (
  IN BORAX_ALLOCATOR           *Alloc,
  IN CONST CHAR16              *Name,
  IN BORAX_OBJECT              Arglist,
  IN BORAX_OBJECT              Entry,
  IN BORAX_BUILT_IN_CODE       Code,
  IN BORAX_OBJECT              Constants,
  IN BORAX_OBJECT              Locals,
  IN BORAX_OBJECT              Shared,
  IN BORAX_OBJECT              Closure,
  OUT BORAX_BUILT_IN_FUNCTION  **Function
  )
{
  EFI_STATUS               Status;
  BORAX_BUILT_IN_FUNCTION  *NewFunction;

  // Allocate a regular lisp object
  Status = BoraxAllocateObject (
             Alloc,
             sizeof (BORAX_BUILT_IN_FUNCTION),
             (BORAX_OBJECT_HEADER **)&NewFunction
             );
  if (EFI_ERROR (Status)) {
    return Status;
  }

  // Initialize the record
  NewFunction->Header.WideTag = BORAX_WIDETAG_BUILT_IN_FUNCTION;
  NewFunction->Code           = Code;
  NewFunction->Constants      = Constants;
  NewFunction->Locals         = Locals;
  NewFunction->Shared         = Shared;
  NewFunction->Closure        = Closure;
  NewFunction->Name           = Name;
  NewFunction->Arglist        = Arglist;
  NewFunction->Entry          = Entry;

  *Function = NewFunction;
  return EFI_SUCCESS;
}

STATIC EFI_STATUS
EFIAPI
CopyBuiltInFunction (
  IN BORAX_ALLOCATOR       *Alloc,
  IN BORAX_OBJECT_HEADER   *OldObject,
  OUT BORAX_OBJECT_HEADER  **NewObject
  )
{
  return BoraxCopyObject (
           Alloc,
           sizeof (BORAX_BUILT_IN_FUNCTION),
           OldObject,
           NewObject
           );
}

STATIC EFI_STATUS
EFIAPI
BuiltInFunctionSubObjects (
  IN BORAX_OBJECT_HEADER          *Object,
  IN VOID                         *Ctx,
  IN BORAX_GC_SUBOBJECT_CALLBACK  Callback
  )
{
  EFI_STATUS               Status;
  BORAX_BUILT_IN_FUNCTION  *Function = (BORAX_BUILT_IN_FUNCTION *)Object;

  Status = Callback (Ctx, &Function->Constants);
  if (EFI_ERROR (Status)) {
    return Status;
  }

  Status = Callback (Ctx, &Function->Locals);
  if (EFI_ERROR (Status)) {
    return Status;
  }

  Status = Callback (Ctx, &Function->Shared);
  if (EFI_ERROR (Status)) {
    return Status;
  }

  Status = Callback (Ctx, &Function->Closure);
  if (EFI_ERROR (Status)) {
    return Status;
  }

  Status = Callback (Ctx, &Function->Arglist);
  if (EFI_ERROR (Status)) {
    return Status;
  }

  return Callback (Ctx, &Function->Entry);
}

CONST BORAX_GC_HOOKS  gBuiltInFunctionGcHooks = {
  .Copy       = &CopyBuiltInFunction,
  .SubObjects = &BuiltInFunctionSubObjects,
};
