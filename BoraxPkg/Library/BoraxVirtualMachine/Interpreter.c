#include <Library/BoraxInterpreter.h>

#include <Library/BaseLib.h>
#include <Library/BoraxMemory.h>
#include <Library/MemoryAllocationLib.h>
#include <Library/UefiBootServicesTableLib.h>

#define GC_PAGE_THRESHOLD_MIN     10
#define GC_PAGE_THRESHOLD_FACTOR  2

#define STACK_PAGE_MIN     1
#define STACK_PAGE_FACTOR  2

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

VOID
EFIAPI
BoraxInterpreterCleanup (
  IN BORAX_INTERPRETER  *Interp
  )
{
  LIST_ENTRY  *Entry, *NextEntry;

  BASE_LIST_FOR_EACH_SAFE (Entry, NextEntry, &Interp->TaskList) {
    BORAX_TASK  *Task = BASE_CR (Entry, BORAX_TASK, TaskList);

    if (Task->Result != NULL) {
      // TODO: Return a meaningful value indicating abnormal exit
      Task->Result->Object = BORAX_IMMEDIATE_UNBOUND;
    }

    if (Task->Completion != NULL) {
      gBS->SignalEvent (Task->Completion);
    }

    TaskStackCleanup (&Task->Stack);
    BoraxReleasePinRecord (&Task->Record);
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
  EFI_STATUS  Status;
  BORAX_TASK  *Task = NULL;

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

  Task->State        = BORAX_TASK_RUNNING;
  Task->Registers.BP = 0;
  Task->Registers.SP = 0;
  Task->Completion   = Completion;
  Task->Result       = Result;

  InsertTailList (&Interp->TaskList, &Task->TaskList);
  Task   = NULL;
  Status = EFI_SUCCESS;

cleanup:
  if (Task != NULL) {
    BoraxReleasePinRecord (&Task->Record);
  }

  return Status;
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

  return EFI_SUCCESS;
}

CONST BORAX_GC_HOOKS  gTaskGcHooks = {
  .Copy       = &BoraxGcHookNoCopy,  // pin record
  .SubObjects = &TaskSubObjects,
};

EFI_STATUS
EFIAPI
BoraxInterpreterRun (
  IN BORAX_INTERPRETER  *Interp,
  OUT BORAX_PIN         **IORequests
  )
{
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

EFI_STATUS
EFIAPI
BoraxSetSymbolFunction (
  IN BORAX_INTERPRETER  *Interp,
  IN BORAX_OBJECT       Symbol,
  IN BORAX_OBJECT       Function
  )
{
  EFI_STATUS    Status;
  BORAX_SYMBOL  *TheSymbol;

  Status = BORAX_GET_OBJECT_RECORD (Symbol, &TheSymbol);
  if (EFI_ERROR (Status)) {
    return Status;
  }

  TheSymbol->Function = Function;
  return EFI_SUCCESS;
}

EFI_STATUS
EFIAPI
BoraxAllocateBuiltInFunction (
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
