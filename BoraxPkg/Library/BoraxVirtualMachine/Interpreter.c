#include <Library/BoraxInterpreter.h>

#include <Library/BaseLib.h>
#include <Library/BoraxBytecode.h>
#include <Library/BoraxMemory.h>
#include <Library/BoraxPrimitive.h>
#include <Library/DebugLib.h>
#include <Library/MemoryAllocationLib.h>
#include <Library/UefiBootServicesTableLib.h>

#define GC_PAGE_THRESHOLD_MIN     10
#define GC_PAGE_THRESHOLD_FACTOR  2

#define STACK_PAGE_MIN     1
#define STACK_PAGE_MAX     32
#define STACK_PAGE_FACTOR  2

#define MULTIPLE_VALUES_MIN  8

#define STACK_TRACE_LIMIT  16

STATIC EFI_STATUS
EFIAPI
VirtualSubObject (
  IN VOID                         *Ctx,
  IN BORAX_GC_SUBOBJECT_CALLBACK  Callback,
  IN OUT VOID                     **Ptr
  )
{
  EFI_STATUS    Status;
  BORAX_OBJECT  Object;

  Object = BORAX_MAKE_POINTER (*Ptr);
  Status = Callback (Ctx, &Object);
  ASSERT (BORAX_IS_POINTER (Object));
  *Ptr = BORAX_GET_POINTER (Object);
  return Status;
}

STATIC BORAX_OBJECT
EFIAPI
UnsafeStackRead (
  IN BORAX_TASK  *Task,
  IN UINTN       Index
  )
{
  return *BoraxTaskUnsafeStackAddress (Task, Index);
}

STATIC VOID
EFIAPI
UnsafeStackWrite (
  IN BORAX_TASK    *Task,
  IN UINTN         Index,
  IN BORAX_OBJECT  Value
  )
{
  *BoraxTaskUnsafeStackAddress (Task, Index) = Value;
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

  Pages = AllocateZeroPool (STACK_PAGE_MIN * sizeof (BORAX_OBJECT *));
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

STATIC BORAX_OBJECT
EFIAPI
TaskStackEnsureCapacity (
  IN BORAX_INTERPRETER  *Interp,
  IN BORAX_TASK_STACK   *Stack,
  IN UINTN              Words
  )
{
  BORAX_OBJECT  Condition;
  UINTN         Pages          = (Words + BORAX_WORDS_PER_PAGE - 1) / BORAX_WORDS_PER_PAGE;
  UINTN         NewPagesLength = Stack->PagesLength;
  UINTN         I;

  if (Pages > STACK_PAGE_MAX) {
    return BoraxPrimitiveStackExhausted (Interp);
  }

  // Allocate space for the page pointers
  if (Stack->PagesCapacity < Pages) {
    UINTN  NewPagesCapacity = MAX (
                                Stack->PagesCapacity * STACK_PAGE_FACTOR,
                                Pages
                                );
    BORAX_OBJECT  **NewPages = ReallocatePool (
                                 Stack->PagesCapacity * sizeof (BORAX_OBJECT *),
                                 NewPagesCapacity * sizeof (BORAX_OBJECT *),
                                 Stack->Pages
                                 );

    if (NewPages == NULL) {
      Condition = BoraxPrimitiveHeapExhausted (Interp);
      goto cleanup;
    }

    Stack->Pages         = NewPages;
    Stack->PagesCapacity = NewPagesCapacity;
  }

  // Allocate pages
  for ( ; NewPagesLength < Pages; ++NewPagesLength) {
    Stack->Pages[NewPagesLength] = AllocatePages (1);
    if (Stack->Pages[NewPagesLength] == NULL) {
      Condition = BoraxPrimitiveHeapExhausted (Interp);
      goto cleanup;
    }
  }

  Stack->PagesLength = NewPagesLength;
  Condition          = BORAX_NIL;

cleanup:
  // On error, any newly allocated pages will lie between PagesLength and
  // NewPagesLength. On success, these indices are equal. We could undo the
  // capacity increase as well, but this is less important (and not required for
  // correctness).
  for (I = Stack->PagesLength; I < NewPagesLength; ++I) {
    FreePages (Stack->Pages[I], 1);
    Stack->Pages[I] = NULL;
  }

  return Condition;
}

STATIC VOID
EFIAPI
TaskRun (
  IN BORAX_TASK  *Task
  )
{
  BORAX_OBJECT              Condition;
  BORAX_OBJECT              Code;
  CONST BORAX_FUNCTION_OPS  *Ops;

  if (Task->State == BORAX_TASK_STARTING) {
    Task->State = BORAX_TASK_RUNNING;
    Condition   = BoraxTaskEnterFunction (Task, Task->EntryPoint);
    if (BORAX_BOOL (Condition)) {
      goto signal;
    }
  }

  Code = UnsafeStackRead (Task, Task->Registers.BP + BORAX_STACK_CODE);

  Condition = BoraxResolveFunction (Task->Interp, &Code, &Ops);
  if (BORAX_BOOL (Condition)) {
    goto signal;
  }

  Condition = Ops->Run (Task, Code);
  if (BORAX_BOOL (Condition)) {
    goto signal;
  }

signal:
  if (BORAX_BOOL (Condition)) {
    BORAX_OBJECT  Condition2 = BoraxTaskError (Task, Condition);
    if (BORAX_BOOL (Condition2)) {
      Task->DoubleFault.Condition1 = Condition;
      Task->DoubleFault.Condition2 = Condition2;
      Task->State                  = BORAX_TASK_DOUBLE_FAULTED;
    }
  }

  // TODO: Only when crossing the threshold
  BoraxAllocatorCollect (Task->Interp->Alloc);
}

STATIC VOID
EFIAPI
TaskEnd (
  IN BORAX_TASK  *Task
  )
{
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
  IN BORAX_ALLOCATOR     *Alloc,
  IN BORAX_OBJECT        GlobalEnvironment,
  OUT BORAX_INTERPRETER  **Interp
  )
{
  EFI_STATUS         Status;
  BORAX_INTERPRETER  *NewInterp = NULL;

  Status = BoraxAllocatePinRecord (
             Alloc,
             BORAX_WIDETAG_INTERPRETER,
             sizeof (BORAX_INTERPRETER),
             (BORAX_PIN_RECORD **)&NewInterp
             );
  if (EFI_ERROR (Status)) {
    goto cleanup;
  }

  Status = BORAX_GET_OBJECT_RECORD (
             GlobalEnvironment,
             &NewInterp->GlobalEnvironment
             );
  if (EFI_ERROR (Status)) {
    goto cleanup;
  }

  NewInterp->Alloc           = Alloc;
  NewInterp->GcPageThreshold = MAX (
                                 GC_PAGE_THRESHOLD_MIN,
                                 GC_PAGE_THRESHOLD_FACTOR * Alloc->UsedPages
                                 );
  InitializeListHead (&NewInterp->TaskList);

  Status = BoraxGlobalInit (NewInterp);
  if (EFI_ERROR (Status)) {
    goto cleanup;
  }

  *Interp   = NewInterp;
  NewInterp = NULL;
  Status    = EFI_SUCCESS;

cleanup:
  if (NewInterp != NULL) {
    BoraxReleasePinRecord (&NewInterp->Record);
  }

  return Status;
}

STATIC EFI_STATUS
EFIAPI
InterpreterSubObjects (
  IN BORAX_OBJECT_HEADER          *Object,
  IN VOID                         *Ctx,
  IN BORAX_GC_SUBOBJECT_CALLBACK  Callback
  )
{
  EFI_STATUS         Status;
  BORAX_INTERPRETER  *Interp = (BORAX_INTERPRETER *)Object;
  UINTN              I;

  Status = VirtualSubObject (Ctx, Callback, (VOID **)&Interp->GlobalEnvironment);
  if (EFI_ERROR (Status)) {
    return Status;
  }

  for (I = 0; I < BORAX_GLOBAL_COUNT; ++I) {
    Status = Callback (Ctx, &Interp->Globals[I]);
    if (EFI_ERROR (Status)) {
      return Status;
    }
  }

  return EFI_SUCCESS;
}

CONST BORAX_GC_HOOKS  gInterpreterGcHooks = {
  .Copy       = &BoraxGcHookNoCopy,  // pin record
  .SubObjects = &InterpreterSubObjects,
};

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

  BoraxReleasePinRecord (&Interp->Record);
}

EFI_STATUS
EFIAPI
BoraxInterpreterSpawn (
  IN BORAX_INTERPRETER  *Interp,
  IN EFI_EVENT          Completion  OPTIONAL,
  IN BORAX_OBJECT       ErrorHandler  OPTIONAL,
  IN BORAX_OBJECT       EntryPoint,
  IN BORAX_OBJECT       Args,
  OUT BORAX_TASK        **Task      OPTIONAL
  )
{
  EFI_STATUS        Status;
  BORAX_TASK        *NewTask = NULL;
  BORAX_TASK_STACK  *Stack   = NULL;

  if (BORAX_DISCRIMINATE (Args) != BORAX_DISCRIM_MULTIPLE_VALUES) {
    return EFI_INVALID_PARAMETER;
  }

  Status = BoraxAllocatePinRecord (
             Interp->Alloc,
             BORAX_WIDETAG_TASK,
             sizeof (BORAX_TASK),
             (BORAX_PIN_RECORD **)&NewTask
             );
  if (EFI_ERROR (Status)) {
    goto cleanup;
  }

  Status = TaskStackInit (&NewTask->Stack);
  if (EFI_ERROR (Status)) {
    goto cleanup;
  }

  Stack = &NewTask->Stack;

  NewTask->Interp       = Interp;
  NewTask->State        = BORAX_TASK_STARTING;
  NewTask->ErrorHandler = ErrorHandler;
  NewTask->EntryPoint   = EntryPoint;
  NewTask->Completion   = Completion;

  NewTask->Registers.BP = 0;
  NewTask->Registers.SP = 0;
  NewTask->Registers.PC = 0;
  NewTask->Registers.LC = 0;
  NewTask->Registers.SC = 0;
  NewTask->Registers.VR = (BORAX_MULTIPLE_VALUES *)BORAX_GET_POINTER (Args);

  InsertTailList (&Interp->TaskList, &NewTask->TaskList);
  if (Task != NULL) {
    BoraxAcquirePinRecord (&NewTask->Record);
    *Task = NewTask;
  }

  NewTask = NULL;
  Stack   = NULL;
  Status  = EFI_SUCCESS;

cleanup:
  if (Stack != NULL) {
    TaskStackCleanup (Stack);
  }

  if (NewTask != NULL) {
    BoraxReleasePinRecord (&NewTask->Record);
  }

  return Status;
}

VOID
EFIAPI
BoraxInterpreterRun (
  IN BORAX_INTERPRETER  *Interp,
  OUT BORAX_PIN         **IORequests
  )
{
  LIST_ENTRY  *Entry, *NextEntry;
  BOOLEAN     WorkDone;

  do {
    WorkDone = FALSE;

    BASE_LIST_FOR_EACH_SAFE (Entry, NextEntry, &Interp->TaskList) {
      BORAX_TASK  *Task = BASE_CR (Entry, BORAX_TASK, TaskList);

      while (Task->State <= BORAX_TASK_RUNNING) {
        WorkDone = TRUE;
        TaskRun (Task);
      }

      if (Task->State >= BORAX_TASK_RETURNED) {
        TaskEnd (Task);
      }
    }
  } while (WorkDone);

  // TODO
  *IORequests = NULL;
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
BoraxMakeMultipleValues (
  IN BORAX_INTERPRETER       *Interp,
  IN UINTN                   Length,
  OUT BORAX_MULTIPLE_VALUES  **Values
  )
{
  EFI_STATUS             Status;
  UINTN                  Capacity;
  BORAX_MULTIPLE_VALUES  *NewMV;

  Capacity = MAX (MULTIPLE_VALUES_MIN, Length);

  Status = BoraxAllocateObject (
             Interp->Alloc,
             sizeof (BORAX_MULTIPLE_VALUES) + Capacity * sizeof (BORAX_OBJECT),
             (BORAX_OBJECT_HEADER **)&NewMV
             );
  if (EFI_ERROR (Status)) {
    return Status;
  }

  NewMV->Header.WideTag = BORAX_WIDETAG_MULTIPLE_VALUES;
  NewMV->Capacity       = Capacity;
  NewMV->Length         = Length;
  *Values               = NewMV;
  return EFI_SUCCESS;
}

STATIC EFI_STATUS
EFIAPI
CopyMultipleValues (
  IN BORAX_ALLOCATOR       *Alloc,
  IN BORAX_OBJECT_HEADER   *OldObject,
  OUT BORAX_OBJECT_HEADER  **NewObject
  )
{
  BORAX_MULTIPLE_VALUES  *Values = (BORAX_MULTIPLE_VALUES *)OldObject;
  UINTN                  Size    = sizeof (BORAX_MULTIPLE_VALUES)
                                   + sizeof (BORAX_OBJECT) * Values->Capacity;

  return BoraxCopyObject (Alloc, Size, OldObject, NewObject);
}

EFI_STATUS
EFIAPI
BoraxCopyMultipleValues (
  IN BORAX_INTERPRETER       *Interp,
  IN BORAX_MULTIPLE_VALUES   *In,
  OUT BORAX_MULTIPLE_VALUES  **Out
  )
{
  return CopyMultipleValues (
           Interp->Alloc,
           &In->Header,
           (BORAX_OBJECT_HEADER **)Out
           );
}

EFI_STATUS
EFIAPI
BoraxResizeMultipleValues (
  IN BORAX_INTERPRETER          *Interp,
  IN UINTN                      Length,
  IN OUT BORAX_MULTIPLE_VALUES  **Values
  )
{
  BORAX_MULTIPLE_VALUES  *MV = *Values;

  if (Length > MV->Capacity) {
    return BoraxMakeMultipleValues (Interp, Length, Values);
  } else {
    MV->Length = Length;
    return EFI_SUCCESS;
  }
}

STATIC EFI_STATUS
EFIAPI
MultipleValuesSubObjects (
  IN BORAX_OBJECT_HEADER          *Object,
  IN VOID                         *Ctx,
  IN BORAX_GC_SUBOBJECT_CALLBACK  Callback
  )
{
  EFI_STATUS             Status;
  BORAX_MULTIPLE_VALUES  *Values = (BORAX_MULTIPLE_VALUES *)Object;
  UINTN                  I;

  for (I = 0; I < Values->Length; ++I) {
    Status = Callback (Ctx, &Values->Values[I]);
    if (EFI_ERROR (Status)) {
      return Status;
    }
  }

  return EFI_SUCCESS;
}

CONST BORAX_GC_HOOKS  gMultipleValuesGcHooks = {
  .Copy       = &CopyMultipleValues,
  .SubObjects = &MultipleValuesSubObjects,
};

BORAX_OBJECT *
EFIAPI
BoraxTaskUnsafeStackAddress (
  IN BORAX_TASK  *Task,
  IN UINTN       Index
  )
{
  UINTN  PageIndex  = Index / BORAX_WORDS_PER_PAGE;
  UINTN  PageOffset = Index % BORAX_WORDS_PER_PAGE;

  ASSERT (Index < Task->Registers.SP);
  ASSERT (PageIndex < Task->Stack.PagesLength);
  return &Task->Stack.Pages[PageIndex][PageOffset];
}

BORAX_OBJECT
EFIAPI
BoraxTaskAccessLocal (
  IN BORAX_TASK     *Task,
  IN UINTN          Index,
  OUT BORAX_OBJECT  **Local
  )
{
  if (Index >= Task->Registers.LC) {
    return BoraxPrimitiveLocalLocationError (Task->Interp, Index);
  }

  *Local = BoraxTaskUnsafeStackAddress (
             Task,
             Task->Registers.BP + BORAX_STACK_SLOTS + Index
             );
  return BORAX_NIL;
}

BORAX_OBJECT
EFIAPI
BoraxTaskReadConstant (
  IN BORAX_TASK     *Task,
  IN UINTN          Index,
  OUT BORAX_OBJECT  *Constant
  )
{
  BORAX_OBJECT              Condition;
  BORAX_OBJECT              Code;
  CONST BORAX_FUNCTION_OPS  *Ops;

  Code = UnsafeStackRead (Task, Task->Registers.BP + BORAX_STACK_CODE);

  Condition = BoraxResolveFunction (Task->Interp, &Code, &Ops);
  if (BORAX_BOOL (Condition)) {
    return Condition;
  }

  return Ops->Constant (Task->Interp, Code, Index, Constant);
}

BORAX_OBJECT
EFIAPI
BoraxTaskBind (
  IN BORAX_TASK     *Task,
  IN UINTN          SlotsLength,
  OUT BORAX_OBJECT  **Slots
  )
{
  UINTN  I;

  if (Task->Registers.VR->Length != SlotsLength) {
    BORAX_OBJECT  Args[] = {
      BORAX_MAKE_FIXNUM (SlotsLength),
      BORAX_MAKE_FIXNUM (Task->Registers.VR->Length),
    };
    return BoraxPrimitiveSimpleCondition (
             Task->Interp,
             Task->Interp->Globals[BORAX_GLOBAL_CLASS_SIMPLE_PROGRAM_ERROR],
             L"Invalid argument count: expected ~S, got ~S",
             ARRAY_SIZE (Args),
             Args
             );
  }

  for (I = 0; I < SlotsLength; ++I) {
    *Slots[I] = Task->Registers.VR->Values[I];
  }

  return BORAX_NIL;
}

BORAX_OBJECT
EFIAPI
BoraxTaskCoBind (
  IN BORAX_TASK    *Task,
  IN UINTN         SlotsLength,
  IN BORAX_OBJECT  *Slots
  )
{
  EFI_STATUS  Status;
  UINTN       I;

  Status = BoraxResizeMultipleValues (
             Task->Interp,
             SlotsLength,
             &Task->Registers.VR
             );
  if (EFI_ERROR (Status)) {
    return BoraxPrimitiveHeapExhausted (Task->Interp);
  }

  for (I = 0; I < SlotsLength; ++I) {
    Task->Registers.VR->Values[I] = Slots[I];
  }

  return BORAX_NIL;
}

BORAX_OBJECT
EFIAPI
BoraxTaskEnterFunction (
  IN BORAX_TASK    *Task,
  IN BORAX_OBJECT  Function
  )
{
  BORAX_OBJECT              Condition;
  CONST BORAX_FUNCTION_OPS  *Ops;
  BORAX_FUNCTION_INFO       Info;
  UINTN                     OldBP, NewBP, NewSP;
  UINTN                     I;

  // Mutably resolving the function ensures the function object proper gets
  // written to the stack, not a symbol
  Condition = BoraxResolveFunction (Task->Interp, &Function, &Ops);
  if (BORAX_BOOL (Condition)) {
    return Condition;
  }

  Condition = Ops->Info (Task->Interp, Function, &Info);
  if (BORAX_BOOL (Condition)) {
    return Condition;
  }

  // TODO: Shared bindings
  if (Info.Shared != 0) {
    BORAX_OBJECT  Args[] = { Function };

    return BoraxPrimitiveSimpleCondition (
             Task->Interp,
             Task->Interp->Globals[BORAX_GLOBAL_CLASS_SIMPLE_ERROR],
             L"Not implemented: shared bindings (when entering ~S)",
             ARRAY_SIZE (Args),
             Args
             );
  }

  OldBP = Task->Registers.BP;
  NewBP = Task->Registers.SP;
  NewSP = NewBP + BORAX_STACK_SLOTS + Info.Locals + Info.Shared;

  Condition = TaskStackEnsureCapacity (Task->Interp, &Task->Stack, NewSP);
  if (BORAX_BOOL (Condition)) {
    return Condition;
  }

  Task->Registers.SP = NewSP;
  Task->Registers.BP = NewBP;

  UnsafeStackWrite (
    Task,
    NewBP + BORAX_STACK_SAVED_BP,
    BORAX_MAKE_FIXNUM (OldBP)
    );
  UnsafeStackWrite (
    Task,
    NewBP + BORAX_STACK_SAVED_PC,
    BORAX_MAKE_FIXNUM (Task->Registers.PC)
    );
  UnsafeStackWrite (
    Task,
    NewBP + BORAX_STACK_SAVED_LC,
    BORAX_MAKE_FIXNUM (Task->Registers.LC)
    );
  UnsafeStackWrite (
    Task,
    NewBP + BORAX_STACK_SAVED_SC,
    BORAX_MAKE_FIXNUM (Task->Registers.SC)
    );
  UnsafeStackWrite (Task, NewBP + BORAX_STACK_CODE, Function);
  UnsafeStackWrite (Task, NewBP + BORAX_STACK_CLOSURE, BORAX_UNBOUND);

  for (I = 0; I < Info.Locals; ++I) {
    UnsafeStackWrite (Task, NewBP + BORAX_STACK_SLOTS + I, BORAX_NIL);
  }

  Task->Registers.PC = Info.Entry;
  Task->Registers.LC = Info.Locals;
  Task->Registers.SC = Info.Shared;

  return BORAX_NIL;
}

STATIC BORAX_OBJECT
EFIAPI
UnwindFrame (
  IN BORAX_TASK    *Task,
  IN BORAX_OBJECT  TargetExit,
  OUT BOOLEAN      *Intercepted
  )
{
  BORAX_OBJECT  Condition;
  UINTN         BP = Task->Registers.BP;
  BOOLEAN       PopIntercepted;

  Condition = BoraxTaskPopDynamic (Task, 0, TargetExit, &PopIntercepted);
  if (BORAX_BOOL (Condition)) {
    return Condition;
  }

  if (PopIntercepted) {
    *Intercepted = TRUE;
    return BORAX_NIL;
  }

  Task->Registers.BP = BORAX_GET_FIXNUM (
                         UnsafeStackRead (Task, BP + BORAX_STACK_SAVED_BP)
                         );
  Task->Registers.PC = BORAX_GET_FIXNUM (
                         UnsafeStackRead (Task, BP + BORAX_STACK_SAVED_PC)
                         );
  Task->Registers.LC = BORAX_GET_FIXNUM (
                         UnsafeStackRead (Task, BP + BORAX_STACK_SAVED_LC)
                         );
  Task->Registers.SC = BORAX_GET_FIXNUM (
                         UnsafeStackRead (Task, BP + BORAX_STACK_SAVED_SC)
                         );
  Task->Registers.SP = BP;

  // TODO: Shrink the stack if appropriate
  *Intercepted = FALSE;
  return BORAX_NIL;
}

BORAX_OBJECT
EFIAPI
BoraxTaskEnterFunctionTail (
  IN BORAX_TASK    *Task,
  IN BORAX_OBJECT  Function
  )
{
  BORAX_OBJECT  Condition;
  BOOLEAN       Intercepted;

  Condition = UnwindFrame (Task, BORAX_NIL, &Intercepted);
  if (BORAX_BOOL (Condition) || Intercepted) {
    return Condition;
  }

  return BoraxTaskEnterFunction (Task, Function);
}

BORAX_OBJECT
EFIAPI
BoraxTaskExitFunction (
  IN BORAX_TASK  *Task
  )
{
  BORAX_OBJECT  Condition;
  BOOLEAN       Intercepted;

  Condition = UnwindFrame (Task, BORAX_NIL, &Intercepted);
  if (BORAX_BOOL (Condition) || Intercepted) {
    return Condition;
  }

  if (Task->Registers.SP == 0) {
    Task->State = BORAX_TASK_RETURNED;
  }

  return BORAX_NIL;
}

BORAX_OBJECT
EFIAPI
BoraxTaskPushExit (
  IN BORAX_TASK   *Task,
  IN UINTN        PC,
  OUT BORAX_EXIT  **Exit
  )
{
  EFI_STATUS    Status;
  BORAX_OBJECT  Condition;
  UINTN         NewSP = Task->Registers.SP + 1;
  BORAX_EXIT    *NewExit;

  Condition = TaskStackEnsureCapacity (Task->Interp, &Task->Stack, NewSP);
  if (BORAX_BOOL (Condition)) {
    return Condition;
  }

  Status = BoraxAllocateObject (
             Task->Interp->Alloc,
             sizeof (BORAX_EXIT),
             (BORAX_OBJECT_HEADER **)&NewExit
             );
  if (EFI_ERROR (Status)) {
    return BoraxPrimitiveHeapExhausted (Task->Interp);
  }

  NewExit->Valid = TRUE;
  NewExit->Task  = BORAX_MAKE_POINTER (Task);
  NewExit->BP    = Task->Registers.BP;
  NewExit->PC    = PC;

  Task->Registers.SP = NewSP;
  UnsafeStackWrite (Task, NewSP - 1, BORAX_MAKE_POINTER (NewExit));

  *Exit = NewExit;
  return BORAX_NIL;
}

STATIC EFI_STATUS
EFIAPI
CopyExit (
  IN BORAX_ALLOCATOR       *Alloc,
  IN BORAX_OBJECT_HEADER   *OldObject,
  OUT BORAX_OBJECT_HEADER  **NewObject
  )
{
  return BoraxCopyObject (
           Alloc,
           sizeof (BORAX_EXIT),
           OldObject,
           NewObject
           );
}

STATIC EFI_STATUS
EFIAPI
ExitSubObjects (
  IN BORAX_OBJECT_HEADER          *Object,
  IN VOID                         *Ctx,
  IN BORAX_GC_SUBOBJECT_CALLBACK  Callback
  )
{
  BORAX_EXIT  *Exit = (BORAX_EXIT *)Object;

  return Callback (Ctx, &Exit->Task);
}

CONST BORAX_GC_HOOKS  gExitGcHooks = {
  .Copy       = &CopyExit,
  .SubObjects = &ExitSubObjects,
};

BORAX_OBJECT
EFIAPI
BoraxTaskTakeExit (
  IN BORAX_TASK    *Task,
  IN BORAX_OBJECT  Exit
  )
{
  BORAX_OBJECT  Condition;
  BORAX_EXIT    *TheExit;

  if (BORAX_DISCRIMINATE (Exit) != BORAX_DISCRIM_EXIT) {
    BORAX_OBJECT  Args[] = { Exit };

    return BoraxPrimitiveSimpleCondition (
             Task->Interp,
             Task->Interp->Globals[BORAX_GLOBAL_CLASS_SIMPLE_PROGRAM_ERROR],
             L"Not an exit object: ~S",
             ARRAY_SIZE (Args),
             Args
             );
  }

  TheExit = (BORAX_EXIT *)BORAX_GET_POINTER (Exit);

  if (!TheExit->Valid) {
    BORAX_OBJECT  Args[] = { Exit };

    return BoraxPrimitiveSimpleCondition (
             Task->Interp,
             Task->Interp->Globals[BORAX_GLOBAL_CLASS_SIMPLE_PROGRAM_ERROR],
             L"Tried to take an expired exit: ~S",
             ARRAY_SIZE (Args),
             Args
             );
  }

  if (!BORAX_EQ (TheExit->Task, BORAX_MAKE_POINTER (Task))) {
    BORAX_OBJECT  Args[] = { Exit };

    return BoraxPrimitiveSimpleCondition (
             Task->Interp,
             Task->Interp->Globals[BORAX_GLOBAL_CLASS_SIMPLE_PROGRAM_ERROR],
             L"Tried to take an exit to another task: ~S",
             ARRAY_SIZE (Args),
             Args
             );
  }

  while (Task->Registers.BP > TheExit->BP) {
    BOOLEAN  Intercepted;

    Condition = UnwindFrame (Task, Exit, &Intercepted);
    if (BORAX_BOOL (Condition) || Intercepted) {
      return Condition;
    }
  }

  ASSERT (Task->Registers.BP == TheExit->BP);

  // The exit remains valid
  Task->Registers.PC = TheExit->PC;
  return BORAX_NIL;
}

BORAX_OBJECT
EFIAPI
BoraxTaskPopDynamic (
  IN BORAX_TASK    *Task,
  IN UINTN         Depth,
  IN BORAX_OBJECT  TargetExit,
  OUT BOOLEAN      *Intercepted
  )
{
  UINTN  DP = Task->Registers.BP + BORAX_STACK_SLOTS
              + Task->Registers.LC + Task->Registers.SC;

  if (Task->Registers.SP < DP + Depth) {
    BORAX_OBJECT  Args[] = {
      BORAX_MAKE_FIXNUM (Depth),
      BORAX_MAKE_FIXNUM (Task->Registers.SP - DP),
    };

    return BoraxPrimitiveSimpleCondition (
             Task->Interp,
             Task->Interp->Globals[BORAX_GLOBAL_CLASS_SIMPLE_PROGRAM_ERROR],
             L"Tried to PopDynamic to depth ~S, but frame has depth ~S",
             ARRAY_SIZE (Args),
             Args
             );
  }

  while (Task->Registers.SP > DP + Depth) {
    BORAX_OBJECT  Dynamic = UnsafeStackRead (Task, Task->Registers.SP - 1);

    switch (BORAX_DISCRIMINATE (Dynamic)) {
      case BORAX_DISCRIM_EXIT:
      {
        BORAX_EXIT  *Exit = (BORAX_EXIT *)BORAX_GET_POINTER (Dynamic);
        Exit->Valid = FALSE;
        break;
      }

      // TODO: make sure to set Intercepted when entering a cleanup

      default:
        // If the stack were to be corrupted to the point that we find something
        // other than a dynamic extent here, it could potentially result in us
        // failing to pop a special variable binding -- which would be
        // annoying -- or failing to invalidate a non-local exit, which could be
        // catastrophic. The memory safety of the C code depends on the stack
        // not being corrupted this way; we do not attempt to handle this
        // situation safely because if it happens, we've already lost.
        ASSERT (FALSE);
        break;
    }

    --Task->Registers.SP;
  }

  *Intercepted = FALSE;
  return BORAX_NIL;
}

BORAX_OBJECT
EFIAPI
BoraxTaskError (
  IN BORAX_TASK    *Task,
  IN BORAX_OBJECT  Condition
  )
{
  // TODO: Call ERROR and allow the condition to be handled
  if (BORAX_BOOL (Task->ErrorHandler)) {
    BORAX_OBJECT  Condition2;

    // TODO: Preserve VR when handling interpreter conditions
    Condition2 = BoraxTaskCoBind (Task, 1, &Condition);
    if (BORAX_BOOL (Condition2)) {
      return Condition2;
    }

    // TODO: Guard against infinite recursion
    return BoraxTaskEnterFunction (Task, Task->ErrorHandler);
  } else {
    // Allow the task to double-fault
    return Condition;
  }
}

VOID
EFIAPI
BoraxTaskAbort (
  IN BORAX_TASK    *Task,
  IN BORAX_OBJECT  Condition
  )
{
  Task->AbortCondition = Condition;
  Task->State          = BORAX_TASK_ABORTED;
}

BORAX_STACK_FRAME_ITERATOR
EFIAPI
BoraxStackFrameIterate (
  IN BORAX_TASK  *Task
  )
{
  return (BORAX_STACK_FRAME_ITERATOR) {
           .Task   = Task,
           .NextBP = Task->Registers.BP,
           .NextSP = Task->Registers.SP,
           .NextPC = Task->Registers.PC,
  };
}

BOOLEAN
EFIAPI
BoraxStackFrameNext (
  IN BORAX_STACK_FRAME_ITERATOR  *Iter,
  OUT BORAX_STACK_FRAME          *Frame
  )
{
  BORAX_TASK    *Task = Iter->Task;
  UINTN         BP    = Iter->NextBP;
  UINTN         SP    = Iter->NextSP;
  UINTN         PC    = Iter->NextPC;
  BORAX_OBJECT  Temp;

  if (SP == 0) {
    return FALSE;
  }

  ASSERT (SP >= BP + BORAX_STACK_SLOTS);

  Frame->Code = UnsafeStackRead (Task, BP + BORAX_STACK_CODE);
  Frame->BP   = BP;
  Frame->SP   = SP;
  Frame->PC   = PC;

  Temp = UnsafeStackRead (Task, BP + BORAX_STACK_SAVED_BP);
  ASSERT (BORAX_IS_FIXNUM (Temp));
  Iter->NextBP = BORAX_GET_FIXNUM (Temp);
  Iter->NextSP = BP;

  Temp = UnsafeStackRead (Task, BP + BORAX_STACK_SAVED_PC);
  ASSERT (BORAX_IS_FIXNUM (Temp));
  Iter->NextPC = BORAX_GET_FIXNUM (Temp);

  return TRUE;
}

VOID
EFIAPI
BoraxTaskDebugStackTrace (
  IN UINTN       ErrorLevel,
  IN BORAX_TASK  *Task
  )
{
  BORAX_STACK_FRAME_ITERATOR  Iter = BoraxStackFrameIterate (Task);
  BORAX_STACK_FRAME           Frame;

  BORAX_OBJECT  LastCode   = BORAX_UNBOUND;
  UINTN         LastPC     = 0;
  UINTN         Duplicates = 0;
  UINTN         Printed    = 0;
  UINTN         Skipped    = 0;

  DebugPrint (ErrorLevel, "Task %p stack trace:\n", Task);

  while (BoraxStackFrameNext (&Iter, &Frame)) {
    if (BORAX_EQ (Frame.Code, LastCode) && (Frame.PC == LastPC)) {
      ++Duplicates;
    } else {
      if (Duplicates != 0) {
        DebugPrint (
          ErrorLevel,
          "  (omitted %u duplicate entries)\n",
          Duplicates
          );
        Duplicates = 0;
      }

      if (Printed < STACK_TRACE_LIMIT) {
        BORAX_OBJECT              Condition;
        CONST BORAX_FUNCTION_OPS  *Ops;
        BORAX_OBJECT              Name;
        BORAX_PACKAGE             *Package;
        BORAX_SYMBOL              *Symbol;
        UINTN                     PackageLength, SymbolLength;
        CHAR16                    *PackageData, *SymbolData;
        BOOLEAN                   HaveName = FALSE;

        Condition = BoraxResolveFunction (Task->Interp, &Frame.Code, &Ops);
        if (BORAX_BOOL (Condition)) {
          goto print_name;
        }

        Condition = Ops->Name (Task->Interp, Frame.Code, &Name);
        if (BORAX_BOOL (Condition)) {
          goto print_name;
        }

        Condition = BoraxPrimitiveTheSymbol (
                      Task->Interp,
                      Name,
                      &Symbol
                      );
        if (BORAX_BOOL (Condition)) {
          goto print_name;
        }

        Condition = BoraxPrimitiveThePackage (
                      Task->Interp,
                      Symbol->Package,
                      &Package
                      );
        if (BORAX_BOOL (Condition)) {
          goto print_name;
        }

        Condition = BoraxPrimitiveStringData (
                      Task->Interp,
                      Package->Name,
                      &PackageLength,
                      &PackageData
                      );
        if (BORAX_BOOL (Condition)) {
          goto print_name;
        }

        Condition = BoraxPrimitiveStringData (
                      Task->Interp,
                      Symbol->Name,
                      &SymbolLength,
                      &SymbolData
                      );
        if (BORAX_BOOL (Condition)) {
          goto print_name;
        }

        HaveName = TRUE;

print_name:
        if (HaveName) {
          DebugPrint (
            ErrorLevel,
            "  %.*s:%.*s:%u\n",
            PackageLength,
            PackageData,
            SymbolLength,
            SymbolData,
            Frame.PC
            );
        } else {
          DebugPrint (ErrorLevel, "  <unknown>:%u\n", Frame.PC);
        }

        ++Printed;
      } else {
        ++Skipped;
      }
    }

    LastCode = Frame.Code;
    LastPC   = Frame.PC;
  }

  if (Duplicates != 0) {
    DebugPrint (
      ErrorLevel,
      "  (omitted %u duplicate entries)\n",
      Duplicates
      );
  }

  if (Skipped != 0) {
    DebugPrint (ErrorLevel, "  (skipped %u entries)\n", Skipped);
  }
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

  Status = VirtualSubObject (Ctx, Callback, (VOID **)&Task->Registers.VR);
  if (EFI_ERROR (Status)) {
    return Status;
  }

  Status = Callback (Ctx, &Task->ErrorHandler);
  if (EFI_ERROR (Status)) {
    return Status;
  }

  Status = Callback (Ctx, &Task->EntryPoint);
  if (EFI_ERROR (Status)) {
    return Status;
  }

  switch (Task->State) {
    case BORAX_TASK_ABORTED:
      Status = Callback (Ctx, &Task->AbortCondition);
      if (EFI_ERROR (Status)) {
        return Status;
      }

      break;

    case BORAX_TASK_DOUBLE_FAULTED:
      Status = Callback (Ctx, &Task->DoubleFault.Condition1);
      if (EFI_ERROR (Status)) {
        return Status;
      }

      Status = Callback (Ctx, &Task->DoubleFault.Condition2);
      if (EFI_ERROR (Status)) {
        return Status;
      }

      break;

    default:
      break;
  }

  return EFI_SUCCESS;
}

CONST BORAX_GC_HOOKS  gTaskGcHooks = {
  .Copy       = &BoraxGcHookNoCopy,  // pin record
  .SubObjects = &TaskSubObjects,
};

BORAX_OBJECT
EFIAPI
BoraxResolveFunction (
  IN BORAX_INTERPRETER          *Interp,
  IN OUT BORAX_OBJECT           *Function,
  OUT CONST BORAX_FUNCTION_OPS  **Ops
  )
{
  EFI_STATUS    Status;
  BORAX_OBJECT  ClassBytecodeFunction = Interp->Globals[BORAX_GLOBAL_CLASS_BYTECODE_FUNCTION];
  BORAX_OBJECT  ClassSymbol           = Interp->Globals[BORAX_GLOBAL_CLASS_SYMBOL];
  BORAX_OBJECT  Resolved              = *Function;

  // Do not use recursion to handle symbols in case someone has FBOUND a symbol
  // to itself
  if (BORAX_DISCRIMINATE (Resolved) == BORAX_DISCRIM_OBJECT_RECORD) {
    BORAX_RECORD  *Record = (BORAX_RECORD *)BORAX_GET_POINTER (Resolved);
    BORAX_SYMBOL  *Symbol;

    if (!BORAX_EQ (Record->Class, ClassSymbol)) {
      goto not_a_symbol;
    }

    // TODO: replace this macro with one that handles type checking too
    Status = BORAX_GET_OBJECT_RECORD (Resolved, &Symbol);
    if (EFI_ERROR (Status)) {
      goto not_a_symbol;
    }

    if (!BORAX_BOUNDP (Symbol->Function)) {
      return BoraxPrimitiveCellError (
               Interp,
               Interp->Globals[BORAX_GLOBAL_CLASS_UNDEFINED_FUNCTION],
               Resolved
               );
    }

    Resolved = Symbol->Function;
  }

not_a_symbol:
  switch (BORAX_DISCRIMINATE (Resolved)) {
    case BORAX_DISCRIM_BUILT_IN_FUNCTION:
      *Function = Resolved;
      *Ops      = &gBuiltInFunctionOps;
      return BORAX_NIL;

    case BORAX_DISCRIM_OBJECT_RECORD:
    {
      BORAX_RECORD             *Record = (BORAX_RECORD *)BORAX_GET_POINTER (Resolved);
      BORAX_BYTECODE_FUNCTION  *F;

      if (!BORAX_EQ (Record->Class, ClassBytecodeFunction)) {
        goto not_a_function;
      }

      // TODO: replace this macro with one that handles type checking too
      Status = BORAX_GET_OBJECT_RECORD (Resolved, &F);
      if (EFI_ERROR (Status)) {
        goto not_a_function;
      }

      *Function = Resolved;
      *Ops      = &gBytecodeFunctionOps;
      return BORAX_NIL;
    }

    default:
      goto not_a_function;
  }

not_a_function:
  // TODO: Strictly, this should be (OR FUNCTION SYMBOL)
  return BoraxPrimitiveTypeError (
           Interp,
           Resolved,
           Interp->Globals[BORAX_GLOBAL_CLASS_FUNCTION]
           );
}

EFI_STATUS
EFIAPI
BoraxMakeBuiltInFunction (
  IN BORAX_ALLOCATOR           *Alloc,
  IN BORAX_OBJECT              Name,
  IN BORAX_OBJECT              Arglist,
  IN UINTN                     Entry,
  IN BORAX_BUILT_IN_CODE       Code,
  IN UINTN                     Locals,
  IN UINTN                     SharedLength,
  IN UINTN                     *Shared  OPTIONAL,
  IN UINTN                     ConstantsLength,
  OUT BORAX_BUILT_IN_FUNCTION  **Function
  )
{
  EFI_STATUS               Status;
  BORAX_BUILT_IN_FUNCTION  *NewFunction;

  Status = BoraxAllocateObject (
             Alloc,
             sizeof (BORAX_BUILT_IN_FUNCTION)
             + ConstantsLength * sizeof (BORAX_OBJECT),
             (BORAX_OBJECT_HEADER **)&NewFunction
             );
  if (EFI_ERROR (Status)) {
    return Status;
  }

  NewFunction->Header.WideTag  = BORAX_WIDETAG_BUILT_IN_FUNCTION;
  NewFunction->Name            = Name;
  NewFunction->Arglist         = Arglist;
  NewFunction->Entry           = Entry;
  NewFunction->Code            = Code;
  NewFunction->Locals          = Locals;
  NewFunction->SharedLength    = SharedLength;
  NewFunction->Shared          = Shared;
  NewFunction->ConstantsLength = ConstantsLength;

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
  BORAX_BUILT_IN_FUNCTION  *Function = (BORAX_BUILT_IN_FUNCTION *)OldObject;

  return BoraxCopyObject (
           Alloc,
           sizeof (BORAX_BUILT_IN_FUNCTION)
           + Function->ConstantsLength * sizeof (BORAX_OBJECT),
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
  UINTN                    I;

  Status = Callback (Ctx, &Function->Name);
  if (EFI_ERROR (Status)) {
    return Status;
  }

  Status = Callback (Ctx, &Function->Arglist);
  if (EFI_ERROR (Status)) {
    return Status;
  }

  for (I = 0; I < Function->ConstantsLength; ++I) {
    Status = Callback (Ctx, &Function->Constants[I]);
    if (EFI_ERROR (Status)) {
      return Status;
    }
  }

  return EFI_SUCCESS;
}

CONST BORAX_GC_HOOKS  gBuiltInFunctionGcHooks = {
  .Copy       = &CopyBuiltInFunction,
  .SubObjects = &BuiltInFunctionSubObjects,
};

STATIC BORAX_OBJECT
EFIAPI
BuiltInFunctionRun (
  IN BORAX_TASK    *Task,
  IN BORAX_OBJECT  Function
  )
{
  BORAX_BUILT_IN_FUNCTION  *F = (BORAX_BUILT_IN_FUNCTION  *)BORAX_GET_POINTER (Function);

  return F->Code (Task);
}

STATIC BORAX_OBJECT
EFIAPI
BuiltInFunctionName (
  IN BORAX_INTERPRETER  *Interp,
  IN BORAX_OBJECT       Function,
  OUT BORAX_OBJECT      *Name
  )
{
  BORAX_BUILT_IN_FUNCTION  *F = (BORAX_BUILT_IN_FUNCTION  *)BORAX_GET_POINTER (Function);

  *Name = F->Name;
  return BORAX_NIL;
}

STATIC BORAX_OBJECT
EFIAPI
BuiltInFunctionInfo (
  IN BORAX_INTERPRETER     *Interp,
  IN BORAX_OBJECT          Function,
  OUT BORAX_FUNCTION_INFO  *Info
  )
{
  BORAX_BUILT_IN_FUNCTION  *F = (BORAX_BUILT_IN_FUNCTION  *)BORAX_GET_POINTER (Function);

  Info->Entry  = F->Entry;
  Info->Locals = F->Locals;
  Info->Shared = F->SharedLength;
  return BORAX_NIL;
}

STATIC BORAX_OBJECT
EFIAPI
BuiltInFunctionShared (
  IN BORAX_INTERPRETER  *Interp,
  IN BORAX_OBJECT       Function,
  IN UINTN              Block,
  OUT UINTN             *Count
  )
{
  BORAX_BUILT_IN_FUNCTION  *F = (BORAX_BUILT_IN_FUNCTION  *)BORAX_GET_POINTER (Function);

  if (Block >= F->SharedLength) {
    return BoraxPrimitiveSharedBlockLocationError (Interp, Block);
  }

  *Count = F->Shared[Block];
  return BORAX_NIL;
}

STATIC BORAX_OBJECT
EFIAPI
BuiltInFunctionConstant (
  IN BORAX_INTERPRETER  *Interp,
  IN BORAX_OBJECT       Function,
  IN UINTN              Index,
  OUT BORAX_OBJECT      *Constant
  )
{
  BORAX_BUILT_IN_FUNCTION  *F = (BORAX_BUILT_IN_FUNCTION  *)BORAX_GET_POINTER (Function);

  if (Index >= F->ConstantsLength) {
    return BoraxPrimitiveConstantLocationError (Interp, Index);
  }

  *Constant = F->Constants[Index];
  return BORAX_NIL;
}

CONST BORAX_FUNCTION_OPS  gBuiltInFunctionOps = {
  .Run      = &BuiltInFunctionRun,
  .Name     = &BuiltInFunctionName,
  .Info     = &BuiltInFunctionInfo,
  .Shared   = &BuiltInFunctionShared,
  .Constant = &BuiltInFunctionConstant,
};

EFI_STATUS
EFIAPI
BoraxMakeConstant (
  IN BORAX_ALLOCATOR  *Alloc,
  IN UINTN            Size,
  OUT BORAX_CONSTANT  **Constant
  )
{
  EFI_STATUS      Status;
  BORAX_CONSTANT  *NewConstant;

  Status = BoraxAllocateObject (
             Alloc,
             sizeof (BORAX_CONSTANT) + Size,
             (BORAX_OBJECT_HEADER **)&NewConstant
             );
  if (EFI_ERROR (Status)) {
    return Status;
  }

  NewConstant->Header.WideTag = BORAX_WIDETAG_CONSTANT;
  NewConstant->Size           = Size;

  *Constant = NewConstant;
  return EFI_SUCCESS;
}

STATIC EFI_STATUS
EFIAPI
CopyConstant (
  IN BORAX_ALLOCATOR       *Alloc,
  IN BORAX_OBJECT_HEADER   *OldObject,
  OUT BORAX_OBJECT_HEADER  **NewObject
  )
{
  BORAX_CONSTANT  *Constant = (BORAX_CONSTANT *)OldObject;

  return BoraxCopyObject (
           Alloc,
           sizeof (BORAX_CONSTANT) + Constant->Size,
           OldObject,
           NewObject
           );
}

CONST BORAX_GC_HOOKS  gConstantGcHooks = {
  .Copy       = &CopyConstant,
  .SubObjects = &BoraxGcHookNoSubObjects,
};
