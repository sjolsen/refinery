#include <Library/BoraxInterpreter.h>

#include <Library/BaseLib.h>
#include <Library/BoraxMemory.h>
#include <Library/MemoryAllocationLib.h>
#include <Library/UefiBootServicesTableLib.h>

#define GC_PAGE_THRESHOLD_MIN     10
#define GC_PAGE_THRESHOLD_FACTOR  2

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

    if (Task->Result != NULL) {
      // TODO: Return a meaningful value indicating abnormal exit
      Task->Result->Object = BORAX_IMMEDIATE_UNBOUND;
    }

    if (Task->Completion != NULL) {
      gBS->SignalEvent (Task->Completion);
    }

    // TODO: The memory module uses the SystemAllocator abstraction to
    // facilitate unit testing. Should the interpreter do the same?
    FreePool (Task);
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
  EFI_STATUS       Status;
  BORAX_TASK       *Task = NULL;
  BORAX_TASK_DATA  *TaskData;
  BORAX_PIN        *TaskDataPin = NULL;

  if ((Completion == NULL) && (Result != NULL)) {
    return EFI_INVALID_PARAMETER;
  }

  Task = AllocatePool (sizeof (BORAX_TASK));
  if (Task == NULL) {
    Status = EFI_OUT_OF_RESOURCES;
    goto cleanup;
  }

  Status = BoraxAllocateRecord (
             Interp->Alloc,
             BORAX_WIDETAG_OBJECT_RECORD,
             BORAX_IMMEDIATE_UNBOUND, // Class
             BORAX_RECORD_LENGTH (BORAX_TASK_DATA),
             0,                       // LengthAux
             BORAX_IMMEDIATE_UNBOUND, // InitialElement
             (BORAX_RECORD **)&TaskData
             );
  if (EFI_ERROR (Status)) {
    goto cleanup;
  }

  Status = BoraxAllocatePin (
             Interp->Alloc,
             BORAX_MAKE_POINTER (TaskData),
             &TaskDataPin
             );
  if (EFI_ERROR (Status)) {
    goto cleanup;
  }

  Task->State          = BORAX_TASK_RUNNING;
  Task->Completion     = Completion;
  Task->Result         = Result;
  Task->Data           = TaskDataPin;
  TaskData->EntryPoint = EntryPoint;
  TaskData->Args       = Args;

  InsertTailList (&Interp->TaskList, &Task->TaskList);
  Task        = NULL;
  TaskDataPin = NULL;

cleanup:
  if (Task != NULL) {
    FreePool (Task);
  }

  if (TaskDataPin != NULL) {
    BoraxReleasePin (TaskDataPin);
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
  BORAX_BUILT_IN_FUNCTION  *Function = (BORAX_BUILT_IN_FUNCTION *)OldObject;

  return BoraxAllocateBuiltInFunction (
           Alloc,
           Function->Name,
           Function->Arglist,
           Function->Entry,
           Function->Code,
           Function->Constants,
           Function->Locals,
           Function->Shared,
           Function->Closure,
           (BORAX_BUILT_IN_FUNCTION **)NewObject
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
