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
