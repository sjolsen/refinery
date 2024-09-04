#include <Library/BoraxInterpreter.h>

#include <Library/BaseLib.h>
#include <Library/BoraxMemory.h>
#include <Library/MemoryAllocationLib.h>
#include <Library/UefiBootServicesTableLib.h>

#define GC_PAGE_THRESHOLD_MIN     10
#define GC_PAGE_THRESHOLD_FACTOR  2

typedef struct {
  BORAX_RECORD    Record;
  // TODO
} BORAX_GLOBAL_ENVIRONMENT;

STATIC EFI_STATUS
EFIAPI
ValidateGlobalEnvironment (
  IN BORAX_OBJECT  GlobalEnvironment
  )
{
  EFI_STATUS                Status;
  BORAX_GLOBAL_ENVIRONMENT  *Env;

  Status = BORAX_GET_OBJECT_RECORD (GlobalEnvironment, &Env);
  if (EFI_ERROR (Status)) {
    return Status;
  }

  BoraxLockObject (&Env->Record.Header);

  return EFI_SUCCESS;
}

EFI_STATUS
EFIAPI
BoraxInterpreterInit (
  OUT BORAX_INTERPRETER  *Interp,
  IN BORAX_ALLOCATOR     *Alloc,
  IN BORAX_PIN           *GlobalEnvironment
  )
{
  EFI_STATUS  Status;

  Interp->Alloc = Alloc;

  Status = ValidateGlobalEnvironment (GlobalEnvironment->Object);
  if (EFI_ERROR (Status)) {
    return Status;
  }

  Interp->GlobalEnvironment = GlobalEnvironment;
  Interp->GcPageThreshold   = MAX (
                                GC_PAGE_THRESHOLD_MIN,
                                GC_PAGE_THRESHOLD_FACTOR * Alloc->UsedPages
                                );
  InitializeListHead (&Interp->TaskList);

  return EFI_SUCCESS;
}

typedef enum {
  BORAX_TASK_RUNNING,
  BORAX_TASK_PENDING,
} BORAX_TASK_STATE;

typedef struct {
  LIST_ENTRY          TaskList;
  BORAX_TASK_STATE    State;
  EFI_EVENT           Completion;
  BORAX_PIN           *Result;
  BORAX_PIN           *Data;
} BORAX_TASK;

typedef struct {
  BORAX_RECORD    Record;
  BORAX_OBJECT    EntryPoint;
  BORAX_OBJECT    Args;
} BORAX_TASK_DATA;

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
  IN EFI_EVENT          Completion OPTIONAL,
  IN OUT BORAX_PIN      *Result OPTIONAL,
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

  BoraxLockObject (&TaskData->Record.Header);

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
