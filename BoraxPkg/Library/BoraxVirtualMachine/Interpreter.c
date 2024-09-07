#include <Library/BoraxInterpreter.h>

#include <Library/BaseLib.h>
#include <Library/BoraxMemory.h>
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

  if (Pages > STACK_PAGE_MAX) {
    DEBUG ((DEBUG_ERROR, "Stack depth limit exceeded\n"));
    return EFI_OUT_OF_RESOURCES;
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
      DebugPrint (DEBUG_ERROR, "Task returned error: %r\n", Status);
      BoraxTaskDebugStackTrace (DEBUG_ERROR, Task);
      return Status;
    }

    // TODO: Only when crossing the threshold
    BoraxAllocatorCollect (Task->Interp->Alloc);
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
    Task->Result->Object = BORAX_MAKE_POINTER (Task->Registers.VR);
  }

  if (Task->Completion != NULL) {
    gBS->SignalEvent (Task->Completion);
  }

  TaskStackCleanup (&Task->Stack);
  RemoveEntryList (&Task->TaskList);
  BoraxReleasePinRecord (&Task->Record);
}

VOID
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

  if (BORAX_DISCRIMINATE (Args) != BORAX_DISCRIM_MULTIPLE_VALUES) {
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

  Task->Interp     = Interp;
  Task->State      = BORAX_TASK_RUNNING;
  Task->Completion = Completion;
  Task->Result     = Result;

  Task->Registers.BP = 0;
  Task->Registers.SP = 0;
  Task->Registers.PC = 0;
  Task->Registers.VR = (BORAX_MULTIPLE_VALUES *)BORAX_GET_POINTER (Args);

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

EFI_STATUS
EFIAPI
BoraxGlobalEnvironment (
  IN BORAX_INTERPRETER          *Interp,
  OUT BORAX_GLOBAL_ENVIRONMENT  **Env
  )
{
  return BORAX_GET_OBJECT_RECORD (Interp->GlobalEnvironment->Object, Env);
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

EFI_STATUS
EFIAPI
BoraxResizeMultipleValues (
  IN BORAX_INTERPRETER          *Interp,
  IN OUT BORAX_MULTIPLE_VALUES  **Values,
  IN UINTN                      Length
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

STATIC BORAX_OBJECT *
EFIAPI
BoraxTaskStackIndex (
  IN BORAX_TASK  *Task,
  IN UINTN       Index
  )
{
  UINTN  PageIndex  = Index / BORAX_WORDS_PER_PAGE;
  UINTN  PageOffset = Index % BORAX_WORDS_PER_PAGE;

  ASSERT (PageIndex < Task->Stack.PagesLength);
  return &Task->Stack.Pages[PageIndex][PageOffset];
}

BORAX_OBJECT
EFIAPI
BoraxTaskStackRead (
  IN BORAX_TASK  *Task,
  IN UINTN       Index
  )
{
  return *BoraxTaskStackIndex (Task, Index);
}

VOID
EFIAPI
BoraxTaskStackWrite (
  IN BORAX_TASK    *Task,
  IN UINTN         Index,
  IN BORAX_OBJECT  Value
  )
{
  *BoraxTaskStackIndex (Task, Index) = Value;
}

BORAX_OBJECT *
EFIAPI
BoraxTaskStackLocal (
  IN BORAX_TASK  *Task,
  IN UINTN       Index
  )
{
  return BoraxTaskStackIndex (Task, Task->Registers.BP + 4 + Index);
}

BORAX_OBJECT
EFIAPI
BoraxTaskFunctionConstant (
  IN BORAX_TASK  *Task,
  IN UINTN       Index
  )
{
  BORAX_OBJECT  Function = BoraxTaskStackRead (Task, Task->Registers.BP + 2);

  BORAX_BUILT_IN_FUNCTION  *F;

  // TODO: think a little harder about this API
  ASSERT (BORAX_DISCRIMINATE (Function) == BORAX_DISCRIM_BUILT_IN_FUNCTION);
  F = (BORAX_BUILT_IN_FUNCTION *)BORAX_GET_POINTER (Function);
  ASSERT (Index < F->ConstantsLength);
  return F->Constants[Index];
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
  UINTN                    NewBP, NewSP;
  UINTN                    I;

  // TODO: Non-built-ins
  if (BORAX_DISCRIMINATE (Function) != BORAX_DISCRIM_BUILT_IN_FUNCTION) {
    // TODO: Error reporting
    return EFI_INVALID_PARAMETER;
  }

  F = (BORAX_BUILT_IN_FUNCTION *)BORAX_GET_POINTER (Function);

  // TODO: Shared bindings
  if (F->SharedLength != 0) {
    return EFI_UNSUPPORTED;
  }

  NewBP = Task->Registers.SP;
  NewSP = NewBP + 4 + F->Locals;

  Status = TaskStackEnsureCapacity (&Task->Stack, NewSP);
  if (EFI_ERROR (Status)) {
    return Status;
  }

  BoraxTaskStackWrite (Task, NewBP, BORAX_MAKE_FIXNUM (Task->Registers.BP));
  BoraxTaskStackWrite (Task, NewBP + 1, BORAX_MAKE_FIXNUM (Task->Registers.PC));
  BoraxTaskStackWrite (Task, NewBP + 2, Function);
  BoraxTaskStackWrite (Task, NewBP + 3, BORAX_IMMEDIATE_UNBOUND);

  for (I = 0; I < F->Locals; ++I) {
    BoraxTaskStackWrite (Task, NewBP + 4 + I, BORAX_IMMEDIATE_UNBOUND);
  }

  Task->Registers.SP = NewSP;
  Task->Registers.BP = NewBP;
  Task->Registers.PC = F->Entry;

  return EFI_SUCCESS;
}

STATIC VOID
EFIAPI
BoraxTaskExitFunctionCommon (
  IN BORAX_TASK  *Task
  )
{
  UINTN  NewSP = Task->Registers.BP;

  Task->Registers.SP = NewSP;
  Task->Registers.BP = BORAX_GET_FIXNUM (BoraxTaskStackRead (Task, NewSP));
  Task->Registers.PC = BORAX_GET_FIXNUM (BoraxTaskStackRead (Task, NewSP + 1));

  // TODO: Shrink the stack if appropriate
}

EFI_STATUS
EFIAPI
BoraxTaskEnterFunctionTail (
  IN BORAX_TASK    *Task,
  IN BORAX_OBJECT  Function
  )
{
  BoraxTaskExitFunctionCommon (Task);
  return BoraxTaskEnterFunction (Task, Function);
}

VOID
EFIAPI
BoraxTaskExitFunction (
  IN BORAX_TASK  *Task
  )
{
  BoraxTaskExitFunctionCommon (Task);
  if (Task->Registers.SP == 0) {
    Task->State = BORAX_TASK_EXITED;
  }
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

EFI_STATUS
EFIAPI
BoraxStackFrameNext (
  IN BORAX_STACK_FRAME_ITERATOR  *Iter,
  OUT BOOLEAN                    *Done,
  OUT BORAX_STACK_FRAME          *Frame
  )
{
  BORAX_TASK    *Task = Iter->Task;
  UINTN         BP    = Iter->NextBP;
  UINTN         SP    = Iter->NextSP;
  UINTN         PC    = Iter->NextPC;
  BORAX_OBJECT  Temp;

  if (SP == 0) {
    *Done = TRUE;
    return EFI_SUCCESS;
  }

  if (SP < BP + 4) {
    DebugPrint (DEBUG_ERROR, "Corrupted BP/SP [%u:%u]\n", BP, SP);
    // TODO: Better error codes
    return EFI_VOLUME_CORRUPTED;
  }

  Frame->Code = BoraxTaskStackRead (Task, BP + 2);
  Frame->BP   = BP;
  Frame->SP   = SP;
  Frame->PC   = PC;

  Temp = BoraxTaskStackRead (Task, BP);
  ASSERT (BORAX_IS_FIXNUM (Temp));
  Iter->NextBP = BORAX_GET_FIXNUM (Temp);
  Iter->NextSP = BP;

  Temp = BoraxTaskStackRead (Task, BP + 1);
  ASSERT (BORAX_IS_FIXNUM (Temp));
  Iter->NextPC = BORAX_GET_FIXNUM (Temp);

  *Done = FALSE;
  return EFI_SUCCESS;
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
  BOOLEAN                     Done;

  BORAX_OBJECT  LastCode   = BORAX_IMMEDIATE_UNBOUND;
  UINTN         LastPC     = 0;
  UINTN         Duplicates = 0;
  UINTN         Printed    = 0;
  UINTN         Skipped    = 0;

  while (TRUE) {
    EFI_STATUS  Status = BoraxStackFrameNext (&Iter, &Done, &Frame);
    if (EFI_ERROR (Status) || Done) {
      break;
    }

    if ((Frame.Code == LastCode) && (Frame.PC == LastPC)) {
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
        switch (BORAX_DISCRIMINATE (Frame.Code)) {
          case BORAX_DISCRIM_BUILT_IN_FUNCTION:
          {
            BORAX_BUILT_IN_FUNCTION  *F;
            F = (BORAX_BUILT_IN_FUNCTION *)BORAX_GET_POINTER (Frame.Code);

            DebugPrint (ErrorLevel, "  %s:%u\n", F->Name, Frame.PC);
            break;
          }

          default:
            DebugPrint (ErrorLevel, "  <unknown>:%u\n", Frame.PC);
            break;
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
  EFI_STATUS    Status;
  BORAX_TASK    *Task = (BORAX_TASK *)Object;
  UINTN         I;
  BORAX_OBJECT  VR;

  for (I = 0; I < Task->Registers.SP; ++I) {
    UINTN  PageIndex  = I / BORAX_WORDS_PER_PAGE;
    UINTN  PageOffset = I % BORAX_WORDS_PER_PAGE;

    Status = Callback (Ctx, &Task->Stack.Pages[PageIndex][PageOffset]);
    if (EFI_ERROR (Status)) {
      return Status;
    }
  }

  VR     = BORAX_MAKE_POINTER (Task->Registers.VR);
  Status = Callback (Ctx, &VR);
  ASSERT (BORAX_IS_POINTER (VR));
  Task->Registers.VR = (BORAX_MULTIPLE_VALUES *)BORAX_GET_POINTER (VR);

  return Status;
}

CONST BORAX_GC_HOOKS  gTaskGcHooks = {
  .Copy       = &BoraxGcHookNoCopy,  // pin record
  .SubObjects = &TaskSubObjects,
};

EFI_STATUS
EFIAPI
BoraxMakeBuiltInFunction (
  IN BORAX_ALLOCATOR           *Alloc,
  IN CONST CHAR16              *Name,
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
