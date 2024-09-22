#include "Image.h"

#include <Library/BoraxInterpreter.h>
#include <Library/BoraxMemory.h>
#include <Library/BoraxObjectFile.h>
#include <Library/BoraxPlugin.h>
#include <Library/BoraxPrimitive.h>
#include <Library/BoraxRuntime.h>
#include <Library/BoraxSystemAllocator.h>
#include <Library/BundledResource.h>
#include <Library/DebugLib.h>
#include <Library/DevicePathLib.h>
#include <Library/MemoryAllocationLib.h>
#include <Library/UefiLib.h>

STATIC BORAX_ALLOCATOR  gAlloc;

typedef struct {
  BORAX_CONSTANT    Constant;
  BUFFER            *Buffer;
} BUFFER_HANDLE;

#define IMAGE_ERROR(_fmt, ...) \
DEBUG ((DEBUG_ERROR, "%a:%d: " _fmt "\n", __func__, __LINE__, ##__VA_ARGS__))

#define TRY(_expr)  do {                \
  BORAX_OBJECT _TryCondition = (_expr); \
  if (BORAX_BOOL (_TryCondition)) {     \
    return _TryCondition;               \
  }                                     \
} while (0)

enum {
  DATA_BUFFER,
};

BORAX_OBJECT
EFIAPI
SomeErrorTodo (
  IN BORAX_INTERPRETER  *Interp
  )
{
  // TODO: We shouldn't use primitive APIs outside of the interpreter
  return BoraxPrimitiveSimpleCondition (
           Interp,
           Interp->Globals[BORAX_GLOBAL_CLASS_SIMPLE_ERROR],
           BoraxCString (L"This was an EFI status code. FIXME"),
           0,
           NULL
           );
}

STATIC BORAX_OBJECT *
EFIAPI
UnsafeLocal (
  IN BORAX_TASK  *Task,
  UINTN          Index
  )
{
  BORAX_OBJECT  Condition;
  BORAX_OBJECT  *Local;

  Condition = BoraxTaskAccessLocal (Task, Index, &Local);
  ASSERT (!BORAX_BOOL (Condition));
  return Local;
}

STATIC VOID *
EFIAPI
UnsafeConstant (
  IN BORAX_TASK  *Task,
  UINTN          Index
  )
{
  BORAX_OBJECT  Condition;
  BORAX_OBJECT  Object;

  Condition = BoraxTaskReadConstant (Task, Index, &Object);
  ASSERT (!BORAX_BOOL (Condition));
  ASSERT (BORAX_IS_POINTER (Object));

  return BORAX_GET_POINTER (Object);
}

enum {
  WS_CONST_BUFFER,
  WS_CONSTS
};

STATIC BORAX_OBJECT
EFIAPI
WriteString (
  IN BORAX_TASK  *Task
  )
{
  EFI_STATUS     Status;
  BUFFER_HANDLE  *BufferHandle = UnsafeConstant (Task, WS_CONST_BUFFER);
  BUFFER         *Buffer       = BufferHandle->Buffer;
  BORAX_OBJECT   String;
  BORAX_OBJECT   *Args[] = { &String };
  BORAX_STRING   TheString;

  TRY (BoraxTaskBind (Task, ARRAY_SIZE (Args), Args));
  TRY (BoraxPrimitiveStringData (Task->Interp, String, &TheString));

  // TODO: Non-printable characters
  Status = BufferWriteChars (Buffer, TheString.Data, TheString.Length);
  if (EFI_ERROR (Status)) {
    return SomeErrorTodo (Task->Interp);
  }

  return BoraxTaskExitFunction (Task);
}

STATIC CONST BORAX_DESCRIPTOR_FUNCTION  gWriteString = {
  .Name        = {
    .Package = L"BORAX-RUNTIME",
    .Name    = L"WRITE-STRING",
  },
  .Code      = &WriteString,
  .Constants = {
    WS_CONSTS,
    (CONST BORAX_DESCRIPTOR_CONSTANT[]) {
      [WS_CONST_BUFFER] = {
        .Tag   = BORAX_CONST_DATA,
        .Index = DATA_BUFFER,
      },
    },
  },
};

enum {
  WC_CONST_BUFFER,
  WC_CONSTS
};

STATIC BORAX_OBJECT
EFIAPI
WriteCharacter (
  IN BORAX_TASK  *Task
  )
{
  EFI_STATUS     Status;
  BUFFER_HANDLE  *BufferHandle = UnsafeConstant (Task, WC_CONST_BUFFER);
  BUFFER         *Buffer       = BufferHandle->Buffer;
  BORAX_OBJECT   Character;
  BORAX_OBJECT   *Args[] = { &Character };
  CHAR16         Char;

  TRY (BoraxTaskBind (Task, ARRAY_SIZE (Args), Args));
  TRY (BoraxPrimitiveTheCharacter (Task->Interp, Character, &Char));

  // TODO: Non-printable characters
  Status = BufferWriteChar (Buffer, Char);
  if (EFI_ERROR (Status)) {
    return SomeErrorTodo (Task->Interp);
  }

  return BoraxTaskExitFunction (Task);
}

STATIC CONST BORAX_DESCRIPTOR_FUNCTION  gWriteCharacter = {
  .Name        = {
    .Package = L"BORAX-RUNTIME",
    .Name    = L"WRITE-CHARACTER",
  },
  .Code      = &WriteCharacter,
  .Constants = {
    WS_CONSTS,
    (CONST BORAX_DESCRIPTOR_CONSTANT[]) {
      [WS_CONST_BUFFER] = {
        .Tag   = BORAX_CONST_DATA,
        .Index = DATA_BUFFER,
      },
    },
  },
};

enum {
  PF_CONST_BUFFER,
  PF_CONSTS
};

STATIC BORAX_OBJECT
EFIAPI
PrintFixnum (
  IN BORAX_TASK  *Task
  )
{
  EFI_STATUS     Status;
  BUFFER_HANDLE  *BufferHandle = UnsafeConstant (Task, PF_CONST_BUFFER);
  BUFFER         *Buffer       = BufferHandle->Buffer;
  BORAX_OBJECT   Fixnum;
  BORAX_OBJECT   *Args[] = { &Fixnum };
  INTN           Value;

  TRY (BoraxTaskBind (Task, ARRAY_SIZE (Args), Args));
  TRY (BoraxPrimitiveTheFixnum (Task->Interp, Fixnum, &Value));

  // TODO: Non-printable characters
  Status = BufferWriteInt (Buffer, Value);
  if (EFI_ERROR (Status)) {
    return SomeErrorTodo (Task->Interp);
  }

  return BoraxTaskExitFunction (Task);
}

STATIC CONST BORAX_DESCRIPTOR_FUNCTION  gPrintFixnum = {
  .Name        = {
    .Package = L"BORAX-RUNTIME",
    .Name    = L"PRINT-FIXNUM",
  },
  .Code      = &PrintFixnum,
  .Constants = {
    PF_CONSTS,
    (CONST BORAX_DESCRIPTOR_CONSTANT[]) {
      [PF_CONST_BUFFER] = {
        .Tag   = BORAX_CONST_DATA,
        .Index = DATA_BUFFER,
      },
    },
  },
};

enum {
  PB_CONST_BUFFER,
  PB_CONSTS
};

STATIC BORAX_OBJECT
EFIAPI
PrintByte (
  IN BORAX_TASK  *Task
  )
{
  EFI_STATUS     Status;
  BUFFER_HANDLE  *BufferHandle = UnsafeConstant (Task, PB_CONST_BUFFER);
  BUFFER         *Buffer       = BufferHandle->Buffer;
  BORAX_OBJECT   Fixnum;
  BORAX_OBJECT   *Args[] = { &Fixnum };
  INTN           Value;

  TRY (BoraxTaskBind (Task, ARRAY_SIZE (Args), Args));
  TRY (BoraxPrimitiveTheFixnum (Task->Interp, Fixnum, &Value));

  // TODO: Non-printable characters
  Status = BufferWriteHex (Buffer, Value, 2);
  if (EFI_ERROR (Status)) {
    return SomeErrorTodo (Task->Interp);
  }

  return BoraxTaskExitFunction (Task);
}

STATIC CONST BORAX_DESCRIPTOR_FUNCTION  gPrintByte = {
  .Name        = {
    .Package = L"BORAX-RUNTIME",
    .Name    = L"PRINT-BYTE",
  },
  .Code      = &PrintByte,
  .Constants = {
    PF_CONSTS,
    (CONST BORAX_DESCRIPTOR_CONSTANT[]) {
      [PB_CONST_BUFFER] = {
        .Tag   = BORAX_CONST_DATA,
        .Index = DATA_BUFFER,
      },
    },
  },
};

enum {
  EH_CONST_BUFFER,
  EH_CONST_SYMBOL_PRINT_RECURSIVE,
  EH_CONSTS
};

enum {
  EH_LOCAL_CONDITION,
  EH_LOCALS
};

enum {
  EH_PC_START,
  EH_PC_EXIT,
  EH_PC_ENDLIST,
};

STATIC BORAX_OBJECT
EFIAPI
ErrorHandler (
  IN BORAX_TASK  *Task
  )
{
  EFI_STATUS     Status;
  BORAX_OBJECT   Condition;
  BUFFER_HANDLE  *BufferHandle   = UnsafeConstant (Task, EH_CONST_BUFFER);
  BUFFER         *Buffer         = BufferHandle->Buffer;
  BORAX_OBJECT   *SavedCondition = UnsafeLocal (Task, EH_LOCAL_CONDITION);

  switch (Task->Registers.PC) {
    case EH_PC_START:
    {
      BORAX_OBJECT  SymbolPrintRecursive;
      BORAX_OBJECT  *Args[] = { SavedCondition };

      Condition = BoraxTaskBind (Task, ARRAY_SIZE (Args), Args);
      if (BORAX_BOOL (Condition)) {
        return Condition;
      }

      // TODO: Print this to the buffer instead
      BoraxTaskDebugStackTrace (DEBUG_ERROR, Task);

      Status = BufferWrite (
                 Buffer,
                 L"\nTask encountered an error condition:\n  "
                 );
      if (EFI_ERROR (Status)) {
        return SomeErrorTodo (Task->Interp);
      }

      TRY (BoraxTaskReadConstant (Task, EH_CONST_SYMBOL_PRINT_RECURSIVE, &SymbolPrintRecursive));

      // Just let PrintRecursive consume the VR
      //
      // TODO: Prevent infinite recursion
      return BoraxTaskEnterFunction (Task, SymbolPrintRecursive, EH_PC_EXIT);
    }

    case EH_PC_EXIT:
    {
      Status = BufferWrite (Buffer, L"\n  Aborting task.\n");
      if (EFI_ERROR (Status)) {
        return SomeErrorTodo (Task->Interp);
      }

      BoraxTaskAbort (Task, *SavedCondition);
      return BORAX_NIL;
    }

    default:
      return SomeErrorTodo (Task->Interp);
  }
}

STATIC CONST BORAX_DESCRIPTOR_FUNCTION  gErrorHandler = {
  .Name         = {
    .Package = L"BORAX-RUNTIME",
    .Name    = L"ERROR-HANDLER",
  },
  .Entry     = EH_PC_START,
  .Code      = &ErrorHandler,
  .Locals    = EH_LOCALS,
  .Constants = {
    EH_CONSTS,
    (CONST BORAX_DESCRIPTOR_CONSTANT[]) {
      [EH_CONST_BUFFER] = {
        .Tag   = BORAX_CONST_DATA,
        .Index = DATA_BUFFER,
      },
      [EH_CONST_SYMBOL_PRINT_RECURSIVE] = {
        .Tag    = BORAX_CONST_SYMBOL,
        .Symbol = { L"BORAX-RUNTIME",L"PRINT-RECURSIVE"  },
      },
    },
  },
};

STATIC CONST BORAX_DESCRIPTOR_FUNCTION  *CONST  gFunctions[] = {
  &gErrorHandler,
  &gPrintByte,
  &gPrintFixnum,
  &gWriteCharacter,
  &gWriteString,
};

STATIC CONST BORAX_DESCRIPTOR_PLUGIN  gPluginRefinery = {
  .Functions = {
    .Length = ARRAY_SIZE (gFunctions),
    .Values = gFunctions,
  },
};

STATIC BORAX_OBJECT
EFIAPI
InitializeEnvironment (
  IN BORAX_INTERPRETER  *Interp,
  IN BUFFER             *Content
  )
{
  EFI_STATUS     Status;
  BUFFER_HANDLE  *Buffer;

  // TODO: Plugins should have a way of constructing their own data
  TRY (BoraxAddPlugin (Interp, &gPluginCore, NULL));

  Status = BoraxMakeConstant (
             &gAlloc,
             BORAX_CONSTANT_SIZE (BUFFER_HANDLE),
             (BORAX_CONSTANT **)&Buffer
             );
  if (EFI_ERROR (Status)) {
    return BoraxPrimitiveHeapExhausted (Interp);
  }

  Buffer->Buffer = Content;

  {
    BORAX_OBJECT       DataArray[] = {
      [DATA_BUFFER] = BORAX_MAKE_POINTER (Buffer),
    };
    BORAX_PLUGIN_DATA  Data = {
      .Length = ARRAY_SIZE (DataArray),
      .Values = DataArray,
    };

    return BoraxAddPlugin (Interp, &gPluginRefinery, &Data);
  }
}

STATIC BORAX_OBJECT
EFIAPI
CallLisp (
  IN BORAX_INTERPRETER  *Interp,
  IN BORAX_OBJECT       Function,
  IN UINTN              ArgsLength,
  IN BORAX_OBJECT       *Args
  )
{
  EFI_STATUS             Status;
  BORAX_SYMBOL           *ErrorHandler;
  BORAX_MULTIPLE_VALUES  *VR;
  UINTN                  I;
  BORAX_PIN              *IORequests;

  TRY (BoraxIntern (Interp, &gErrorHandler.Name, &ErrorHandler));

  Status = BoraxMakeMultipleValues (Interp, ArgsLength, &VR);
  if (EFI_ERROR (Status)) {
    return BoraxPrimitiveHeapExhausted (Interp);
  }

  for (I = 0; I < ArgsLength; ++I) {
    VR->Values[I] = Args[I];
  }

  Status = BoraxInterpreterSpawn (
             Interp,
             NULL,  // Completion
             ErrorHandler->Function,
             Function,
             BORAX_MAKE_POINTER (VR),
             NULL  // Task
             );
  if (EFI_ERROR (Status)) {
    return BoraxPrimitiveSimpleCondition (
             Interp,
             Interp->Globals[BORAX_GLOBAL_CLASS_SIMPLE_ERROR],
             BoraxCString (L"Failed to spawn task"),
             0,
             NULL
             );
  }

  BoraxInterpreterRun (Interp, &IORequests);

  // TODO: Return error condition(s)?
  return BORAX_NIL;
}

STATIC CONST BORAX_DESCRIPTOR_SYMBOL  gDemo = { L"BORAX-RUNTIME", L"DEMO" };

EFI_STATUS
EFIAPI
ImageLoadContent (
  IN OUT BUFFER  *Content
  )
{
  EFI_STATUS                Status;
  BORAX_OBJECT              Condition;
  EFI_DEVICE_PATH_PROTOCOL  *InitialImageDevicePath = NULL;
  EFI_DEVICE_PATH_PROTOCOL  *Remainder;
  CHAR16                    *InitialImagePath = NULL;
  EFI_FILE_PROTOCOL         *InitialImage     = NULL;
  BORAX_PIN                 *GlobalEnvironment;
  BORAX_INTERPRETER         *Interp = NULL;
  BORAX_SYMBOL              *Demo;

  BoraxAllocatorInit (&gAlloc, &gSystemAllocator);

  Status = BundledResourcePath (L"initial-image.bxo", &InitialImageDevicePath);
  if (EFI_ERROR (Status)) {
    (VOID)BufferWrite (Content, L"Failed to construct device path\n");
    goto cleanup;
  }

  InitialImagePath = ConvertDevicePathToText (
                       InitialImageDevicePath,
                       TRUE,
                       TRUE
                       );
  if (InitialImagePath == NULL) {
    (VOID)BufferWrite (Content, L"Failed to render device path\n");
    Status = EFI_OUT_OF_RESOURCES;
    goto cleanup;
  }

  Remainder = InitialImageDevicePath;
  Status    = EfiOpenFileByDevicePath (
                &Remainder,
                &InitialImage,
                EFI_FILE_MODE_READ,
                0
                );
  if (EFI_ERROR (Status)) {
    (VOID)BufferWrite (Content, L"Failed to open ");
    (VOID)BufferWrite (Content, InitialImagePath);
    (VOID)BufferWrite (Content, L"\n");
    goto cleanup;
  }

  Status = BoraxLoadObjectFile (&gAlloc, InitialImage, &GlobalEnvironment);
  if (EFI_ERROR (Status)) {
    (VOID)BufferWrite (Content, L"Failed to load ");
    (VOID)BufferWrite (Content, InitialImagePath);
    (VOID)BufferWrite (Content, L"\n");
    goto cleanup;
  }

  (VOID)BufferWrite (Content, L"Loaded ");
  (VOID)BufferWrite (Content, InitialImagePath);
  (VOID)BufferWrite (Content, L"\n");

  Status = BoraxInterpreterInit (&gAlloc, GlobalEnvironment->Object, &Interp);
  if (EFI_ERROR (Status)) {
    goto cleanup;
  }

  Condition = InitializeEnvironment (Interp, Content);
  if (BORAX_BOOL (Condition)) {
    // TODO: Figure out _some_ kind of way of printing conditions without the
    // basic Lisp system running
    (VOID)BufferWrite (Content, L"InitializeEnvironment failed\n");
    goto cleanup;
  }

  Condition = BoraxIntern (Interp, &gDemo, &Demo);
  if (BORAX_BOOL (Condition)) {
    (VOID)BufferWrite (Content, L"Intern(Demo) failed\n");
    goto cleanup;
  }

  Condition = CallLisp (Interp, BORAX_MAKE_POINTER (Demo), 0, NULL);
  if (BORAX_BOOL (Condition)) {
    (VOID)BufferWrite (Content, L"CallLisp(Demo) failed\n");
    goto cleanup;
  }

cleanup:
  if (Interp != NULL) {
    BoraxInterpreterCleanup (Interp);
  }

  if (InitialImage != NULL) {
    InitialImage->Close (InitialImage);
  }

  if (InitialImagePath != NULL) {
    FreePool (InitialImagePath);
  }

  if (InitialImageDevicePath != NULL) {
    FreePool (InitialImageDevicePath);
  }

  BoraxAllocatorCleanup (&gAlloc);
  return EFI_SUCCESS;
}
