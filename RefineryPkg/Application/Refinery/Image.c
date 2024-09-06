#include "Image.h"

#include <Library/BoraxInterpreter.h>
#include <Library/BoraxMemory.h>
#include <Library/BoraxObjectFile.h>
#include <Library/BoraxSystemAllocator.h>
#include <Library/BundledResource.h>
#include <Library/DevicePathLib.h>
#include <Library/MemoryAllocationLib.h>
#include <Library/UefiLib.h>

STATIC BORAX_ALLOCATOR    gAlloc;
STATIC BORAX_INTERPRETER  gInterp;

typedef struct {
  BORAX_GLOBAL_ENVIRONMENT    Env;
  BORAX_OBJECT                Globals;
  BORAX_OBJECT                Classes;
  BORAX_OBJECT                Packages;
  BORAX_OBJECT                Numbers;
  BORAX_OBJECT                Stuff;
  BORAX_OBJECT                Vector;
  BORAX_OBJECT                Hello;
  BORAX_OBJECT                SumList;
} ROOT;

typedef struct {
  BORAX_RECORD    Record;
  BORAX_OBJECT    Nil;
  BORAX_OBJECT    CommonLisp;
  BORAX_OBJECT    Keyword;
} GLOBALS;

typedef struct {
  BORAX_RECORD    Record;
  BORAX_OBJECT    StandardClass;
  BORAX_OBJECT    Package;
  BORAX_OBJECT    Symbol;
  BORAX_OBJECT    SimpleVector;
  BORAX_OBJECT    String;
  BORAX_OBJECT    BytecodeFunction;
} CLASSES;

typedef struct {
  BORAX_RECORD    Record;
  BORAX_OBJECT    Name;
} STANDARD_CLASS;

typedef struct {
  BORAX_RECORD    Record;
  BORAX_OBJECT    Name;
} PACKAGE;

typedef struct {
  BORAX_RECORD    Record;
  BORAX_OBJECT    Package;
  BORAX_OBJECT    Name;
} SYMBOL;

typedef struct {
  BUFFER          *Content;
  GLOBALS         *Globals;
  CLASSES         *Classes;
  BORAX_OBJECT    FormatSimpleVector;
  BORAX_OBJECT    FormatObjectRecord;
  BORAX_OBJECT    FormatStandardClass;
  BORAX_OBJECT    FormatRecursive;
} LISP_CONTEXT;

// TODO: Make this a function constant
STATIC LISP_CONTEXT  gCtx;

STATIC BORAX_OBJECT
EFIAPI
GetClassName (
  IN BORAX_OBJECT  Object
  )
{
  EFI_STATUS      Status;
  STANDARD_CLASS  *Class;

  Status = BORAX_GET_OBJECT_RECORD (Object, &Class);
  if (EFI_ERROR (Status)) {
    return gCtx.Globals->Nil;
  }

  if (Class->Record.Class != gCtx.Classes->StandardClass) {
    return gCtx.Globals->Nil;
  }

  return Class->Name;
}

STATIC BORAX_OBJECT
EFIAPI
GetPackageName (
  IN BORAX_OBJECT  Object
  )
{
  EFI_STATUS  Status;
  PACKAGE     *Package;

  Status = BORAX_GET_OBJECT_RECORD (Object, &Package);
  if (EFI_ERROR (Status)) {
    return gCtx.Globals->Nil;
  }

  if (Package->Record.Class != gCtx.Classes->Package) {
    return gCtx.Globals->Nil;
  }

  return Package->Name;
}

STATIC EFI_STATUS
EFIAPI
WriteString (
  IN BORAX_OBJECT  Object
  )
{
  BORAX_RECORD  *Record;
  UINTN         Length;

  if (BORAX_DISCRIMINATE (Object) != BORAX_DISCRIM_WORD_RECORD) {
    (VOID)BufferWrite (gCtx.Content, L"Not a string\n");
    return EFI_INVALID_PARAMETER;
  }

  Record = (BORAX_RECORD *)BORAX_GET_POINTER (Object);
  if (Record->Class != gCtx.Classes->String) {
    (VOID)BufferWrite (gCtx.Content, L"Not a string\n");
    return EFI_INVALID_PARAMETER;
  }

  Length = (Record->Length * sizeof (UINTN)) / sizeof (CHAR16) - Record->LengthAux;

  // TODO: Non-printable characters
  return BufferWriteChars (gCtx.Content, (CHAR16 *)Record->Data, Length);
}

typedef struct {
  CONST CHAR16           *Name;
  UINTN                  Entry;
  BORAX_BUILT_IN_CODE    Code;
  UINTN                  Locals;
} FUNCTION_DESCRIPTOR;

enum {
  FSV_LOCAL_OBJECT,
  FSV_LOCAL_INDEX,
  FSV_LOCALS
};

enum {
  FSV_PC_START,
  FSV_PC_SLOTS,
};

STATIC EFI_STATUS
EFIAPI
FormatSimpleVector (
  IN BORAX_TASK  *Task
  )
{
  EFI_STATUS    Status;
  BORAX_OBJECT  *Object = BoraxTaskStackLocal (Task, FSV_LOCAL_OBJECT);
  BORAX_OBJECT  *Index  = BoraxTaskStackLocal (Task, FSV_LOCAL_INDEX);

  switch (Task->Registers.PC) {
    case FSV_PC_START:
    {
      BORAX_RECORD  *Record;

      if (Task->Registers.VR->Length != 1) {
        (VOID)BufferWrite (gCtx.Content, L"Wrong number of arguments\n");
        return EFI_INVALID_PARAMETER;
      }

      *Object = Task->Registers.VR->Values[0];

      Status = BORAX_GET_WORD_RECORD (*Object, &Record);
      if (EFI_ERROR (Status)) {
        (VOID)BufferWrite (gCtx.Content, L"Not a word vector\n");
        return EFI_INVALID_PARAMETER;
      }

      Status = BufferWrite (gCtx.Content, L"#(");
      if (EFI_ERROR (Status)) {
        return Status;
      }

      // Bounce through to the loop
      *Index             = BORAX_MAKE_FIXNUM (0);
      Task->Registers.PC = FSV_PC_SLOTS;
      return EFI_SUCCESS;
    }

    case FSV_PC_SLOTS:
    {
      BORAX_RECORD  *Record = (BORAX_RECORD *)BORAX_GET_POINTER (*Object);
      UINTN         I       = BORAX_GET_FIXNUM (*Index);

      Status = BoraxResizeMultipleValues (Task->Interp, &Task->Registers.VR, 1);
      if (EFI_ERROR (Status)) {
        return Status;
      }

      if (I != 0) {
        Status = BufferWriteChar (gCtx.Content, L' ');
        if (EFI_ERROR (Status)) {
          return Status;
        }
      }

      if (I < Record->Length) {
        BORAX_OBJECT  Value = Record->Data[I];

        *Index                        = BORAX_MAKE_FIXNUM (I + 1);
        Task->Registers.VR->Values[0] = Value;
        return BoraxTaskEnterFunction (Task, gCtx.FormatRecursive);
      } else {
        Status = BufferWriteChar (gCtx.Content, L')');
        if (EFI_ERROR (Status)) {
          return Status;
        }

        Task->Registers.VR->Values[0] = *Object;
        BoraxTaskExitFunction (Task);
        return EFI_SUCCESS;
      }
    }

    default:
      return EFI_INVALID_PARAMETER;
  }
}

STATIC CONST FUNCTION_DESCRIPTOR  gFormatSimpleVector = {
  .Name   = L"FormatSimpleVector",
  .Entry  = FSV_PC_START,
  .Code   = &FormatSimpleVector,
  .Locals = FSV_LOCALS,
};

enum {
  FSC_LOCALS
};

enum {
  FSC_PC_START,
  FSC_PC_END,
};

STATIC EFI_STATUS
EFIAPI
FormatStandardClass (
  IN BORAX_TASK  *Task
  )
{
  EFI_STATUS  Status;

  switch (Task->Registers.PC) {
    case FSC_PC_START:
    {
      BORAX_OBJECT    Object;
      STANDARD_CLASS  *Class;

      if (Task->Registers.VR->Length != 1) {
        (VOID)BufferWrite (gCtx.Content, L"Wrong number of arguments\n");
        return EFI_INVALID_PARAMETER;
      }

      Object = Task->Registers.VR->Values[0];

      Status = BORAX_GET_OBJECT_RECORD (Object, &Class);
      if (EFI_ERROR (Status)) {
        (VOID)BufferWrite (gCtx.Content, L"Malformed class object\n");
        return EFI_INVALID_PARAMETER;
      }

      Status = BufferWrite (gCtx.Content, L"<STANDARD-CLASS ");
      if (EFI_ERROR (Status)) {
        return Status;
      }

      Task->Registers.VR->Values[0] = Class->Name;
      Task->Registers.PC            = FSC_PC_END;
      return BoraxTaskEnterFunction (Task, gCtx.FormatRecursive);
    }
    case FSC_PC_END:
      Status = BufferWriteChar (gCtx.Content, L'>');
      if (EFI_ERROR (Status)) {
        return Status;
      }

      BoraxTaskExitFunction (Task);
      return EFI_SUCCESS;

    default:
      return EFI_INVALID_PARAMETER;
  }
}

STATIC CONST FUNCTION_DESCRIPTOR  gFormatStandardClass = {
  .Name   = L"FormatStandardClass",
  .Entry  = FSC_PC_START,
  .Code   = &FormatStandardClass,
  .Locals = FSC_LOCALS,
};

enum {
  FOR_LOCAL_OBJECT,
  FOR_LOCAL_CLASS_NAME,
  FOR_LOCAL_INDEX,
  FOR_LOCALS
};

enum {
  FOR_PC_START,
  FOR_PC_SLOTS,
};

STATIC EFI_STATUS
EFIAPI
FormatObjectRecord (
  IN BORAX_TASK  *Task
  )
{
  EFI_STATUS    Status;
  BORAX_OBJECT  *Object    = BoraxTaskStackLocal (Task, FOR_LOCAL_OBJECT);
  BORAX_OBJECT  *ClassName = BoraxTaskStackLocal (Task, FOR_LOCAL_CLASS_NAME);
  BORAX_OBJECT  *Index     = BoraxTaskStackLocal (Task, FOR_LOCAL_INDEX);

  switch (Task->Registers.PC) {
    case FOR_PC_START:
    {
      BORAX_RECORD  *Record;

      if (Task->Registers.VR->Length != 1) {
        (VOID)BufferWrite (gCtx.Content, L"Wrong number of arguments\n");
        return EFI_INVALID_PARAMETER;
      }

      *Object = Task->Registers.VR->Values[0];

      Status = BORAX_GET_OBJECT_RECORD (*Object, &Record);
      if (EFI_ERROR (Status)) {
        (VOID)BufferWrite (gCtx.Content, L"Not an object vector\n");
        return Status;
      }

      *ClassName = GetClassName (Record->Class);
      if (*ClassName == gCtx.Globals->Nil) {
        Status = BufferWrite (gCtx.Content, L"<OBJECT-RECORD ");
        if (EFI_ERROR (Status)) {
          return Status;
        }

        Task->Registers.VR->Values[0] = Record->Class;
      } else {
        Status = BufferWriteChar (gCtx.Content, L'<');
        if (EFI_ERROR (Status)) {
          return Status;
        }

        Task->Registers.VR->Values[0] = *ClassName;
      }

      *Index             = BORAX_MAKE_FIXNUM (0);
      Task->Registers.PC = FOR_PC_SLOTS;
      return BoraxTaskEnterFunction (Task, gCtx.FormatRecursive);
    }

    case FOR_PC_SLOTS:
    {
      BORAX_RECORD  *Record = (BORAX_RECORD *)BORAX_GET_POINTER (*Object);
      UINTN         I       = BORAX_GET_FIXNUM (*Index);

      Status = BoraxResizeMultipleValues (Task->Interp, &Task->Registers.VR, 1);
      if (EFI_ERROR (Status)) {
        return Status;
      }

      if (I < Record->Length) {
        BORAX_OBJECT  Value = Record->Data[I];

        Status = BufferWriteChar (gCtx.Content, L' ');
        if (EFI_ERROR (Status)) {
          return Status;
        }

        Status = BufferWriteInt (gCtx.Content, I);
        if (EFI_ERROR (Status)) {
          return Status;
        }

        Status = BufferWriteChar (gCtx.Content, L'=');
        if (EFI_ERROR (Status)) {
          return Status;
        }

        *Index                        = BORAX_MAKE_FIXNUM (I + 1);
        Task->Registers.VR->Values[0] = Value;
        return BoraxTaskEnterFunction (Task, gCtx.FormatRecursive);
      } else {
        Status = BufferWriteChar (gCtx.Content, L'>');
        if (EFI_ERROR (Status)) {
          return Status;
        }

        Task->Registers.VR->Values[0] = *Object;
        BoraxTaskExitFunction (Task);
        return EFI_SUCCESS;
      }
    }

    default:
      return EFI_INVALID_PARAMETER;
  }
}

STATIC CONST FUNCTION_DESCRIPTOR  gFormatObjectRecord = {
  .Name   = L"FormatObjectRecord",
  .Entry  = FOR_PC_START,
  .Code   = &FormatObjectRecord,
  .Locals = FOR_LOCALS,
};

enum {
  FR_LOCAL_OBJECT,
  FR_LOCAL_REST,
  FR_LOCALS,
};

enum {
  FR_PC_START,
  FR_PC_LIST,
  FR_PC_ENDLIST,
};

STATIC EFI_STATUS
EFIAPI
FormatRecursive (
  IN BORAX_TASK  *Task
  )
{
  EFI_STATUS    Status;
  BORAX_OBJECT  *SavedObject = BoraxTaskStackLocal (Task, FR_LOCAL_OBJECT);
  BORAX_OBJECT  *SavedRest   = BoraxTaskStackLocal (Task, FR_LOCAL_REST);

  switch (Task->Registers.PC) {
    case FR_PC_START:
    {
      BORAX_OBJECT  Object;

      if (Task->Registers.VR->Length != 1) {
        (VOID)BufferWrite (gCtx.Content, L"Wrong number of arguments\n");
        return EFI_INVALID_PARAMETER;
      }

      Object       = Task->Registers.VR->Values[0];
      *SavedObject = Object;

      switch (BORAX_DISCRIMINATE (Object)) {
        case BORAX_DISCRIM_FIXNUM:
          Status = BufferWriteInt (gCtx.Content, BORAX_GET_FIXNUM (Object));
          if (EFI_ERROR (Status)) {
            return Status;
          }

          BoraxTaskExitFunction (Task);
          return EFI_SUCCESS;

        case BORAX_DISCRIM_UNBOUND:
          Status = BufferWrite (gCtx.Content, L"<UNBOUND>");
          if (EFI_ERROR (Status)) {
            return Status;
          }

          BoraxTaskExitFunction (Task);
          return EFI_SUCCESS;

        case BORAX_DISCRIM_CHARACTER:
        {
          Status = BufferWrite (gCtx.Content, L"#\\");
          if (EFI_ERROR (Status)) {
            return Status;
          }

          // TODO: Non-printable characters
          Status = BufferWriteChar (gCtx.Content, BORAX_GET_CHARACTER (Object));
          if (EFI_ERROR (Status)) {
            return Status;
          }

          BoraxTaskExitFunction (Task);
          return EFI_SUCCESS;
        }

        case BORAX_DISCRIM_CONS:
        {
          BORAX_CONS  *Cons = (BORAX_CONS *)BORAX_GET_POINTER (Object);

          Status = BufferWriteChar (gCtx.Content, L'(');
          if (EFI_ERROR (Status)) {
            return Status;
          }

          Task->Registers.VR->Values[0] = Cons->Car;
          *SavedRest                    = Cons->Cdr;
          Task->Registers.PC            = FR_PC_LIST;
          return BoraxTaskEnterFunction (Task, gCtx.FormatRecursive);
        }

        case BORAX_DISCRIM_WORD_RECORD:
        {
          BORAX_RECORD  *Record = (BORAX_RECORD *)BORAX_GET_POINTER (Object);

          if (Record->Class == gCtx.Classes->String) {
            Status = BufferWriteChar (gCtx.Content, L'"');
            if (EFI_ERROR (Status)) {
              return Status;
            }

            Status = WriteString (Object);
            if (EFI_ERROR (Status)) {
              return Status;
            }

            Status = BufferWriteChar (gCtx.Content, L'"');
            if (EFI_ERROR (Status)) {
              return Status;
            }
          } else {
            Status = BufferWrite (gCtx.Content, L"<WORD-RECORD>");
            if (EFI_ERROR (Status)) {
              return Status;
            }
          }

          BoraxTaskExitFunction (Task);
          return EFI_SUCCESS;
        }

        case BORAX_DISCRIM_OBJECT_RECORD:
        {
          BORAX_RECORD  *Record = (BORAX_RECORD *)BORAX_GET_POINTER (Object);

          if (Object == gCtx.Globals->Nil) {
            return BufferWrite (gCtx.Content, L"NIL");
            if (EFI_ERROR (Status)) {
              return Status;
            }

            BoraxTaskExitFunction (Task);
            return EFI_SUCCESS;
          } else if (Record->Class == gCtx.Classes->Symbol) {
            SYMBOL  *Symbol;

            Status = BORAX_GET_OBJECT_RECORD (Object, &Symbol);
            if (EFI_ERROR (Status)) {
              (VOID)BufferWrite (gCtx.Content, L"Malformed symbol object\n");
              return EFI_INVALID_PARAMETER;
            }

            if (Symbol->Package != gCtx.Globals->CommonLisp) {
              if (Symbol->Package != gCtx.Globals->Keyword) {
                BORAX_OBJECT  PackageName = GetPackageName (Symbol->Package);

                Status = WriteString (PackageName);
                if (EFI_ERROR (Status)) {
                  return Status;
                }
              }

              Status = BufferWriteChar (gCtx.Content, L':');
              if (EFI_ERROR (Status)) {
                return Status;
              }
            }

            Status = WriteString (Symbol->Name);
            if (EFI_ERROR (Status)) {
              return Status;
            }

            BoraxTaskExitFunction (Task);
            return EFI_SUCCESS;
          } else if (Record->Class == gCtx.Classes->SimpleVector) {
            return BoraxTaskEnterFunctionTail (Task, gCtx.FormatSimpleVector);
          } else if (Record->Class == gCtx.Classes->StandardClass) {
            return BoraxTaskEnterFunctionTail (Task, gCtx.FormatStandardClass);
          } else {
            return BoraxTaskEnterFunctionTail (Task, gCtx.FormatObjectRecord);
          }
        }

        case BORAX_DISCRIM_WEAK_POINTER:
          Status = BufferWrite (gCtx.Content, L"<WEAK-POINTER>");
          if (EFI_ERROR (Status)) {
            return Status;
          }

          BoraxTaskExitFunction (Task);
          return EFI_SUCCESS;

        case BORAX_DISCRIM_PIN:
          Status = BufferWrite (gCtx.Content, L"<PIN>");
          if (EFI_ERROR (Status)) {
            return Status;
          }

          BoraxTaskExitFunction (Task);
          return EFI_SUCCESS;

        case BORAX_DISCRIM_MOVED:
          Status = BufferWrite (gCtx.Content, L"<MOVED>");
          if (EFI_ERROR (Status)) {
            return Status;
          }

          BoraxTaskExitFunction (Task);
          return EFI_SUCCESS;

        case BORAX_DISCRIM_UNINITIALIZED:
          Status = BufferWrite (gCtx.Content, L"<UNINITIALIZED>");
          if (EFI_ERROR (Status)) {
            return Status;
          }

          BoraxTaskExitFunction (Task);
          return EFI_SUCCESS;

        default:
          Status = BufferWrite (gCtx.Content, L"<ILLEGAL>");
          if (EFI_ERROR (Status)) {
            return Status;
          }

          BoraxTaskExitFunction (Task);
          return EFI_SUCCESS;
      }
    }

    case FR_PC_LIST:
    {
      BORAX_OBJECT  Rest = *SavedRest;

      Status = BoraxResizeMultipleValues (Task->Interp, &Task->Registers.VR, 1);
      if (EFI_ERROR (Status)) {
        return Status;
      }

      if (BORAX_DISCRIMINATE (Rest) == BORAX_DISCRIM_CONS) {
        BORAX_CONS  *Cons = (BORAX_CONS *)BORAX_GET_POINTER (Rest);

        Status = BufferWriteChar (gCtx.Content, L' ');
        if (EFI_ERROR (Status)) {
          return Status;
        }

        Task->Registers.VR->Values[0] = Cons->Car;
        *SavedRest                    = Cons->Cdr;
        return BoraxTaskEnterFunction (Task, gCtx.FormatRecursive);
      } else if (Rest != gCtx.Globals->Nil) {
        Status = BufferWrite (gCtx.Content, L" . ");
        if (EFI_ERROR (Status)) {
          return Status;
        }

        Task->Registers.VR->Values[0] = Rest;
        Task->Registers.PC            = FR_PC_ENDLIST;
        return BoraxTaskEnterFunction (Task, gCtx.FormatRecursive);
      } else {
        Task->Registers.PC = FR_PC_ENDLIST;
        return EFI_SUCCESS;
      }
    }

    case FR_PC_ENDLIST:
    {
      Status = BufferWriteChar (gCtx.Content, L')');
      if (EFI_ERROR (Status)) {
        return Status;
      }

      Status = BoraxResizeMultipleValues (Task->Interp, &Task->Registers.VR, 1);
      if (EFI_ERROR (Status)) {
        return Status;
      }

      Task->Registers.VR->Values[0] = *SavedObject;
      BoraxTaskExitFunction (Task);
      return EFI_SUCCESS;
    }

    default:
      return EFI_INVALID_PARAMETER;
  }
}

STATIC CONST FUNCTION_DESCRIPTOR  gFormatRecursive = {
  .Name   = L"FormatRecursive",
  .Entry  = FR_PC_START,
  .Code   = &FormatRecursive,
  .Locals = FR_LOCALS,
};

STATIC EFI_STATUS
EFIAPI
PrintLabelled (
  IN CONST CHAR16  *Label,
  IN BORAX_OBJECT  Object
  )
{
  EFI_STATUS             Status;
  BORAX_MULTIPLE_VALUES  *Args;
  BORAX_PIN              *IORequests;

  Status = BufferWrite (gCtx.Content, Label);
  if (EFI_ERROR (Status)) {
    return Status;
  }

  Status = BufferWrite (gCtx.Content, L" = ");
  if (EFI_ERROR (Status)) {
    return Status;
  }

  Status = BoraxMakeMultipleValues (&gInterp, 1, &Args);
  if (EFI_ERROR (Status)) {
    return Status;
  }

  Args->Values[0] = Object;

  Status = BoraxInterpreterSpawn (
             &gInterp,
             NULL,
             NULL,
             gCtx.FormatRecursive,
             BORAX_MAKE_POINTER (Args)
             );
  if (EFI_ERROR (Status)) {
    return Status;
  }

  Status = BoraxInterpreterRun (&gInterp, &IORequests);
  if (EFI_ERROR (Status)) {
    return Status;
  }

  return BufferWriteChar (gCtx.Content, L'\n');
}

STATIC EFI_STATUS
EFIAPI
MakeFunction (
  IN CONST FUNCTION_DESCRIPTOR  *Desc,
  OUT BORAX_OBJECT              *Function
  )
{
  EFI_STATUS               Status;
  BORAX_BUILT_IN_FUNCTION  *F;

  Status = BoraxMakeBuiltInFunction (
             &gAlloc,
             Desc->Name,
             BORAX_IMMEDIATE_UNBOUND, // Arglist
             Desc->Entry,
             Desc->Code,
             Desc->Locals,
             BORAX_IMMEDIATE_UNBOUND, // Shared
             0,                       // ConstantsLength
             &F
             );
  if (EFI_ERROR (Status)) {
    return Status;
  }

  *Function = BORAX_MAKE_POINTER (F);
  return EFI_SUCCESS;
}

STATIC EFI_STATUS
EFIAPI
FillContent (
  IN OUT BUFFER  *Content,
  BORAX_PIN      *RootPin
  )
{
  EFI_STATUS  Status;
  ROOT        *Root;

  gCtx.Content = Content;

  Status = BORAX_GET_OBJECT_RECORD (RootPin->Object, &Root);
  if (EFI_ERROR (Status)) {
    (VOID)BufferWrite (Content, L"Malformed root object\n");
    return EFI_INVALID_PARAMETER;
  }

  Status = BORAX_GET_OBJECT_RECORD (Root->Globals, &gCtx.Globals);
  if (EFI_ERROR (Status)) {
    (VOID)BufferWrite (Content, L"Malformed globals object\n");
    return EFI_INVALID_PARAMETER;
  }

  Status = BORAX_GET_OBJECT_RECORD (Root->Classes, &gCtx.Classes);
  if (EFI_ERROR (Status)) {
    (VOID)BufferWrite (Content, L"Malformed classes object\n");
    return EFI_INVALID_PARAMETER;
  }

  Status = MakeFunction (&gFormatSimpleVector, &gCtx.FormatSimpleVector);
  if (EFI_ERROR (Status)) {
    return Status;
  }

  Status = MakeFunction (&gFormatStandardClass, &gCtx.FormatStandardClass);
  if (EFI_ERROR (Status)) {
    return Status;
  }

  Status = MakeFunction (&gFormatObjectRecord, &gCtx.FormatObjectRecord);
  if (EFI_ERROR (Status)) {
    return Status;
  }

  Status = MakeFunction (&gFormatRecursive, &gCtx.FormatRecursive);
  if (EFI_ERROR (Status)) {
    return Status;
  }

  BoraxInterpreterInit (&gInterp, &gAlloc, RootPin);

  (VOID)PrintLabelled (L"NUMBERS", Root->Numbers);
  (VOID)PrintLabelled (L"STUFF", Root->Stuff);
  (VOID)PrintLabelled (L"VECTOR", Root->Vector);
  (VOID)PrintLabelled (L"HELLO", Root->Hello);
  (VOID)PrintLabelled (L"SUM-LIST", Root->SumList);

  BoraxInterpreterCleanup (&gInterp);

  return EFI_SUCCESS;
}

EFI_STATUS
EFIAPI
ImageLoadContent (
  IN OUT BUFFER  *Content
  )
{
  EFI_STATUS                Status;
  EFI_DEVICE_PATH_PROTOCOL  *InitialImageDevicePath = NULL;
  EFI_DEVICE_PATH_PROTOCOL  *Remainder;
  CHAR16                    *InitialImagePath = NULL;
  EFI_FILE_PROTOCOL         *InitialImage     = NULL;
  BORAX_PIN                 *RootPin;

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

  Status = BoraxLoadObjectFile (&gAlloc, InitialImage, &RootPin);
  if (EFI_ERROR (Status)) {
    (VOID)BufferWrite (Content, L"Failed to load ");
    (VOID)BufferWrite (Content, InitialImagePath);
    (VOID)BufferWrite (Content, L"\n");
    goto cleanup;
  }

  (VOID)BufferWrite (Content, L"Loaded ");
  (VOID)BufferWrite (Content, InitialImagePath);
  (VOID)BufferWrite (Content, L"\n");

  (VOID)FillContent (Content, RootPin);

cleanup:
  if (InitialImage != NULL) {
    InitialImage->Close (InitialImage);
  }

  FreePool (InitialImagePath);
  FreePool (InitialImageDevicePath);
  BoraxAllocatorCleanup (&gAlloc);
  return Status;
}
