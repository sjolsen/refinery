#include "Image.h"

#include <Library/BoraxInterpreter.h>
#include <Library/BoraxMemory.h>
#include <Library/BoraxObjectFile.h>
#include <Library/BoraxPrimitive.h>
#include <Library/BoraxSystemAllocator.h>
#include <Library/BundledResource.h>
#include <Library/DebugLib.h>
#include <Library/DevicePathLib.h>
#include <Library/MemoryAllocationLib.h>
#include <Library/UefiLib.h>

STATIC BORAX_ALLOCATOR  gAlloc;

typedef struct {
  BORAX_RECORD    Record;
  BORAX_OBJECT    Name;
} STANDARD_CLASS;

typedef struct {
  BORAX_RECORD    Record;
  BORAX_OBJECT    Name;
  BORAX_OBJECT    Symbols;
} PACKAGE;

typedef struct {
  BORAX_RECORD    Record;
  BORAX_OBJECT    Package;
  BORAX_OBJECT    Name;
  BORAX_OBJECT    Value;
  BORAX_OBJECT    Class;
} SYMBOL;

typedef struct {
  BORAX_RECORD    Record;
  BORAX_OBJECT    ClassPackage;
  BORAX_OBJECT    ClassSimpleVector;
  BORAX_OBJECT    ClassStandardClass;
  BORAX_OBJECT    ClassString;
  BORAX_OBJECT    ClassSymbol;
  BORAX_OBJECT    FormatObjectRecord;
  BORAX_OBJECT    FormatRecursive;
  BORAX_OBJECT    FormatSimpleVector;
  BORAX_OBJECT    FormatStandardClass;
  BORAX_OBJECT    PackageCommonLisp;
  BORAX_OBJECT    PackageInitialImage;
  BORAX_OBJECT    PackageKeyword;
} LISP_CONTEXT;

typedef struct {
  BORAX_CONSTANT    Constant;
  BUFFER            *Buffer;
} BUFFER_HANDLE;

typedef enum {
  CONST_CTX,
  CONST_BUFFER,
} CONSTANT_DESCRIPTOR;

typedef struct {
  CONST CHAR16           *Name;
  UINTN                  Entry;
  BORAX_BUILT_IN_CODE    Code;
  UINTN                  Locals;
  struct {
    UINTN                        Length;
    CONST CONSTANT_DESCRIPTOR    *Values;
  } Constants;
} FUNCTION_DESCRIPTOR;

#define IMAGE_ERROR(_fmt, ...) \
DEBUG ((DEBUG_ERROR, "%a:%d: " _fmt "\n", __func__, __LINE__, ##__VA_ARGS__))

STATIC UINTN
EFIAPI
StringLength (
  IN BORAX_RECORD  *Record
  )
{
  return (Record->Length * sizeof (UINTN)) / sizeof (CHAR16) - Record->LengthAux;
}

STATIC EFI_STATUS
EFIAPI
EarlyStringEqual (
  IN BORAX_OBJECT  Name1,
  IN CONST CHAR16  *Name2,
  OUT BOOLEAN      *Match
  )
{
  EFI_STATUS    Status;
  BORAX_RECORD  *Record;
  UINTN         Chars1, Chars2;
  CONST CHAR16  *Name1p;
  UINTN         I;

  Status = BORAX_GET_WORD_RECORD (Name1, &Record);
  if (EFI_ERROR (Status)) {
    IMAGE_ERROR ("Not a valid string object");
    return Status;
  }

  Chars1 = StringLength (Record);
  Chars2 = StrLen (Name2);
  if (Chars1 != Chars2) {
    *Match = FALSE;
    return EFI_SUCCESS;
  }

  Name1p = (CONST CHAR16 *)Record->Data;
  for (I = 0; I < Chars1; ++I) {
    if (Name1p[I] != Name2[I]) {
      *Match = FALSE;
      return EFI_SUCCESS;
    }
  }

  *Match = TRUE;
  return EFI_SUCCESS;
}

STATIC EFI_STATUS
EFIAPI
EarlyFindPackage (
  IN BORAX_GLOBAL_ENVIRONMENT  *Env,
  IN CONST CHAR16              *Name,
  OUT PACKAGE                  **Package
  )
{
  EFI_STATUS    Status;
  BORAX_OBJECT  List = Env->Packages;

  while (BORAX_DISCRIMINATE (List) == BORAX_DISCRIM_CONS) {
    BORAX_CONS  *Cons = (BORAX_CONS *)BORAX_GET_POINTER (List);
    PACKAGE     *SomePackage;
    BOOLEAN     Match;

    Status = BORAX_GET_OBJECT_RECORD (Cons->Car, &SomePackage);
    if (EFI_ERROR (Status)) {
      IMAGE_ERROR ("Not a valid package object");
      return Status;
    }

    Status = EarlyStringEqual (SomePackage->Name, Name, &Match);
    if (EFI_ERROR (Status)) {
      return Status;
    }

    if (Match) {
      *Package = SomePackage;
      return EFI_SUCCESS;
    }

    List = Cons->Cdr;
  }

  IMAGE_ERROR ("Package not found: %s", Name);
  return EFI_INVALID_PARAMETER;
}

STATIC EFI_STATUS
EFIAPI
EarlyFindSymbol (
  IN PACKAGE       *Package,
  IN CONST CHAR16  *Name,
  OUT SYMBOL       **Symbol
  )
{
  EFI_STATUS    Status;
  BORAX_OBJECT  List = Package->Symbols;

  while (BORAX_DISCRIMINATE (List) == BORAX_DISCRIM_CONS) {
    BORAX_CONS  *Cons = (BORAX_CONS *)BORAX_GET_POINTER (List);
    SYMBOL      *SomeSymbol;
    BOOLEAN     Match;

    Status = BORAX_GET_OBJECT_RECORD (Cons->Car, &SomeSymbol);
    if (EFI_ERROR (Status)) {
      IMAGE_ERROR ("Not a valid symbol object");
      return Status;
    }

    Status = EarlyStringEqual (SomeSymbol->Name, Name, &Match);
    if (EFI_ERROR (Status)) {
      return Status;
    }

    if (Match) {
      *Symbol = SomeSymbol;
      return EFI_SUCCESS;
    }

    List = Cons->Cdr;
  }

  IMAGE_ERROR ("Symbol not found: %s", Name);
  return EFI_INVALID_PARAMETER;
}

STATIC BORAX_OBJECT
EFIAPI
GetClassName (
  IN LISP_CONTEXT  *Ctx,
  IN BORAX_OBJECT  Object
  )
{
  EFI_STATUS      Status;
  STANDARD_CLASS  *Class;

  Status = BORAX_GET_OBJECT_RECORD (Object, &Class);
  if (EFI_ERROR (Status)) {
    IMAGE_ERROR ("Not a valid class object");
    return BORAX_NIL;
  }

  if (!BORAX_EQ (Class->Record.Class, Ctx->ClassStandardClass)) {
    IMAGE_ERROR ("Not an instance of STANDARD-CLASS");
    return BORAX_NIL;
  }

  return Class->Name;
}

STATIC BORAX_OBJECT
EFIAPI
GetPackageName (
  IN LISP_CONTEXT  *Ctx,
  IN BORAX_OBJECT  Object
  )
{
  EFI_STATUS  Status;
  PACKAGE     *Package;

  Status = BORAX_GET_OBJECT_RECORD (Object, &Package);
  if (EFI_ERROR (Status)) {
    IMAGE_ERROR ("Not a valid package object");
    return BORAX_NIL;
  }

  if (!BORAX_EQ (Package->Record.Class, Ctx->ClassPackage)) {
    IMAGE_ERROR ("Not an instance of PACKAGE");
    return BORAX_NIL;
  }

  return Package->Name;
}

STATIC EFI_STATUS
EFIAPI
WriteString (
  IN LISP_CONTEXT  *Ctx,
  IN BUFFER        *Buffer,
  IN BORAX_OBJECT  Object
  )
{
  BORAX_RECORD  *Record;
  UINTN         Length;

  if (BORAX_DISCRIMINATE (Object) != BORAX_DISCRIM_WORD_RECORD) {
    IMAGE_ERROR ("Not a valid string object");
    return EFI_INVALID_PARAMETER;
  }

  Record = (BORAX_RECORD *)BORAX_GET_POINTER (Object);
  if (!BORAX_EQ (Record->VectorClass, Ctx->ClassString)) {
    IMAGE_ERROR ("Not an instance of STRING");
    return EFI_INVALID_PARAMETER;
  }

  Length = StringLength (Record);

  // TODO: Non-printable characters
  return BufferWriteChars (Buffer, (CHAR16 *)Record->Data, Length);
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

BORAX_OBJECT
EFIAPI
SomeErrorTodo (
  IN BORAX_INTERPRETER  *Interp
  )
{
  // TODO: We shouldn't use primitive APIs outside of the interpreter
  return BoraxPrimitiveSimpleCondition (
           Interp,
           BORAX_GLOBAL_CLASS_SIMPLE_ERROR,
           L"This was an EFI status code. FIXME",
           0,
           NULL
           );
}

enum {
  FSV_CONST_CTX,
  FSV_CONST_BUFFER,
  FSV_CONSTS
};

enum {
  FSV_LOCAL_OBJECT,
  FSV_LOCAL_INDEX,
  FSV_LOCALS
};

enum {
  FSV_PC_START,
  FSV_PC_SLOTS,
};

STATIC BORAX_OBJECT
EFIAPI
FormatSimpleVector (
  IN BORAX_TASK  *Task
  )
{
  EFI_STATUS     Status;
  LISP_CONTEXT   *Ctx          = UnsafeConstant (Task, FSV_CONST_CTX);
  BUFFER_HANDLE  *BufferHandle = UnsafeConstant (Task, FSV_CONST_BUFFER);
  BUFFER         *Buffer       = BufferHandle->Buffer;
  BORAX_OBJECT   *Object       = UnsafeLocal (Task, FSV_LOCAL_OBJECT);
  BORAX_OBJECT   *Index        = UnsafeLocal (Task, FSV_LOCAL_INDEX);

  switch (Task->Registers.PC) {
    case FSV_PC_START:
    {
      BORAX_RECORD  *Record;

      if (Task->Registers.VR->Length != 1) {
        IMAGE_ERROR ("Wrong number of arguments");
        return SomeErrorTodo (Task->Interp);
      }

      *Object = Task->Registers.VR->Values[0];

      Status = BORAX_GET_OBJECT_RECORD (*Object, &Record);
      if (EFI_ERROR (Status)) {
        IMAGE_ERROR ("Not a valid simple-vector object");
        return SomeErrorTodo (Task->Interp);
      }

      Status = BufferWrite (Buffer, L"#(");
      if (EFI_ERROR (Status)) {
        return SomeErrorTodo (Task->Interp);
      }

      // Bounce through to the loop
      *Index             = BORAX_MAKE_FIXNUM (0);
      Task->Registers.PC = FSV_PC_SLOTS;
      return BORAX_NIL;
    }

    case FSV_PC_SLOTS:
    {
      BORAX_RECORD  *Record = (BORAX_RECORD *)BORAX_GET_POINTER (*Object);
      UINTN         I       = BORAX_GET_FIXNUM (*Index);

      Status = BoraxResizeMultipleValues (Task->Interp, &Task->Registers.VR, 1);
      if (EFI_ERROR (Status)) {
        return SomeErrorTodo (Task->Interp);
      }

      if (I < Record->Length) {
        BORAX_OBJECT  Value = Record->Slots[I];

        if (I != 0) {
          Status = BufferWriteChar (Buffer, L' ');
          if (EFI_ERROR (Status)) {
            return SomeErrorTodo (Task->Interp);
          }
        }

        *Index                        = BORAX_MAKE_FIXNUM (I + 1);
        Task->Registers.VR->Values[0] = Value;
        return BoraxTaskEnterFunction (Task, Ctx->FormatRecursive);
      } else {
        Status = BufferWriteChar (Buffer, L')');
        if (EFI_ERROR (Status)) {
          return SomeErrorTodo (Task->Interp);
        }

        Task->Registers.VR->Values[0] = *Object;
        return BoraxTaskExitFunction (Task);
      }
    }

    default:
      return SomeErrorTodo (Task->Interp);
  }
}

STATIC CONST FUNCTION_DESCRIPTOR  gFormatSimpleVector = {
  .Name      = L"FormatSimpleVector",
  .Entry     = FSV_PC_START,
  .Code      = &FormatSimpleVector,
  .Constants = {
    FSV_CONSTS,
    (CONST CONSTANT_DESCRIPTOR[]) {
      [FSV_CONST_CTX]    = CONST_CTX,
      [FSV_CONST_BUFFER] = CONST_BUFFER,
    },
  },
  .Locals    = FSV_LOCALS,
};

enum {
  FSC_CONST_CTX,
  FSC_CONST_BUFFER,
  FSC_CONSTS
};

enum {
  FSC_LOCALS
};

enum {
  FSC_PC_START,
  FSC_PC_END,
};

STATIC BORAX_OBJECT
EFIAPI
FormatStandardClass (
  IN BORAX_TASK  *Task
  )
{
  EFI_STATUS     Status;
  LISP_CONTEXT   *Ctx          = UnsafeConstant (Task, FSC_CONST_CTX);
  BUFFER_HANDLE  *BufferHandle = UnsafeConstant (Task, FSC_CONST_BUFFER);
  BUFFER         *Buffer       = BufferHandle->Buffer;

  switch (Task->Registers.PC) {
    case FSC_PC_START:
    {
      BORAX_OBJECT    Object;
      STANDARD_CLASS  *Class;

      if (Task->Registers.VR->Length != 1) {
        IMAGE_ERROR ("Wrong number of arguments");
        return SomeErrorTodo (Task->Interp);
      }

      Object = Task->Registers.VR->Values[0];

      Status = BORAX_GET_OBJECT_RECORD (Object, &Class);
      if (EFI_ERROR (Status)) {
        IMAGE_ERROR ("Not a valid class object");
        return SomeErrorTodo (Task->Interp);
      }

      if (!BORAX_EQ (Class->Record.Class, Ctx->ClassStandardClass)) {
        IMAGE_ERROR ("Not an instance of STANDARD-CLASS");
        return SomeErrorTodo (Task->Interp);
      }

      Status = BufferWrite (Buffer, L"<STANDARD-CLASS ");
      if (EFI_ERROR (Status)) {
        return SomeErrorTodo (Task->Interp);
      }

      Task->Registers.VR->Values[0] = Class->Name;
      Task->Registers.PC            = FSC_PC_END;
      return BoraxTaskEnterFunction (Task, Ctx->FormatRecursive);
    }
    case FSC_PC_END:
      Status = BufferWriteChar (Buffer, L'>');
      if (EFI_ERROR (Status)) {
        return SomeErrorTodo (Task->Interp);
      }

      return BoraxTaskExitFunction (Task);

    default:
      return SomeErrorTodo (Task->Interp);
  }
}

STATIC CONST FUNCTION_DESCRIPTOR  gFormatStandardClass = {
  .Name      = L"FormatStandardClass",
  .Entry     = FSC_PC_START,
  .Code      = &FormatStandardClass,
  .Locals    = FSC_LOCALS,
  .Constants = {
    FSC_CONSTS,
    (CONST CONSTANT_DESCRIPTOR[]) {
      [FSC_CONST_CTX]    = CONST_CTX,
      [FSC_CONST_BUFFER] = CONST_BUFFER,
    },
  },
};

enum {
  FOR_CONST_CTX,
  FOR_CONST_BUFFER,
  FOR_CONSTS
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

STATIC BORAX_OBJECT
EFIAPI
FormatObjectRecord (
  IN BORAX_TASK  *Task
  )
{
  EFI_STATUS     Status;
  LISP_CONTEXT   *Ctx          = UnsafeConstant (Task, FOR_CONST_CTX);
  BUFFER_HANDLE  *BufferHandle = UnsafeConstant (Task, FOR_CONST_BUFFER);
  BUFFER         *Buffer       = BufferHandle->Buffer;
  BORAX_OBJECT   *Object       = UnsafeLocal (Task, FOR_LOCAL_OBJECT);
  BORAX_OBJECT   *ClassName    = UnsafeLocal (Task, FOR_LOCAL_CLASS_NAME);
  BORAX_OBJECT   *Index        = UnsafeLocal (Task, FOR_LOCAL_INDEX);

  switch (Task->Registers.PC) {
    case FOR_PC_START:
    {
      BORAX_RECORD  *Record;

      if (Task->Registers.VR->Length != 1) {
        IMAGE_ERROR ("Wrong number of arguments");
        return SomeErrorTodo (Task->Interp);
      }

      *Object = Task->Registers.VR->Values[0];

      Status = BORAX_GET_OBJECT_RECORD (*Object, &Record);
      if (EFI_ERROR (Status)) {
        IMAGE_ERROR ("Not a valid object record");
        return SomeErrorTodo (Task->Interp);
      }

      *ClassName = GetClassName (Ctx, Record->Class);
      if (BORAX_EQ (*ClassName, BORAX_NIL)) {
        Status = BufferWrite (Buffer, L"<OBJECT-RECORD ");
        if (EFI_ERROR (Status)) {
          return SomeErrorTodo (Task->Interp);
        }

        Task->Registers.VR->Values[0] = Record->Class;
      } else {
        Status = BufferWriteChar (Buffer, L'<');
        if (EFI_ERROR (Status)) {
          return SomeErrorTodo (Task->Interp);
        }

        Task->Registers.VR->Values[0] = *ClassName;
      }

      *Index             = BORAX_MAKE_FIXNUM (0);
      Task->Registers.PC = FOR_PC_SLOTS;
      return BoraxTaskEnterFunction (Task, Ctx->FormatRecursive);
    }

    case FOR_PC_SLOTS:
    {
      BORAX_RECORD  *Record = (BORAX_RECORD *)BORAX_GET_POINTER (*Object);
      UINTN         I       = BORAX_GET_FIXNUM (*Index);

      Status = BoraxResizeMultipleValues (Task->Interp, &Task->Registers.VR, 1);
      if (EFI_ERROR (Status)) {
        return SomeErrorTodo (Task->Interp);
      }

      if (I < Record->Length) {
        BORAX_OBJECT  Value = Record->Slots[I];

        Status = BufferWriteChar (Buffer, L' ');
        if (EFI_ERROR (Status)) {
          return SomeErrorTodo (Task->Interp);
        }

        Status = BufferWriteInt (Buffer, I);
        if (EFI_ERROR (Status)) {
          return SomeErrorTodo (Task->Interp);
        }

        Status = BufferWriteChar (Buffer, L'=');
        if (EFI_ERROR (Status)) {
          return SomeErrorTodo (Task->Interp);
        }

        *Index                        = BORAX_MAKE_FIXNUM (I + 1);
        Task->Registers.VR->Values[0] = Value;
        return BoraxTaskEnterFunction (Task, Ctx->FormatRecursive);
      } else {
        Status = BufferWriteChar (Buffer, L'>');
        if (EFI_ERROR (Status)) {
          return SomeErrorTodo (Task->Interp);
        }

        Task->Registers.VR->Values[0] = *Object;
        return BoraxTaskExitFunction (Task);
      }
    }

    default:
      return SomeErrorTodo (Task->Interp);
  }
}

STATIC CONST FUNCTION_DESCRIPTOR  gFormatObjectRecord = {
  .Name      = L"FormatObjectRecord",
  .Entry     = FOR_PC_START,
  .Code      = &FormatObjectRecord,
  .Locals    = FOR_LOCALS,
  .Constants = {
    FOR_CONSTS,
    (CONST CONSTANT_DESCRIPTOR[]) {
      [FOR_CONST_CTX]    = CONST_CTX,
      [FOR_CONST_BUFFER] = CONST_BUFFER,
    },
  },
};

enum {
  FR_CONST_CTX,
  FR_CONST_BUFFER,
  FR_CONSTS
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

STATIC BORAX_OBJECT
EFIAPI
FormatRecursive (
  IN BORAX_TASK  *Task
  )
{
  EFI_STATUS     Status;
  LISP_CONTEXT   *Ctx          = UnsafeConstant (Task, FR_CONST_CTX);
  BUFFER_HANDLE  *BufferHandle = UnsafeConstant (Task, FR_CONST_BUFFER);
  BUFFER         *Buffer       = BufferHandle->Buffer;
  BORAX_OBJECT   *SavedObject  = UnsafeLocal (Task, FR_LOCAL_OBJECT);
  BORAX_OBJECT   *SavedRest    = UnsafeLocal (Task, FR_LOCAL_REST);

  switch (Task->Registers.PC) {
    case FR_PC_START:
    {
      BORAX_OBJECT  Object;

      if (Task->Registers.VR->Length != 1) {
        IMAGE_ERROR ("Wrong number of arguments");
        return SomeErrorTodo (Task->Interp);
      }

      Object       = Task->Registers.VR->Values[0];
      *SavedObject = Object;

      switch (BORAX_DISCRIMINATE (Object)) {
        case BORAX_DISCRIM_FIXNUM:
          Status = BufferWriteInt (Buffer, BORAX_GET_FIXNUM (Object));
          if (EFI_ERROR (Status)) {
            return SomeErrorTodo (Task->Interp);
          }

          return BoraxTaskExitFunction (Task);

        case BORAX_DISCRIM_UNBOUND:
          Status = BufferWrite (Buffer, L"<UNBOUND>");
          if (EFI_ERROR (Status)) {
            return SomeErrorTodo (Task->Interp);
          }

          return BoraxTaskExitFunction (Task);

        case BORAX_DISCRIM_CHARACTER:
        {
          Status = BufferWrite (Buffer, L"#\\");
          if (EFI_ERROR (Status)) {
            return SomeErrorTodo (Task->Interp);
          }

          // TODO: Non-printable characters
          Status = BufferWriteChar (Buffer, BORAX_GET_CHARACTER (Object));
          if (EFI_ERROR (Status)) {
            return SomeErrorTodo (Task->Interp);
          }

          return BoraxTaskExitFunction (Task);
        }

        case BORAX_DISCRIM_CONS:
        {
          BORAX_CONS  *Cons = (BORAX_CONS *)BORAX_GET_POINTER (Object);

          Status = BufferWriteChar (Buffer, L'(');
          if (EFI_ERROR (Status)) {
            return SomeErrorTodo (Task->Interp);
          }

          Task->Registers.VR->Values[0] = Cons->Car;
          *SavedRest                    = Cons->Cdr;
          Task->Registers.PC            = FR_PC_LIST;
          return BoraxTaskEnterFunction (Task, Ctx->FormatRecursive);
        }

        case BORAX_DISCRIM_WORD_RECORD:
        {
          BORAX_RECORD  *Record = (BORAX_RECORD *)BORAX_GET_POINTER (Object);

          if (BORAX_EQ (Record->Class, Ctx->ClassString)) {
            Status = BufferWriteChar (Buffer, L'"');
            if (EFI_ERROR (Status)) {
              return SomeErrorTodo (Task->Interp);
            }

            Status = WriteString (Ctx, Buffer, Object);
            if (EFI_ERROR (Status)) {
              return SomeErrorTodo (Task->Interp);
            }

            Status = BufferWriteChar (Buffer, L'"');
            if (EFI_ERROR (Status)) {
              return SomeErrorTodo (Task->Interp);
            }
          } else {
            Status = BufferWrite (Buffer, L"<WORD-RECORD>");
            if (EFI_ERROR (Status)) {
              return SomeErrorTodo (Task->Interp);
            }
          }

          return BoraxTaskExitFunction (Task);
        }

        case BORAX_DISCRIM_OBJECT_RECORD:
        {
          BORAX_RECORD  *Record = (BORAX_RECORD *)BORAX_GET_POINTER (Object);

          if (BORAX_EQ (Object, BORAX_NIL)) {
            Status = BufferWrite (Buffer, L"NIL");
            if (EFI_ERROR (Status)) {
              return SomeErrorTodo (Task->Interp);
            }

            return BoraxTaskExitFunction (Task);
          } else if (BORAX_EQ (Record->Class, Ctx->ClassSymbol)) {
            SYMBOL  *Symbol;

            Status = BORAX_GET_OBJECT_RECORD (Object, &Symbol);
            if (EFI_ERROR (Status)) {
              IMAGE_ERROR ("Not a valid symbol object");
              return SomeErrorTodo (Task->Interp);
            }

            if (!BORAX_EQ (Symbol->Package, Ctx->PackageCommonLisp)) {
              if (!BORAX_EQ (Symbol->Package, Ctx->PackageKeyword)) {
                BORAX_OBJECT  PackageName = GetPackageName (Ctx, Symbol->Package);

                Status = WriteString (Ctx, Buffer, PackageName);
                if (EFI_ERROR (Status)) {
                  return SomeErrorTodo (Task->Interp);
                }
              }

              Status = BufferWriteChar (Buffer, L':');
              if (EFI_ERROR (Status)) {
                return SomeErrorTodo (Task->Interp);
              }
            }

            Status = WriteString (Ctx, Buffer, Symbol->Name);
            if (EFI_ERROR (Status)) {
              return SomeErrorTodo (Task->Interp);
            }

            return BoraxTaskExitFunction (Task);
          } else if (BORAX_EQ (Record->Class, Ctx->ClassSimpleVector)) {
            return BoraxTaskEnterFunctionTail (Task, Ctx->FormatSimpleVector);
          } else if (BORAX_EQ (Record->Class, Ctx->ClassStandardClass)) {
            return BoraxTaskEnterFunctionTail (Task, Ctx->FormatStandardClass);
          } else {
            return BoraxTaskEnterFunctionTail (Task, Ctx->FormatObjectRecord);
          }
        }

        case BORAX_DISCRIM_WEAK_POINTER:
          Status = BufferWrite (Buffer, L"<WEAK-POINTER>");
          if (EFI_ERROR (Status)) {
            return SomeErrorTodo (Task->Interp);
          }

          return BoraxTaskExitFunction (Task);

        case BORAX_DISCRIM_PIN:
          Status = BufferWrite (Buffer, L"<PIN>");
          if (EFI_ERROR (Status)) {
            return SomeErrorTodo (Task->Interp);
          }

          return BoraxTaskExitFunction (Task);

        case BORAX_DISCRIM_MOVED:
          Status = BufferWrite (Buffer, L"<MOVED>");
          if (EFI_ERROR (Status)) {
            return SomeErrorTodo (Task->Interp);
          }

          return BoraxTaskExitFunction (Task);

        case BORAX_DISCRIM_UNINITIALIZED:
          Status = BufferWrite (Buffer, L"<UNINITIALIZED>");
          if (EFI_ERROR (Status)) {
            return SomeErrorTodo (Task->Interp);
          }

          return BoraxTaskExitFunction (Task);

        default:
          Status = BufferWrite (Buffer, L"<ILLEGAL>");
          if (EFI_ERROR (Status)) {
            return SomeErrorTodo (Task->Interp);
          }

          return BoraxTaskExitFunction (Task);
      }
    }

    case FR_PC_LIST:
    {
      BORAX_OBJECT  Rest = *SavedRest;

      Status = BoraxResizeMultipleValues (Task->Interp, &Task->Registers.VR, 1);
      if (EFI_ERROR (Status)) {
        return SomeErrorTodo (Task->Interp);
      }

      if (BORAX_DISCRIMINATE (Rest) == BORAX_DISCRIM_CONS) {
        BORAX_CONS  *Cons = (BORAX_CONS *)BORAX_GET_POINTER (Rest);

        Status = BufferWriteChar (Buffer, L' ');
        if (EFI_ERROR (Status)) {
          return SomeErrorTodo (Task->Interp);
        }

        Task->Registers.VR->Values[0] = Cons->Car;
        *SavedRest                    = Cons->Cdr;
        return BoraxTaskEnterFunction (Task, Ctx->FormatRecursive);
      } else if (!BORAX_EQ (Rest, BORAX_NIL)) {
        Status = BufferWrite (Buffer, L" . ");
        if (EFI_ERROR (Status)) {
          return SomeErrorTodo (Task->Interp);
        }

        Task->Registers.VR->Values[0] = Rest;
        Task->Registers.PC            = FR_PC_ENDLIST;
        return BoraxTaskEnterFunction (Task, Ctx->FormatRecursive);
      } else {
        Task->Registers.PC = FR_PC_ENDLIST;
        return BORAX_NIL;
      }
    }

    case FR_PC_ENDLIST:
    {
      Status = BufferWriteChar (Buffer, L')');
      if (EFI_ERROR (Status)) {
        return SomeErrorTodo (Task->Interp);
      }

      Status = BoraxResizeMultipleValues (Task->Interp, &Task->Registers.VR, 1);
      if (EFI_ERROR (Status)) {
        return SomeErrorTodo (Task->Interp);
      }

      Task->Registers.VR->Values[0] = *SavedObject;
      return BoraxTaskExitFunction (Task);
    }

    default:
      return SomeErrorTodo (Task->Interp);
  }
}

STATIC CONST FUNCTION_DESCRIPTOR  gFormatRecursive = {
  .Name      = L"FormatRecursive",
  .Entry     = FR_PC_START,
  .Code      = &FormatRecursive,
  .Locals    = FR_LOCALS,
  .Constants = {
    FR_CONSTS,
    (CONST CONSTANT_DESCRIPTOR[]) {
      [FR_CONST_CTX]    = CONST_CTX,
      [FR_CONST_BUFFER] = CONST_BUFFER,
    },
  },
};

STATIC EFI_STATUS
EFIAPI
PrintLabelled (
  IN BORAX_INTERPRETER  *Interp,
  IN BORAX_PIN          *LispContext,
  IN BUFFER             *Buffer,
  IN CONST CHAR16       *SymbolName
  )
{
  EFI_STATUS             Status;
  LISP_CONTEXT           *Ctx;
  PACKAGE                *Package;
  SYMBOL                 *Symbol;
  BORAX_MULTIPLE_VALUES  *Args;
  BORAX_PIN              *IORequests;

  Status = BORAX_GET_OBJECT_RECORD (LispContext->Object, &Ctx);
  if (EFI_ERROR (Status)) {
    return Status;
  }

  Status = BORAX_GET_OBJECT_RECORD (Ctx->PackageInitialImage, &Package);
  if (EFI_ERROR (Status)) {
    return Status;
  }

  Status = EarlyFindSymbol (Package, SymbolName, &Symbol);
  if (EFI_ERROR (Status)) {
    return Status;
  }

  Status = BufferWrite (Buffer, SymbolName);
  if (EFI_ERROR (Status)) {
    return Status;
  }

  Status = BufferWrite (Buffer, L" = ");
  if (EFI_ERROR (Status)) {
    return Status;
  }

  Status = BoraxMakeMultipleValues (Interp, 1, &Args);
  if (EFI_ERROR (Status)) {
    return Status;
  }

  Args->Values[0] = Symbol->Value;

  Status = BoraxInterpreterSpawn (
             Interp,
             NULL,  // Completion
             Ctx->FormatRecursive,
             BORAX_MAKE_POINTER (Args),
             NULL  // Task
             );
  if (EFI_ERROR (Status)) {
    return Status;
  }

  BoraxInterpreterRun (Interp, &IORequests);

  return BufferWriteChar (Buffer, L'\n');
}

STATIC EFI_STATUS
EFIAPI
MakeFunction (
  IN CONST FUNCTION_DESCRIPTOR  *Desc,
  IN LISP_CONTEXT               *Ctx,
  IN BUFFER_HANDLE              *Buffer,
  OUT BORAX_OBJECT              *Function
  )
{
  EFI_STATUS               Status;
  BORAX_BUILT_IN_FUNCTION  *F;
  UINTN                    I;

  Status = BoraxMakeBuiltInFunction (
             &gAlloc,
             Desc->Name,
             BORAX_UNBOUND, // Arglist
             Desc->Entry,
             Desc->Code,
             Desc->Locals,
             0,    // SharedLength
             NULL, // Shared
             Desc->Constants.Length,
             &F
             );
  if (EFI_ERROR (Status)) {
    return Status;
  }

  for (I = 0; I < Desc->Constants.Length; ++I) {
    switch (Desc->Constants.Values[I]) {
      case CONST_CTX:
        F->Constants[I] = BORAX_MAKE_POINTER (Ctx);
        break;
      case CONST_BUFFER:
        F->Constants[I] = BORAX_MAKE_POINTER (Buffer);
        break;
      default:
        return EFI_INVALID_PARAMETER;
    }
  }

  *Function = BORAX_MAKE_POINTER (F);
  return EFI_SUCCESS;
}

STATIC EFI_STATUS
EFIAPI
InitializeEnvironment (
  IN BORAX_INTERPRETER  *Interp,
  IN BUFFER             *Content,
  OUT BORAX_PIN         **LispContext
  )
{
  EFI_STATUS  Status;

  BORAX_GLOBAL_ENVIRONMENT  *Env;

  PACKAGE  *CommonLisp, *InitialImage, *Keyword;
  SYMBOL   *Package, *SimpleVector, *StandardClass, *String, *Symbol;

  LISP_CONTEXT   *Ctx;
  BUFFER_HANDLE  *Buffer;

  Env = Interp->GlobalEnvironment;

  Status = EarlyFindPackage (Env, L"COMMON-LISP", &CommonLisp);
  if (EFI_ERROR (Status)) {
    return Status;
  }

  // TODO: clean this up
  Status = EarlyFindPackage (Env, L"BORAX-RUNTIME", &InitialImage);
  if (EFI_ERROR (Status)) {
    return Status;
  }

  Status = EarlyFindPackage (Env, L"KEYWORD", &Keyword);
  if (EFI_ERROR (Status)) {
    return Status;
  }

  Status = EarlyFindSymbol (CommonLisp, L"PACKAGE", &Package);
  if (EFI_ERROR (Status)) {
    return Status;
  }

  Status = EarlyFindSymbol (CommonLisp, L"STANDARD-CLASS", &StandardClass);
  if (EFI_ERROR (Status)) {
    return Status;
  }

  Status = EarlyFindSymbol (CommonLisp, L"SIMPLE-VECTOR", &SimpleVector);
  if (EFI_ERROR (Status)) {
    return Status;
  }

  Status = EarlyFindSymbol (CommonLisp, L"STRING", &String);
  if (EFI_ERROR (Status)) {
    return Status;
  }

  Status = EarlyFindSymbol (CommonLisp, L"SYMBOL", &Symbol);
  if (EFI_ERROR (Status)) {
    return Status;
  }

  Status = BoraxAllocateRecord (
             &gAlloc,
             BORAX_WIDETAG_OBJECT_RECORD,
             BORAX_UNBOUND, // Class
             BORAX_RECORD_LENGTH (LISP_CONTEXT),
             0, // LengthAux
             BORAX_IMMEDIATE_UNBOUND,
             (BORAX_RECORD **)&Ctx
             );
  if (EFI_ERROR (Status)) {
    return Status;
  }

  Ctx->ClassPackage        = Package->Class;
  Ctx->ClassSimpleVector   = SimpleVector->Class;
  Ctx->ClassStandardClass  = StandardClass->Class;
  Ctx->ClassString         = String->Class;
  Ctx->ClassSymbol         = Symbol->Class;
  Ctx->PackageCommonLisp   = BORAX_MAKE_POINTER (CommonLisp);
  Ctx->PackageInitialImage = BORAX_MAKE_POINTER (InitialImage);
  Ctx->PackageKeyword      = BORAX_MAKE_POINTER (Keyword);

  Status = BoraxMakeConstant (
             &gAlloc,
             BORAX_CONSTANT_SIZE (BUFFER_HANDLE),
             (BORAX_CONSTANT **)&Buffer
             );
  if (EFI_ERROR (Status)) {
    return Status;
  }

  Buffer->Buffer = Content;

  Status = MakeFunction (
             &gFormatSimpleVector,
             Ctx,
             Buffer,
             &Ctx->FormatSimpleVector
             );
  if (EFI_ERROR (Status)) {
    return Status;
  }

  Status = MakeFunction (
             &gFormatStandardClass,
             Ctx,
             Buffer,
             &Ctx->FormatStandardClass
             );
  if (EFI_ERROR (Status)) {
    return Status;
  }

  Status = MakeFunction (
             &gFormatObjectRecord,
             Ctx,
             Buffer,
             &Ctx->FormatObjectRecord
             );
  if (EFI_ERROR (Status)) {
    return Status;
  }

  Status = MakeFunction (
             &gFormatRecursive,
             Ctx,
             Buffer,
             &Ctx->FormatRecursive
             );
  if (EFI_ERROR (Status)) {
    return Status;
  }

  return BoraxAllocatePin (
           &gAlloc,
           BORAX_MAKE_POINTER (Ctx),
           LispContext
           );
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
  BORAX_PIN                 *GlobalEnvironment;
  BORAX_INTERPRETER         *Interp = NULL;
  BORAX_PIN                 *Ctx;

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

  Status = InitializeEnvironment (Interp, Content, &Ctx);
  if (EFI_ERROR (Status)) {
    goto cleanup;
  }

  (VOID)PrintLabelled (Interp, Ctx, Content, L"NUMBERS");
  (VOID)PrintLabelled (Interp, Ctx, Content, L"STUFF");
  (VOID)PrintLabelled (Interp, Ctx, Content, L"LETTERS");
  (VOID)PrintLabelled (Interp, Ctx, Content, L"HELLO");
  (VOID)PrintLabelled (Interp, Ctx, Content, L"SUM-LIST");

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
