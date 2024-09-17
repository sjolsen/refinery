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
  BORAX_CONSTANT    Constant;
  BUFFER            *Buffer;
} BUFFER_HANDLE;

typedef struct {
  CONST CHAR16    *Package;
  CONST CHAR16    *Name;
} SYMBOL_DESCRIPTOR;

typedef struct _CONSTANT_DESCRIPTOR CONSTANT_DESCRIPTOR;

typedef struct {
  UINTN                        Length;
  CONST CONSTANT_DESCRIPTOR    *Values;
} LIST_DESCRIPTOR;

struct _CONSTANT_DESCRIPTOR {
  enum {
    CONST_BUFFER,
    CONST_SYMBOL,
    CONST_KEYWORD,
    CONST_CLASS,
    CONST_LIST,
  } Tag;
  union {
    SYMBOL_DESCRIPTOR    Symbol; // Symbol, Keyword, Class
    LIST_DESCRIPTOR      List;   // List
  };
};

typedef struct {
  SYMBOL_DESCRIPTOR      Name;
  UINTN                  Entry;
  BORAX_BUILT_IN_CODE    Code;
  UINTN                  Locals;
  LIST_DESCRIPTOR        Constants;
} FUNCTION_DESCRIPTOR;

#define IMAGE_ERROR(_fmt, ...) \
DEBUG ((DEBUG_ERROR, "%a:%d: " _fmt "\n", __func__, __LINE__, ##__VA_ARGS__))

#define TRY(_expr)  do {                \
  BORAX_OBJECT _TryCondition = (_expr); \
  if (BORAX_BOOL (_TryCondition)) {     \
    return _TryCondition;               \
  }                                     \
} while (0)

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
           L"This was an EFI status code. FIXME",
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

STATIC BORAX_OBJECT
EFIAPI
Eq (
  IN BORAX_TASK  *Task
  )
{
  BORAX_OBJECT  A, B;
  BORAX_OBJECT  Result;
  BORAX_OBJECT  *Args[] = { &A, &B };

  TRY (BoraxTaskBind (Task, ARRAY_SIZE (Args), Args));
  Result = BORAX_EQ (A, B) ? BORAX_T : BORAX_NIL;
  TRY (BoraxTaskCoBind (Task, 1, &Result));
  return BoraxTaskExitFunction (Task);
}

STATIC CONST FUNCTION_DESCRIPTOR  gEq = {
  .Name      = {
    .Package = L"COMMON-LISP",
    .Name    = L"EQ",
  },
  .Code      = &Eq,
};

enum {
  FIND_CLASS_CONST_OR_CLASS_SYMBOL,
  FIND_CLASS_CONST_CLASS_CLASS_NOT_FOUND_ERROR,
  FIND_CLASS_CONSTS
};

STATIC BORAX_OBJECT
EFIAPI
FindClass (
  IN BORAX_TASK  *Task
  )
{
  BORAX_OBJECT  ClassStandardClass = Task->Interp->Globals[BORAX_GLOBAL_CLASS_STANDARD_CLASS];
  BORAX_OBJECT  ClassSymbol = Task->Interp->Globals[BORAX_GLOBAL_CLASS_SYMBOL];
  BORAX_OBJECT  Object, Class;
  BORAX_RECORD  *Record;

  {
    BORAX_OBJECT  *Args[] = { &Object };
    TRY (BoraxTaskBind (Task, ARRAY_SIZE (Args), Args));
  }

  if (BORAX_DISCRIMINATE (Object) != BORAX_DISCRIM_OBJECT_RECORD) {
    goto type_error;
  }

  Record = (BORAX_RECORD *)BORAX_GET_POINTER (Object);

  if (BORAX_EQ (Record->Class, ClassStandardClass)) {
    Class = Object;
  } else if (BORAX_EQ (Record->Class, ClassSymbol)) {
    EFI_STATUS    Status;
    BORAX_SYMBOL  *Symbol;

    Status = BORAX_GET_OBJECT_RECORD (Object, &Symbol);
    if (EFI_ERROR (Status)) {
      goto type_error;
    }

    // TODO: Apply boundp where appropriate
    if (!BORAX_BOUNDP (Symbol->Class)) {
      BORAX_OBJECT  ClassClassNotFoundError;
      TRY (BoraxTaskReadConstant (Task, FIND_CLASS_CONST_CLASS_CLASS_NOT_FOUND_ERROR, &ClassClassNotFoundError));

      return BoraxPrimitiveCellError (
               Task->Interp,
               ClassClassNotFoundError,
               BORAX_MAKE_POINTER (Symbol)
               );
    }

    Class = Symbol->Class;
  } else {
    goto type_error;
  }

  TRY (BoraxTaskCoBind (Task, 1, &Class));
  return BoraxTaskExitFunction (Task);

type_error:
  {
    BORAX_OBJECT  OrClassSymbol;
    TRY (BoraxTaskReadConstant (Task, FIND_CLASS_CONST_OR_CLASS_SYMBOL, &OrClassSymbol));
    return BoraxPrimitiveTypeError (
             Task->Interp,
             Object,
             OrClassSymbol
             );
  }
}

STATIC CONST FUNCTION_DESCRIPTOR  gFindClass = {
  .Name               = {
    .Package = L"COMMON-LISP",
    .Name    = L"FIND-CLASS",
  },
  .Code      = &FindClass,
  .Constants = {
    FIND_CLASS_CONSTS,
    (CONST CONSTANT_DESCRIPTOR[]) {
      [FIND_CLASS_CONST_OR_CLASS_SYMBOL] = {
        .Tag  = CONST_LIST,
        .List = {
          3,
          (CONST CONSTANT_DESCRIPTOR[]) {
            {
              .Tag    = CONST_SYMBOL,
              .Symbol = { L"COMMON-LISP",L"OR"                                                    },
            },
            {
              .Tag    = CONST_SYMBOL,
              .Symbol = { L"COMMON-LISP",L"CLASS"                                                 },
            },
            {
              .Tag    = CONST_SYMBOL,
              .Symbol = { L"COMMON-LISP",L"SYMBOL"                                                },
            },
          },
        },
      },
      [FIND_CLASS_CONST_CLASS_CLASS_NOT_FOUND_ERROR] = {
        .Tag    = CONST_CLASS,
        .Symbol = { L"BORAX-RUNTIME", L"CLASS-NOT-FOUND-ERROR" },
      },
    },
  },
};

STATIC BORAX_OBJECT
EFIAPI
ClassOf (
  IN BORAX_TASK  *Task
  )
{
  BORAX_OBJECT  Object, Class;
  BORAX_OBJECT  *Args[] = { &Object };

  TRY (BoraxTaskBind (Task, ARRAY_SIZE (Args), Args));
  // TODO: Make sure memory safety doesn't depend on ERROR not returning
  TRY (BoraxPrimitiveClassOf (Task->Interp, Object, &Class));
  TRY (BoraxTaskCoBind (Task, 1, &Class));
  return BoraxTaskExitFunction (Task);
}

STATIC CONST FUNCTION_DESCRIPTOR  gClassOf = {
  .Name      = {
    .Package = L"COMMON-LISP",
    .Name    = L"CLASS-OF",
  },
  .Code      = &ClassOf,
};

STATIC BORAX_OBJECT
EFIAPI
ClassName (
  IN BORAX_TASK  *Task
  )
{
  BORAX_OBJECT          Object;
  BORAX_STANDARD_CLASS  *Class;
  BORAX_OBJECT          *Args[] = { &Object };

  TRY (BoraxTaskBind (Task, ARRAY_SIZE (Args), Args));
  TRY (BoraxPrimitiveTheStandardClass (Task->Interp, Object, &Class));
  TRY (BoraxTaskCoBind (Task, 1, &Class->Name));
  return BoraxTaskExitFunction (Task);
}

STATIC CONST FUNCTION_DESCRIPTOR  gClassName = {
  .Name      = {
    .Package = L"COMMON-LISP",
    .Name    = L"CLASS-NAME",
  },
  .Code      = &ClassName,
};

STATIC BORAX_OBJECT
EFIAPI
ClassPrecedenceList (
  IN BORAX_TASK  *Task
  )
{
  BORAX_OBJECT          Object;
  BORAX_STANDARD_CLASS  *Class;
  BORAX_OBJECT          *Args[] = { &Object };

  TRY (BoraxTaskBind (Task, ARRAY_SIZE (Args), Args));
  TRY (BoraxPrimitiveTheStandardClass (Task->Interp, Object, &Class));
  TRY (BoraxTaskCoBind (Task, 1, &Class->PrecedenceList));
  return BoraxTaskExitFunction (Task);
}

STATIC CONST FUNCTION_DESCRIPTOR  gClassPrecedenceList = {
  .Name      = {
    .Package = L"BORAX-RUNTIME",
    .Name    = L"CLASS-PRECEDENCE-LIST",
  },
  .Code      = &ClassPrecedenceList,
};

STATIC BORAX_OBJECT
EFIAPI
SymbolName (
  IN BORAX_TASK  *Task
  )
{
  BORAX_OBJECT  Object;
  BORAX_SYMBOL  *Symbol;
  BORAX_OBJECT  *Args[] = { &Object };

  TRY (BoraxTaskBind (Task, ARRAY_SIZE (Args), Args));
  TRY (BoraxPrimitiveTheSymbol (Task->Interp, Object, &Symbol));
  TRY (BoraxTaskCoBind (Task, 1, &Symbol->Name));
  return BoraxTaskExitFunction (Task);
}

STATIC CONST FUNCTION_DESCRIPTOR  gSymbolName = {
  .Name      = {
    .Package = L"COMMON-LISP",
    .Name    = L"SYMBOL-NAME",
  },
  .Code      = &SymbolName,
};

STATIC BORAX_OBJECT
EFIAPI
SymbolValue (
  IN BORAX_TASK  *Task
  )
{
  BORAX_OBJECT  Object;
  BORAX_SYMBOL  *Symbol;
  BORAX_OBJECT  *Args[] = { &Object };

  TRY (BoraxTaskBind (Task, ARRAY_SIZE (Args), Args));
  TRY (BoraxPrimitiveTheSymbol (Task->Interp, Object, &Symbol));
  TRY (BoraxTaskCoBind (Task, 1, &Symbol->Value));
  return BoraxTaskExitFunction (Task);
}

STATIC CONST FUNCTION_DESCRIPTOR  gSymbolValue = {
  .Name      = {
    .Package = L"COMMON-LISP",
    .Name    = L"SYMBOL-VALUE",
  },
  .Code      = &SymbolValue,
};

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
  UINTN          Length;
  CHAR16         *Data;

  TRY (BoraxTaskBind (Task, ARRAY_SIZE (Args), Args));
  TRY (BoraxPrimitiveStringData (Task->Interp, String, &Length, &Data));

  // TODO: Non-printable characters
  Status = BufferWriteChars (Buffer, Data, Length);
  if (EFI_ERROR (Status)) {
    return SomeErrorTodo (Task->Interp);
  }

  return BoraxTaskExitFunction (Task);
}

STATIC CONST FUNCTION_DESCRIPTOR  gWriteString = {
  .Name      = {
    .Package = L"BORAX-RUNTIME",
    .Name    = L"WRITE-STRING",
  },
  .Code      = &WriteString,
  .Constants = {
    WS_CONSTS,
    (CONST CONSTANT_DESCRIPTOR[]) {
      [WS_CONST_BUFFER] = { CONST_BUFFER },
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

STATIC CONST FUNCTION_DESCRIPTOR  gWriteCharacter = {
  .Name      = {
    .Package = L"BORAX-RUNTIME",
    .Name    = L"WRITE-CHARACTER",
  },
  .Code      = &WriteCharacter,
  .Constants = {
    WS_CONSTS,
    (CONST CONSTANT_DESCRIPTOR[]) {
      [WS_CONST_BUFFER] = { CONST_BUFFER },
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

STATIC CONST FUNCTION_DESCRIPTOR  gPrintFixnum = {
  .Name      = {
    .Package = L"BORAX-RUNTIME",
    .Name    = L"PRINT-FIXNUM",
  },
  .Code      = &PrintFixnum,
  .Constants = {
    PF_CONSTS,
    (CONST CONSTANT_DESCRIPTOR[]) {
      [PF_CONST_BUFFER] = { CONST_BUFFER },
    },
  },
};

enum {
  FSV_CONST_BUFFER,
  FSV_CONST_SYMBOL_PRINT_RECURSIVE,
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
PrintSimpleVector (
  IN BORAX_TASK  *Task
  )
{
  EFI_STATUS     Status;
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
      BORAX_OBJECT  SymbolPrintRecursive;
      UINTN         I = BORAX_GET_FIXNUM (*Index);

      TRY (BoraxTaskReadConstant (Task, FSV_CONST_SYMBOL_PRINT_RECURSIVE, &SymbolPrintRecursive));

      Status = BoraxResizeMultipleValues (Task->Interp, 1, &Task->Registers.VR);
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
        return BoraxTaskEnterFunction (Task, SymbolPrintRecursive);
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

STATIC CONST FUNCTION_DESCRIPTOR  gPrintSimpleVector = {
  .Name         = {
    .Package = L"BORAX-RUNTIME",
    .Name    = L"PRINT-SIMPLE-VECTOR",
  },
  .Entry     = FSV_PC_START,
  .Code      = &PrintSimpleVector,
  .Constants = {
    FSV_CONSTS,
    (CONST CONSTANT_DESCRIPTOR[]) {
      [FSV_CONST_BUFFER]                 = { CONST_BUFFER },
      [FSV_CONST_SYMBOL_PRINT_RECURSIVE] = {
        .Tag    = CONST_SYMBOL,
        .Symbol = { L"BORAX-RUNTIME",L"PRINT-RECURSIVE"    },
      },
    },
  },
  .Locals       = FSV_LOCALS,
};

enum {
  FSC_CONST_BUFFER,
  FSC_CONST_CLASS_STANDARD_CLASS,
  FSC_CONST_SYMBOL_PRINT_RECURSIVE,
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
PrintStandardClass (
  IN BORAX_TASK  *Task
  )
{
  EFI_STATUS     Status;
  BUFFER_HANDLE  *BufferHandle = UnsafeConstant (Task, FSC_CONST_BUFFER);
  BUFFER         *Buffer       = BufferHandle->Buffer;

  switch (Task->Registers.PC) {
    case FSC_PC_START:
    {
      BORAX_OBJECT          ClassStandardClass, SymbolPrintRecursive;
      BORAX_OBJECT          Object;
      BORAX_STANDARD_CLASS  *Class;

      if (Task->Registers.VR->Length != 1) {
        IMAGE_ERROR ("Wrong number of arguments");
        return SomeErrorTodo (Task->Interp);
      }

      Object = Task->Registers.VR->Values[0];

      TRY (BoraxTaskReadConstant (Task, FSC_CONST_CLASS_STANDARD_CLASS, &ClassStandardClass));
      TRY (BoraxTaskReadConstant (Task, FSC_CONST_SYMBOL_PRINT_RECURSIVE, &SymbolPrintRecursive));

      Status = BORAX_GET_OBJECT_RECORD (Object, &Class);
      if (EFI_ERROR (Status)) {
        IMAGE_ERROR ("Not a valid class object");
        return SomeErrorTodo (Task->Interp);
      }

      if (!BORAX_EQ (Class->Record.Class, ClassStandardClass)) {
        IMAGE_ERROR ("Not an instance of STANDARD-CLASS");
        return SomeErrorTodo (Task->Interp);
      }

      Status = BufferWrite (Buffer, L"<STANDARD-CLASS ");
      if (EFI_ERROR (Status)) {
        return SomeErrorTodo (Task->Interp);
      }

      Task->Registers.VR->Values[0] = Class->Name;
      Task->Registers.PC            = FSC_PC_END;
      return BoraxTaskEnterFunction (Task, SymbolPrintRecursive);
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

STATIC CONST FUNCTION_DESCRIPTOR  gPrintStandardClass = {
  .Name         = {
    .Package = L"BORAX-RUNTIME",
    .Name    = L"PRINT-STANDARD-CLASS",
  },
  .Entry     = FSC_PC_START,
  .Code      = &PrintStandardClass,
  .Locals    = FSC_LOCALS,
  .Constants = {
    FSC_CONSTS,
    (CONST CONSTANT_DESCRIPTOR[]) {
      [FSC_CONST_BUFFER]               = { CONST_BUFFER },
      [FSC_CONST_CLASS_STANDARD_CLASS] =  {
        .Tag    = CONST_CLASS,
        .Symbol = { L"COMMON-LISP",L"STANDARD-CLASS"       },
      },
      [FSC_CONST_SYMBOL_PRINT_RECURSIVE] = {
        .Tag    = CONST_SYMBOL,
        .Symbol = { L"BORAX-RUNTIME",L"PRINT-RECURSIVE"    },
      },
    },
  },
};

enum {
  FOR_CONST_BUFFER,
  FOR_CONST_SYMBOL_CLASS_NAME,
  FOR_CONST_SYMBOL_PRINT_RECURSIVE,
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
  FOR_PC_HAVE_CLASS_NAME,
  FOR_PC_SLOTS,
};

STATIC BORAX_OBJECT
EFIAPI
PrintObjectRecord (
  IN BORAX_TASK  *Task
  )
{
  EFI_STATUS     Status;
  BUFFER_HANDLE  *BufferHandle = UnsafeConstant (Task, FOR_CONST_BUFFER);
  BUFFER         *Buffer       = BufferHandle->Buffer;
  BORAX_OBJECT   *Object       = UnsafeLocal (Task, FOR_LOCAL_OBJECT);
  BORAX_OBJECT   *ClassName    = UnsafeLocal (Task, FOR_LOCAL_CLASS_NAME);
  BORAX_OBJECT   *Index        = UnsafeLocal (Task, FOR_LOCAL_INDEX);

  switch (Task->Registers.PC) {
    case FOR_PC_START:
    {
      BORAX_RECORD  *Record;
      BORAX_OBJECT  SymbolClassName;

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

      TRY (BoraxTaskReadConstant (Task, FOR_CONST_SYMBOL_CLASS_NAME, &SymbolClassName));

      {
        BORAX_OBJECT  Args[] = { Record->Class };
        TRY (BoraxTaskCoBind (Task, ARRAY_SIZE (Args), Args));
        Task->Registers.PC = FOR_PC_HAVE_CLASS_NAME;
        return BoraxTaskEnterFunction (Task, SymbolClassName);
      }
    }

    case FOR_PC_HAVE_CLASS_NAME:
    {
      BORAX_RECORD  *Record = (BORAX_RECORD *)BORAX_GET_POINTER (*Object);
      BORAX_OBJECT  SymbolPrintRecursive;

      {
        BORAX_OBJECT  *Args[] = { ClassName };
        TRY (BoraxTaskBind (Task, ARRAY_SIZE (Args), Args));
      }

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

      TRY (BoraxTaskReadConstant (Task, FOR_CONST_SYMBOL_PRINT_RECURSIVE, &SymbolPrintRecursive));

      *Index             = BORAX_MAKE_FIXNUM (0);
      Task->Registers.PC = FOR_PC_SLOTS;
      return BoraxTaskEnterFunction (Task, SymbolPrintRecursive);
    }

    case FOR_PC_SLOTS:
    {
      BORAX_RECORD  *Record = (BORAX_RECORD *)BORAX_GET_POINTER (*Object);
      UINTN         I       = BORAX_GET_FIXNUM (*Index);
      BORAX_OBJECT  SymbolPrintRecursive;

      Status = BoraxResizeMultipleValues (Task->Interp, 1, &Task->Registers.VR);
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

        TRY (BoraxTaskReadConstant (Task, FOR_CONST_SYMBOL_PRINT_RECURSIVE, &SymbolPrintRecursive));

        *Index                        = BORAX_MAKE_FIXNUM (I + 1);
        Task->Registers.VR->Values[0] = Value;
        return BoraxTaskEnterFunction (Task, SymbolPrintRecursive);
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

STATIC CONST FUNCTION_DESCRIPTOR  gPrintObjectRecord = {
  .Name         = {
    .Package = L"BORAX-RUNTIME",
    .Name    = L"PRINT-OBJECT-RECORD",
  },
  .Entry     = FOR_PC_START,
  .Code      = &PrintObjectRecord,
  .Locals    = FOR_LOCALS,
  .Constants = {
    FOR_CONSTS,
    (CONST CONSTANT_DESCRIPTOR[]) {
      [FOR_CONST_BUFFER]            = { CONST_BUFFER },
      [FOR_CONST_SYMBOL_CLASS_NAME] = {
        .Tag    = CONST_SYMBOL,
        .Symbol = { L"COMMON-LISP",L"CLASS-NAME"           },
      },
      [FOR_CONST_SYMBOL_PRINT_RECURSIVE] = {
        .Tag    = CONST_SYMBOL,
        .Symbol = { L"BORAX-RUNTIME",L"PRINT-RECURSIVE"    },
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
      Task->Registers.PC = EH_PC_EXIT;
      return BoraxTaskEnterFunction (Task, SymbolPrintRecursive);
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

STATIC CONST FUNCTION_DESCRIPTOR  gErrorHandler = {
  .Name         = {
    .Package = L"BORAX-RUNTIME",
    .Name    = L"ERROR-HANDLER",
  },
  .Entry     = EH_PC_START,
  .Code      = &ErrorHandler,
  .Locals    = EH_LOCALS,
  .Constants = {
    EH_CONSTS,
    (CONST CONSTANT_DESCRIPTOR[]) {
      [EH_CONST_BUFFER]                 = { CONST_BUFFER },
      [EH_CONST_SYMBOL_PRINT_RECURSIVE] = {
        .Tag    = CONST_SYMBOL,
        .Symbol = { L"BORAX-RUNTIME",L"PRINT-RECURSIVE"    },
      },
    },
  },
};

STATIC BORAX_OBJECT
EFIAPI
CarCdr (
  IN BORAX_TASK  *Task
  )
{
  BORAX_OBJECT  Condition;
  BORAX_OBJECT  Object;
  BORAX_CONS    *Cons;

  {
    BORAX_OBJECT  *Args[] = { &Object };
    Condition = BoraxTaskBind (Task, ARRAY_SIZE (Args), Args);
    if (BORAX_BOOL (Condition)) {
      return Condition;
    }
  }

  if (BORAX_DISCRIMINATE (Object) != BORAX_DISCRIM_CONS) {
    return BoraxPrimitiveTypeError (
             Task->Interp,
             Object,
             Task->Interp->Globals[BORAX_GLOBAL_CLASS_CONS]
             );
  }

  Cons = (BORAX_CONS *)BORAX_GET_POINTER (Object);

  {
    BORAX_OBJECT  Args[] = { Cons->Car, Cons->Cdr };
    Condition = BoraxTaskCoBind (Task, ARRAY_SIZE (Args), Args);
    if (BORAX_BOOL (Condition)) {
      return Condition;
    }
  }

  return BoraxTaskExitFunction (Task);
}

STATIC CONST FUNCTION_DESCRIPTOR  gCarCdr = {
  .Name      = {
    .Package = L"BORAX-RUNTIME",
    .Name    = L"CAR-CDR",
  },
  .Code      = &CarCdr,
};

STATIC BORAX_OBJECT
EFIAPI
Plus (
  IN BORAX_TASK  *Task
  )
{
  BORAX_OBJECT  Condition;
  BORAX_OBJECT  A, B;

  {
    BORAX_OBJECT  *Args[] = { &A, &B };
    Condition = BoraxTaskBind (Task, ARRAY_SIZE (Args), Args);
    if (BORAX_BOOL (Condition)) {
      return Condition;
    }
  }

  if (!BORAX_IS_FIXNUM (A)) {
    return BoraxPrimitiveTypeError (
             Task->Interp,
             A,
             Task->Interp->Globals[BORAX_GLOBAL_CLASS_FIXNUM]
             );
  }

  if (!BORAX_IS_FIXNUM (B)) {
    return BoraxPrimitiveTypeError (
             Task->Interp,
             B,
             Task->Interp->Globals[BORAX_GLOBAL_CLASS_FIXNUM]
             );
  }

  {
    // TODO: arbitrary-precision integers
    BORAX_OBJECT  Args[] = {
      BORAX_MAKE_FIXNUM (
        BORAX_GET_FIXNUM (A) + BORAX_GET_FIXNUM (B)
        )
    };
    Condition = BoraxTaskCoBind (Task, ARRAY_SIZE (Args), Args);
    if (BORAX_BOOL (Condition)) {
      return Condition;
    }
  }

  return BoraxTaskExitFunction (Task);
}

STATIC CONST FUNCTION_DESCRIPTOR  gPlus = {
  .Name      = {
    .Package = L"COMMON-LISP",
    .Name    = L"+",
  },
  .Code      = &Plus,
};

STATIC CONST FUNCTION_DESCRIPTOR  *gFunctions[] = {
  &gCarCdr,
  &gClassName,
  &gClassOf,
  &gClassPrecedenceList,
  &gEq,
  &gErrorHandler,
  &gFindClass,
  &gPlus,
  &gPrintFixnum,
  &gPrintObjectRecord,
  &gPrintSimpleVector,
  &gPrintStandardClass,
  &gSymbolName,
  &gSymbolValue,
  &gWriteCharacter,
  &gWriteString,
};

STATIC BORAX_OBJECT
EFIAPI
RequirePackage (
  IN BORAX_INTERPRETER  *Interp,
  IN CONST CHAR16       *Name,
  OUT   BORAX_PACKAGE   **Package
  )
{
  BOOLEAN       Found;
  BORAX_OBJECT  PackageName;

  TRY (BoraxPrimitiveFindPackage (Interp, Name, &Found, Package));
  if (!Found) {
    TRY (BoraxPrimitiveMakeString (Interp, Name, &PackageName));
    return BoraxPrimitiveSimpleCondition (
             Interp,
             Interp->Globals[BORAX_GLOBAL_CLASS_SIMPLE_ERROR],
             L"Package not found: ~S",
             1,
             &PackageName
             );
  }

  return BORAX_NIL;
}

STATIC BORAX_OBJECT
EFIAPI
Symbolize (
  IN BORAX_INTERPRETER          *Interp,
  IN CONST FUNCTION_DESCRIPTOR  *Desc,
  OUT   BORAX_SYMBOL            **Symbol
  )
{
  BORAX_PACKAGE  *Package;

  TRY (RequirePackage (Interp, Desc->Name.Package, &Package));

  return BoraxPrimitiveIntern (
           Interp,
           Package,
           Desc->Name.Name,
           Symbol
           );
}

STATIC BORAX_OBJECT
EFIAPI
DefineConstant (
  IN BORAX_INTERPRETER          *Interp,
  IN BORAX_OBJECT               TopLevel,
  IN CONST CONSTANT_DESCRIPTOR  *Const,
  IN BUFFER_HANDLE              *Buffer,
  OUT BORAX_OBJECT              *Object
  )
{
  switch (Const->Tag) {
    case CONST_BUFFER:
      *Object = BORAX_MAKE_POINTER (Buffer);
      return BORAX_NIL;

    case CONST_SYMBOL:
    {
      BORAX_PACKAGE  *Package;
      BORAX_SYMBOL   *Symbol;

      TRY (RequirePackage (Interp, Const->Symbol.Package, &Package));
      TRY (BoraxPrimitiveIntern (Interp, Package, Const->Symbol.Name, &Symbol));
      *Object = BORAX_MAKE_POINTER (Symbol);
      return BORAX_NIL;
    }

    case CONST_KEYWORD:
    {
      BORAX_PACKAGE  *Package;
      BORAX_SYMBOL   *Symbol;

      TRY (RequirePackage (Interp, L"KEYWORD", &Package));
      TRY (BoraxPrimitiveIntern (Interp, Package, Const->Symbol.Name, &Symbol));
      *Object = BORAX_MAKE_POINTER (Symbol);
      return BORAX_NIL;
    }

    case CONST_CLASS:
    {
      BORAX_PACKAGE  *Package;
      BORAX_SYMBOL   *Symbol;

      TRY (RequirePackage (Interp, Const->Symbol.Package, &Package));
      TRY (BoraxPrimitiveIntern (Interp, Package, Const->Symbol.Name, &Symbol));
      *Object = Symbol->Class;
      return BORAX_NIL;
    }

    case CONST_LIST:
    {
      EFI_STATUS    Status;
      BORAX_OBJECT  List = BORAX_NIL;
      UINTN         I;

      for (I = 0; I < Const->List.Length; ++I) {
        BORAX_CONS  *Cons;

        Status = BoraxAllocateCons (
                   Interp->Alloc,
                   BORAX_UNBOUND,
                   List,
                   &Cons
                   );
        if (EFI_ERROR (Status)) {
          return BoraxPrimitiveHeapExhausted (Interp);
        }

        // Recursion is fine here; we're only handling static data
        TRY (
          DefineConstant (
            Interp,
            TopLevel,
            &Const->List.Values[I],
            Buffer,
            &Cons->Car
            )
          );

        List = BORAX_MAKE_POINTER (Cons);
      }

      *Object = List;
      return BORAX_NIL;
    }

    default:
      return BoraxPrimitiveSimpleCondition (
               Interp,
               Interp->Globals[BORAX_GLOBAL_CLASS_SIMPLE_ERROR],
               L"Invalid descriptor for ~S",
               1,
               &TopLevel
               );
  }
}

STATIC BORAX_OBJECT
EFIAPI
DefineFunction (
  IN BORAX_INTERPRETER          *Interp,
  IN CONST FUNCTION_DESCRIPTOR  *Desc,
  IN BUFFER_HANDLE              *Buffer
  )
{
  EFI_STATUS               Status;
  BORAX_SYMBOL             *Symbol;
  BORAX_BUILT_IN_FUNCTION  *F;
  UINTN                    I;

  TRY (Symbolize (Interp, Desc, &Symbol));

  Status = BoraxMakeBuiltInFunction (
             Interp->Alloc,
             BORAX_MAKE_POINTER (Symbol),
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
    return BoraxPrimitiveHeapExhausted (Interp);
  }

  for (I = 0; I < Desc->Constants.Length; ++I) {
    TRY (
      DefineConstant (
        Interp,
        BORAX_MAKE_POINTER (Symbol),
        &Desc->Constants.Values[I],
        Buffer,
        &F->Constants[I]
        )
      );
  }

  Symbol->Function = BORAX_MAKE_POINTER (F);
  return BORAX_NIL;
}

STATIC BORAX_OBJECT
EFIAPI
InitializeEnvironment (
  IN BORAX_INTERPRETER  *Interp,
  IN BUFFER             *Content
  )
{
  EFI_STATUS     Status;
  BUFFER_HANDLE  *Buffer;
  UINTN          I;

  Status = BoraxMakeConstant (
             &gAlloc,
             BORAX_CONSTANT_SIZE (BUFFER_HANDLE),
             (BORAX_CONSTANT **)&Buffer
             );
  if (EFI_ERROR (Status)) {
    return BoraxPrimitiveHeapExhausted (Interp);
  }

  Buffer->Buffer = Content;

  for (I = 0; I < ARRAY_SIZE (gFunctions); ++I) {
    TRY (DefineFunction (Interp, gFunctions[I], Buffer));
  }

  return BORAX_NIL;
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

  TRY (Symbolize (Interp, &gErrorHandler, &ErrorHandler));

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
             L"Failed to spawn task",
             0,
             NULL
             );
  }

  BoraxInterpreterRun (Interp, &IORequests);

  // TODO: Return error condition(s)?
  return BORAX_NIL;
}

STATIC BORAX_OBJECT
EFIAPI
ImageDemo (
  IN BORAX_INTERPRETER  *Interp,
  IN BUFFER             *Content
  )
{
  BORAX_PACKAGE  *BoraxRuntime;
  BORAX_SYMBOL   *Demo;

  TRY (InitializeEnvironment (Interp, Content));
  TRY (RequirePackage (Interp, L"BORAX-RUNTIME", &BoraxRuntime));
  TRY (BoraxPrimitiveIntern (Interp, BoraxRuntime, L"DEMO", &Demo));

  return CallLisp (Interp, BORAX_MAKE_POINTER (Demo), 0, NULL);
}

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

  Condition = ImageDemo (Interp, Content);
  if (BORAX_BOOL (Condition)) {
    // TODO: Figure out _some_ kind of way of printing conditions without the
    // basic Lisp system running
    (VOID)BufferWrite (Content, L"ImageDemo failed\n");
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
