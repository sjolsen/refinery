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
RecordLength (
  IN BORAX_TASK  *Task
  )
{
  BORAX_OBJECT  Object;
  BORAX_RECORD  *Record;
  BORAX_OBJECT  *Args[] = { &Object };

  TRY (BoraxTaskBind (Task, ARRAY_SIZE (Args), Args));

  if (BORAX_DISCRIMINATE (Object) != BORAX_DISCRIM_OBJECT_RECORD) {
    return BoraxPrimitiveTypeError (
             Task->Interp,
             Object,
             Task->Interp->Globals[BORAX_GLOBAL_CLASS_RECORD_OBJECT]
             );
  }

  Record = (BORAX_RECORD *)BORAX_GET_POINTER (Object);

  {
    BORAX_OBJECT  Args[] = { BORAX_MAKE_FIXNUM (Record->Length) };
    TRY (BoraxTaskCoBind (Task, ARRAY_SIZE (Args), Args));
    return BoraxTaskExitFunction (Task);
  }
}

STATIC CONST FUNCTION_DESCRIPTOR  gRecordLength = {
  .Name      = {
    .Package = L"BORAX-RUNTIME",
    .Name    = L"RECORD-LENGTH",
  },
  .Code      = &RecordLength,
};

enum {
  RS_CONST_SYMBOL_INDEX_ERROR,
  RS_CONSTS
};

STATIC BORAX_OBJECT
EFIAPI
RecordSlot (
  IN BORAX_TASK  *Task
  )
{
  BORAX_OBJECT  Object, IndexObject;
  BORAX_RECORD  *Record;
  INTN          Index;
  BORAX_OBJECT  *Args[] = { &Object, &IndexObject };

  TRY (BoraxTaskBind (Task, ARRAY_SIZE (Args), Args));

  if (BORAX_DISCRIMINATE (Object) != BORAX_DISCRIM_OBJECT_RECORD) {
    return BoraxPrimitiveTypeError (
             Task->Interp,
             Object,
             Task->Interp->Globals[BORAX_GLOBAL_CLASS_RECORD_OBJECT]
             );
  }

  Record = (BORAX_RECORD *)BORAX_GET_POINTER (Object);

  TRY (BoraxPrimitiveTheFixnum (Task->Interp, IndexObject, &Index));

  if ((Index < 0) || (Index >= Record->Length)) {
    BORAX_OBJECT  SymbolIndexError;
    BORAX_OBJECT  Args[] = { IndexObject, BORAX_MAKE_FIXNUM (Record->Length) };
    TRY (BoraxTaskReadConstant (Task, RS_CONST_SYMBOL_INDEX_ERROR, &SymbolIndexError));
    TRY (BoraxTaskCoBind (Task, ARRAY_SIZE (Args), Args));
    // TODO: Implement this error function
    return BoraxTaskEnterFunction (Task, SymbolIndexError, 0);
  }

  // TODO: Handle unbound slots
  TRY (BoraxTaskCoBind (Task, 1, &Record->Slots[Index]));
  return BoraxTaskExitFunction (Task);
}

STATIC CONST FUNCTION_DESCRIPTOR  gRecordSlot = {
  .Name         = {
    .Package = L"BORAX-RUNTIME",
    .Name    = L"RECORD-SLOT",
  },
  .Code      = &RecordSlot,
  .Constants = {
    RS_CONSTS,
    (CONST CONSTANT_DESCRIPTOR[]) {
      [RS_CONST_SYMBOL_INDEX_ERROR] = {
        .Tag    = CONST_SYMBOL,
        .Symbol = { L"BORAX-RUNTIME",L"INDEX-ERROR"    },
      },
    },
  },
};

STATIC BORAX_OBJECT
EFIAPI
FindPackage (
  IN BORAX_TASK  *Task
  )
{
  BORAX_OBJECT   Object;
  UINTN          Length;
  CHAR16         *Data;
  BOOLEAN        Found;
  BORAX_PACKAGE  *Package;
  BORAX_OBJECT   Result;
  BORAX_OBJECT   *Args[] = { &Object };

  // TODO: Accept package objects and string designators
  TRY (BoraxTaskBind (Task, ARRAY_SIZE (Args), Args));
  TRY (BoraxPrimitiveStringData (Task->Interp, Object, &Length, &Data));
  TRY (BoraxPrimitiveFindPackage2 (Task->Interp, Length, Data, &Found, &Package));

  if (Found) {
    Result = BORAX_MAKE_POINTER (Package);
  } else {
    Result = BORAX_NIL;
  }

  TRY (BoraxTaskCoBind (Task, 1, &Result));
  return BoraxTaskExitFunction (Task);
}

STATIC CONST FUNCTION_DESCRIPTOR  gFindPackage = {
  .Name      = {
    .Package = L"COMMON-LISP",
    .Name    = L"FIND-PACKAGE",
  },
  .Code      = &FindPackage,
};

// TODO: Generalize
STATIC BORAX_OBJECT
EFIAPI
ByteVectorLength (
  IN BORAX_TASK  *Task
  )
{
  BORAX_OBJECT  Object;
  BORAX_OBJECT  *Args[] = { &Object };
  UINTN         Length;
  UINT8         *Data;

  TRY (BoraxTaskBind (Task, ARRAY_SIZE (Args), Args));
  TRY (BoraxPrimitiveSimpleVectorU8Data (Task->Interp, Object, &Length, &Data));

  {
    BORAX_OBJECT  Args[] = { BORAX_MAKE_FIXNUM (Length) };
    TRY (BoraxTaskCoBind (Task, ARRAY_SIZE (Args), Args));
    return BoraxTaskExitFunction (Task);
  }
}

STATIC CONST FUNCTION_DESCRIPTOR  gByteVectorLength = {
  .Name      = {
    .Package = L"BORAX-RUNTIME",
    .Name    = L"BYTE-VECTOR-LENGTH",
  },
  .Code      = &ByteVectorLength,
};

enum {
  BVR_CONST_SYMBOL_INDEX_ERROR,
  BVR_CONSTS
};

// TODO: Generalize
STATIC BORAX_OBJECT
EFIAPI
ByteVectorRef (
  IN BORAX_TASK  *Task
  )
{
  BORAX_OBJECT  Object, IndexObject;
  BORAX_OBJECT  *Args[] = { &Object, &IndexObject };
  UINTN         Length;
  UINT8         *Data;
  INTN          Index;

  TRY (BoraxTaskBind (Task, ARRAY_SIZE (Args), Args));
  TRY (BoraxPrimitiveSimpleVectorU8Data (Task->Interp, Object, &Length, &Data));
  TRY (BoraxPrimitiveTheFixnum (Task->Interp, IndexObject, &Index));

  if ((Index < 0) || (Index >= Length)) {
    BORAX_OBJECT  SymbolIndexError;
    BORAX_OBJECT  Args[] = { IndexObject, BORAX_MAKE_FIXNUM (Length) };
    TRY (BoraxTaskReadConstant (Task, BVR_CONST_SYMBOL_INDEX_ERROR, &SymbolIndexError));
    TRY (BoraxTaskCoBind (Task, ARRAY_SIZE (Args), Args));
    // TODO: Implement this error function
    return BoraxTaskEnterFunction (Task, SymbolIndexError, 0);
  }

  {
    BORAX_OBJECT  Args[] = { BORAX_MAKE_FIXNUM (Data[Index]) };
    TRY (BoraxTaskCoBind (Task, ARRAY_SIZE (Args), Args));
    return BoraxTaskExitFunction (Task);
  }
}

STATIC CONST FUNCTION_DESCRIPTOR  gByteVectorRef = {
  .Name         = {
    .Package = L"BORAX-RUNTIME",
    .Name    = L"BYTE-VECTOR-REF",
  },
  .Code      = &ByteVectorRef,
  .Constants = {
    BVR_CONSTS,
    (CONST CONSTANT_DESCRIPTOR[]) {
      [BVR_CONST_SYMBOL_INDEX_ERROR] = {
        .Tag    = CONST_SYMBOL,
        .Symbol = { L"BORAX-RUNTIME",L"INDEX-ERROR"    },
      },
    },
  },
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

STATIC CONST FUNCTION_DESCRIPTOR  gPrintByte = {
  .Name      = {
    .Package = L"BORAX-RUNTIME",
    .Name    = L"PRINT-BYTE",
  },
  .Code      = &PrintByte,
  .Constants = {
    PF_CONSTS,
    (CONST CONSTANT_DESCRIPTOR[]) {
      [PB_CONST_BUFFER] = { CONST_BUFFER },
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
  &gByteVectorLength,
  &gByteVectorRef,
  &gCarCdr,
  &gClassOf,
  &gEq,
  &gErrorHandler,
  &gFindPackage,
  &gRecordLength,
  &gRecordSlot,
  &gPlus,
  &gPrintByte,
  &gPrintFixnum,
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
