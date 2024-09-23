#include <Library/BoraxPlugin.h>

// TODO: Either commonize this or remove it
#define TRY(_expr)  do {                \
  BORAX_OBJECT _TryCondition = (_expr); \
  if (BORAX_BOOL (_TryCondition)) {     \
    return _TryCondition;               \
  }                                     \
} while (0)

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

STATIC CONST BORAX_DESCRIPTOR_FUNCTION  gEq = {
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

STATIC CONST BORAX_DESCRIPTOR_FUNCTION  gClassOf = {
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

STATIC CONST BORAX_DESCRIPTOR_FUNCTION  gRecordLength = {
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
  TRY (BoraxTaskCoBind (Task, 1, &BoraxRecordSlots (Record)[Index]));
  return BoraxTaskExitFunction (Task);
}

STATIC CONST BORAX_DESCRIPTOR_FUNCTION  gRecordSlot = {
  .Name         = {
    .Package = L"BORAX-RUNTIME",
    .Name    = L"RECORD-SLOT",
  },
  .Code      = &RecordSlot,
  .Constants = {
    RS_CONSTS,
    (CONST BORAX_DESCRIPTOR_CONSTANT[]) {
      [RS_CONST_SYMBOL_INDEX_ERROR] = {
        .Tag    = BORAX_CONST_SYMBOL,
        .Symbol = { L"BORAX-RUNTIME",L"INDEX-ERROR"  },
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
  BORAX_STRING   String;
  BOOLEAN        Found;
  BORAX_PACKAGE  *Package;
  BORAX_OBJECT   Result;
  BORAX_OBJECT   *Args[] = { &Object };

  // TODO: Accept package objects and string designators
  TRY (BoraxTaskBind (Task, ARRAY_SIZE (Args), Args));
  TRY (BoraxPrimitiveStringData (Task->Interp, Object, &String));
  TRY (
    BoraxPrimitiveFindPackage (
      Task->Interp,
      BoraxConstString (String),
      &Found,
      &Package
      )
    );

  if (Found) {
    Result = BORAX_MAKE_POINTER (Package);
  } else {
    Result = BORAX_NIL;
  }

  TRY (BoraxTaskCoBind (Task, 1, &Result));
  return BoraxTaskExitFunction (Task);
}

STATIC CONST BORAX_DESCRIPTOR_FUNCTION  gFindPackage = {
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

STATIC CONST BORAX_DESCRIPTOR_FUNCTION  gByteVectorLength = {
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

STATIC CONST BORAX_DESCRIPTOR_FUNCTION  gByteVectorRef = {
  .Name         = {
    .Package = L"BORAX-RUNTIME",
    .Name    = L"BYTE-VECTOR-REF",
  },
  .Code      = &ByteVectorRef,
  .Constants = {
    BVR_CONSTS,
    (CONST BORAX_DESCRIPTOR_CONSTANT[]) {
      [BVR_CONST_SYMBOL_INDEX_ERROR] = {
        .Tag    = BORAX_CONST_SYMBOL,
        .Symbol = { L"BORAX-RUNTIME",L"INDEX-ERROR"  },
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

STATIC CONST BORAX_DESCRIPTOR_FUNCTION  gCarCdr = {
  .Name      = {
    .Package = L"BORAX-RUNTIME",
    .Name    = L"CAR-CDR",
  },
  .Code      = &CarCdr,
};

STATIC BORAX_OBJECT
EFIAPI
Minus (
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
        BORAX_GET_FIXNUM (A) - BORAX_GET_FIXNUM (B)
        )
    };
    Condition = BoraxTaskCoBind (Task, ARRAY_SIZE (Args), Args);
    if (BORAX_BOOL (Condition)) {
      return Condition;
    }
  }

  return BoraxTaskExitFunction (Task);
}

STATIC CONST BORAX_DESCRIPTOR_FUNCTION  gMinus = {
  .Name      = {
    .Package = L"COMMON-LISP",
    .Name    = L"-",
  },
  .Code      = &Minus,
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

STATIC CONST BORAX_DESCRIPTOR_FUNCTION  gPlus = {
  .Name      = {
    .Package = L"COMMON-LISP",
    .Name    = L"+",
  },
  .Code      = &Plus,
};

STATIC CONST BORAX_DESCRIPTOR_FUNCTION  *CONST  gFunctions[] = {
  &gByteVectorLength,
  &gByteVectorRef,
  &gCarCdr,
  &gClassOf,
  &gEq,
  &gFindPackage,
  &gMinus,
  &gPlus,
  &gRecordLength,
  &gRecordSlot,
};

CONST BORAX_DESCRIPTOR_PLUGIN  gPluginCore = {
  .Functions = {
    .Length = ARRAY_SIZE (gFunctions),
    .Values = gFunctions,
  },
};
