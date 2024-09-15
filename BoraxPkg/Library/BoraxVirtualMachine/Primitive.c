#include <Library/BoraxPrimitive.h>

#include <Library/BaseLib.h>
#include <Library/BaseMemoryLib.h>
#include <Library/DebugLib.h>
#include <Library/SafeIntLib.h>

// TODO: Log to a user-defined location
#define PRIMITIVE_ERROR(_fmt, ...) \
DEBUG ((DEBUG_ERROR, "%a:%d: " _fmt "\n", __func__, __LINE__, ##__VA_ARGS__))

STATIC BORAX_OBJECT
EFIAPI
MakeString (
  IN BORAX_INTERPRETER  *Interp,
  IN CONST CHAR16       *CString,
  OUT BORAX_OBJECT      *String
  )
{
  EFI_STATUS    Status;
  BORAX_OBJECT  ClassString  = Interp->Globals[BORAX_GLOBAL_CLASS_STRING];
  UINTN         CharsPerWord = sizeof (UINTN) / sizeof (CHAR16);
  UINTN         CharLength   = StrLen (CString);
  UINTN         WordLength   = (CharLength + CharsPerWord - 1) / CharsPerWord;
  UINTN         LengthAux    = WordLength * CharsPerWord - CharLength;
  BORAX_RECORD  *Record;

  Status = BoraxAllocateRecord (
             Interp->Alloc,
             BORAX_WIDETAG_WORD_RECORD,
             ClassString,
             WordLength,
             LengthAux,
             0,
             &Record
             );
  if (EFI_ERROR (Status)) {
    return BoraxPrimitiveHeapExhausted (Interp);
  }

  CopyMem (Record->Data, CString, CharLength * sizeof (CHAR16));
  *String = BORAX_MAKE_POINTER (Record);
  return BORAX_NIL;
}

STATIC BORAX_OBJECT
EFIAPI
MakeList (
  IN BORAX_INTERPRETER   *Interp,
  IN UINTN               Length,
  IN CONST BORAX_OBJECT  *Items,
  OUT BORAX_OBJECT       *List
  )
{
  EFI_STATUS    Status;
  BORAX_OBJECT  NewList = BORAX_NIL;
  UINTN         I;

  for (I = 0; I < Length; ++I) {
    BORAX_CONS  *NewCons;

    Status = BoraxAllocateCons (
               Interp->Alloc,
               Items[Length - 1 - I],
               NewList,
               &NewCons
               );
    if (EFI_ERROR (Status)) {
      return BoraxPrimitiveHeapExhausted (Interp);
    }

    NewList = BORAX_MAKE_POINTER (NewCons);
  }

  *List = NewList;
  return BORAX_NIL;
}

BORAX_OBJECT
EFIAPI
BoraxPrimitiveSimpleCondition (
  IN BORAX_INTERPRETER   *Interp,
  IN BORAX_OBJECT        Class,
  IN CONST CHAR16        *Control,
  IN UINTN               ArgsLength,
  IN CONST BORAX_OBJECT  *Args
  )
{
  EFI_STATUS              Status;
  BORAX_OBJECT            Condition;
  BORAX_OBJECT            FormatControl;
  BORAX_OBJECT            FormatArguments;
  BORAX_SIMPLE_CONDITION  *SimpleCondition;

  Condition = MakeString (Interp, Control, &FormatControl);
  if (BORAX_BOOL (Condition)) {
    return Condition;
  }

  Condition = MakeList (Interp, ArgsLength, Args, &FormatArguments);
  if (BORAX_BOOL (Condition)) {
    return Condition;
  }

  Status = BoraxAllocateRecord (
             Interp->Alloc,
             BORAX_WIDETAG_OBJECT_RECORD,
             Class,
             BORAX_RECORD_LENGTH (BORAX_SIMPLE_CONDITION),
             0, // LengthAux
             BORAX_IMMEDIATE_UNBOUND,
             (BORAX_RECORD **)&SimpleCondition
             );
  if (EFI_ERROR (Status)) {
    return BoraxPrimitiveHeapExhausted (Interp);
  }

  SimpleCondition->FormatControl   = FormatControl;
  SimpleCondition->FormatArguments = FormatArguments;
  return BORAX_MAKE_POINTER (SimpleCondition);
}

BORAX_OBJECT
EFIAPI
BoraxPrimitiveTypeError (
  IN BORAX_INTERPRETER  *Interp,
  IN BORAX_OBJECT       Datum,
  IN BORAX_OBJECT       ExpectedType
  )
{
  EFI_STATUS        Status;
  BORAX_OBJECT      ClassTypeError = Interp->Globals[BORAX_GLOBAL_CLASS_TYPE_ERROR];
  BORAX_TYPE_ERROR  *TypeError;

  Status = BoraxAllocateRecord (
             Interp->Alloc,
             BORAX_WIDETAG_OBJECT_RECORD,
             ClassTypeError,
             BORAX_RECORD_LENGTH (BORAX_TYPE_ERROR),
             0, // LengthAux
             BORAX_IMMEDIATE_UNBOUND,
             (BORAX_RECORD **)&TypeError
             );
  if (EFI_ERROR (Status)) {
    return BoraxPrimitiveHeapExhausted (Interp);
  }

  TypeError->Datum        = Datum;
  TypeError->ExpectedType = ExpectedType;
  return BORAX_MAKE_POINTER (TypeError);
}

BORAX_OBJECT
EFIAPI
BoraxPrimitiveCellError (
  IN BORAX_INTERPRETER  *Interp,
  IN BORAX_OBJECT       Class,
  IN BORAX_OBJECT       Name
  )
{
  EFI_STATUS        Status;
  BORAX_CELL_ERROR  *CellError;

  Status = BoraxAllocateRecord (
             Interp->Alloc,
             BORAX_WIDETAG_OBJECT_RECORD,
             Class,
             BORAX_RECORD_LENGTH (BORAX_CELL_ERROR),
             0, // LengthAux
             BORAX_IMMEDIATE_UNBOUND,
             (BORAX_RECORD **)&CellError
             );
  if (EFI_ERROR (Status)) {
    return BoraxPrimitiveHeapExhausted (Interp);
  }

  CellError->Name = Name;
  return BORAX_MAKE_POINTER (CellError);
}

STATIC BORAX_OBJECT
EFIAPI
LocationError (
  IN BORAX_INTERPRETER  *Interp,
  IN BORAX_GLOBAL       Keyword,
  IN UINTN              Index
  )
{
  BORAX_OBJECT  Condition;
  BORAX_OBJECT  SymbolKeyword = Interp->Globals[Keyword];
  BORAX_OBJECT  Items[]       = { SymbolKeyword, BORAX_MAKE_FIXNUM (Index) };
  BORAX_OBJECT  Name;

  Condition = MakeList (Interp, ARRAY_SIZE (Items), Items, &Name);
  if (BORAX_BOOL (Condition)) {
    return Condition;
  }

  return BoraxPrimitiveCellError (
           Interp,
           Interp->Globals[BORAX_GLOBAL_CLASS_LOCATION_ERROR],
           Name
           );
}

BORAX_OBJECT
EFIAPI
BoraxPrimitiveLocalLocationError (
  IN BORAX_INTERPRETER  *Interp,
  IN UINTN              Index
  )
{
  return LocationError (Interp, BORAX_GLOBAL_KEYWORD_LOCAL, Index);
}

BORAX_OBJECT
EFIAPI
BoraxPrimitiveSharedBlockLocationError (
  IN BORAX_INTERPRETER  *Interp,
  IN UINTN              Index
  )
{
  return LocationError (Interp, BORAX_GLOBAL_KEYWORD_SHARED, Index);
}

BORAX_OBJECT
EFIAPI
BoraxPrimitiveConstantLocationError (
  IN BORAX_INTERPRETER  *Interp,
  IN UINTN              Index
  )
{
  return LocationError (Interp, BORAX_GLOBAL_KEYWORD_CONSTANT, Index);
}

BORAX_OBJECT
EFIAPI
BoraxPrimitiveStackExhausted (
  IN BORAX_INTERPRETER  *Interp
  )
{
  EFI_STATUS    Status;
  BORAX_OBJECT  ClassStackExhausted = Interp->Globals[BORAX_GLOBAL_CLASS_STACK_EXHAUSTED];
  BORAX_RECORD  *StackExhausted;

  Status = BoraxAllocateRecord (
             Interp->Alloc,
             BORAX_WIDETAG_OBJECT_RECORD,
             ClassStackExhausted,
             0, // Length
             0, // LengthAux
             BORAX_IMMEDIATE_UNBOUND,
             &StackExhausted
             );
  if (EFI_ERROR (Status)) {
    return BoraxPrimitiveHeapExhausted (Interp);
  }

  return BORAX_MAKE_POINTER (StackExhausted);
}

BORAX_OBJECT
EFIAPI
BoraxPrimitiveHeapExhausted (
  IN BORAX_INTERPRETER  *Interp
  )
{
  EFI_STATUS    Status;
  BORAX_OBJECT  ClassHeapExhausted = Interp->Globals[BORAX_GLOBAL_CLASS_HEAP_EXHAUSTED];
  BORAX_RECORD  *HeapExhausted;

  Status = BoraxAllocateRecord (
             Interp->Alloc,
             BORAX_WIDETAG_OBJECT_RECORD,
             ClassHeapExhausted,
             0, // Length
             0, // LengthAux
             BORAX_IMMEDIATE_UNBOUND,
             &HeapExhausted
             );
  if (EFI_ERROR (Status)) {
    // Common Lisp wants distinct condition objects for temporally distinct
    // situations, so ideally we'd return a new condition object every time
    // here. Unfortunately, if we're in this function... there's probably not
    // much memory available for such an instance (it depends on which arena
    // caused the situation). Try anyway and if we can't, just return the class
    // object.
    PRIMITIVE_ERROR ("Failed to make an instance of HEAP-EXHAUSTED");
    return ClassHeapExhausted;
  }

  return BORAX_MAKE_POINTER (HeapExhausted);
}

BORAX_OBJECT
EFIAPI
BoraxPrimitivePackageName (
  IN BORAX_INTERPRETER  *Interp,
  IN BORAX_OBJECT       Package,
  OUT BORAX_OBJECT      *Name
  )
{
  BORAX_OBJECT   ClassPackage = Interp->Globals[BORAX_GLOBAL_CLASS_PACKAGE];
  BORAX_PACKAGE  *ThePackage;

  if (BORAX_DISCRIMINATE (Package) != BORAX_DISCRIM_OBJECT_RECORD) {
    return BoraxPrimitiveTypeError (Interp, Package, ClassPackage);
  }

  ThePackage = (BORAX_PACKAGE *)BORAX_GET_POINTER (Package);

  if (!BORAX_EQ (ThePackage->Record.Class, ClassPackage)) {
    return BoraxPrimitiveTypeError (Interp, Package, ClassPackage);
  }

  *Name = ThePackage->Name;
  return BORAX_NIL;
}

BORAX_OBJECT
EFIAPI
BoraxPrimitiveSymbolPackage (
  IN BORAX_INTERPRETER  *Interp,
  IN BORAX_OBJECT       Symbol,
  OUT BORAX_OBJECT      *Package
  )
{
  BORAX_OBJECT  ClassSymbol = Interp->Globals[BORAX_GLOBAL_CLASS_SYMBOL];
  BORAX_SYMBOL  *TheSymbol;

  if (BORAX_DISCRIMINATE (Symbol) != BORAX_DISCRIM_OBJECT_RECORD) {
    return BoraxPrimitiveTypeError (Interp, Symbol, ClassSymbol);
  }

  TheSymbol = (BORAX_SYMBOL *)BORAX_GET_POINTER (Symbol);

  if (!BORAX_EQ (TheSymbol->Record.Class, ClassSymbol)) {
    return BoraxPrimitiveTypeError (Interp, Symbol, ClassSymbol);
  }

  *Package = TheSymbol->Package;
  return BORAX_NIL;
}

BORAX_OBJECT
EFIAPI
BoraxPrimitiveSymbolName (
  IN BORAX_INTERPRETER  *Interp,
  IN BORAX_OBJECT       Symbol,
  OUT BORAX_OBJECT      *Name
  )
{
  BORAX_OBJECT  ClassSymbol = Interp->Globals[BORAX_GLOBAL_CLASS_SYMBOL];
  BORAX_SYMBOL  *TheSymbol;

  if (BORAX_DISCRIMINATE (Symbol) != BORAX_DISCRIM_OBJECT_RECORD) {
    return BoraxPrimitiveTypeError (Interp, Symbol, ClassSymbol);
  }

  TheSymbol = (BORAX_SYMBOL *)BORAX_GET_POINTER (Symbol);

  if (!BORAX_EQ (TheSymbol->Record.Class, ClassSymbol)) {
    return BoraxPrimitiveTypeError (Interp, Symbol, ClassSymbol);
  }

  *Name = TheSymbol->Name;
  return BORAX_NIL;
}

BORAX_OBJECT
EFIAPI
BoraxPrimitiveSimpleVectorData (
  IN BORAX_INTERPRETER  *Interp,
  IN BORAX_OBJECT       Vector,
  OUT UINTN             *Length,
  OUT BORAX_OBJECT      **Data
  )
{
  BORAX_OBJECT  ClassSimpleVector = Interp->Globals[BORAX_GLOBAL_CLASS_SIMPLE_VECTOR];
  BORAX_RECORD  *Record;

  if (BORAX_DISCRIMINATE (Vector) != BORAX_DISCRIM_OBJECT_RECORD) {
    return BoraxPrimitiveTypeError (Interp, Vector, ClassSimpleVector);
  }

  Record = (BORAX_RECORD *)BORAX_GET_POINTER (Vector);

  if (!BORAX_EQ (Record->Class, ClassSimpleVector)) {
    return BoraxPrimitiveTypeError (Interp, Vector, ClassSimpleVector);
  }

  *Data   = Record->Slots;
  *Length = Record->Length;
  return BORAX_NIL;
}

// TODO: This only works for byte-sized elements (not bit-sized elements)
STATIC EFI_STATUS
EFIAPI
EarlyVectorLength (
  IN BORAX_RECORD  *Record,
  IN UINTN         ElementSize,
  OUT UINTN        *Length
  )
{
  EFI_STATUS  Status;
  UINTN       Bytes, Capacity, Actual;

  Status = SafeUintnMult (Record->Length, sizeof (UINTN), &Bytes);
  if (EFI_ERROR (Status)) {
    return Status;
  }

  // Callers are responsible for ensuring this cannot truncate
  Capacity = Bytes / ElementSize;

  Status = SafeUintnSub (Capacity, Record->LengthAux, &Actual);
  if (EFI_ERROR (Status)) {
    return Status;
  }

  *Length = Actual;
  return EFI_SUCCESS;
}

// TODO: This only works for byte-sized elements (not bit-sized elements)
STATIC BORAX_OBJECT
EFIAPI
VectorLength (
  IN BORAX_INTERPRETER  *Interp,
  IN BORAX_RECORD       *Record,
  IN UINTN              ElementSize,
  OUT UINTN             *Length
  )
{
  EFI_STATUS  Status;

  Status = EarlyVectorLength (Record, ElementSize, Length);
  if (EFI_ERROR (Status)) {
    // We probably don't want to try to print the object...
    return BoraxPrimitiveSimpleCondition (
             Interp,
             Interp->Globals[BORAX_GLOBAL_CLASS_SIMPLE_ERROR],
             L"Malformed LengthAux in vector object",
             0,
             NULL
             );
  }

  return BORAX_NIL;
}

STATIC BORAX_OBJECT
EFIAPI
SimpleVectorSubtypeData (
  IN BORAX_INTERPRETER  *Interp,
  IN BORAX_OBJECT       Class,
  IN UINTN              ElementSize,
  IN BORAX_OBJECT       Vector,
  OUT UINTN             *Length,
  OUT VOID              **Data
  )
{
  BORAX_OBJECT  Condition;
  BORAX_RECORD  *Record;
  UINTN         TheLength;

  if (BORAX_DISCRIMINATE (Vector) != BORAX_DISCRIM_WORD_RECORD) {
    return BoraxPrimitiveTypeError (Interp, Vector, Class);
  }

  Record = (BORAX_RECORD *)BORAX_GET_POINTER (Vector);

  if (!BORAX_EQ (Record->VectorClass, Class)) {
    return BoraxPrimitiveTypeError (Interp, Vector, Class);
  }

  Condition = VectorLength (Interp, Record, ElementSize, &TheLength);
  if (BORAX_BOOL (Condition)) {
    return Condition;
  }

  *Data   = Record->Data;
  *Length = TheLength;
  return BORAX_NIL;
}

BORAX_OBJECT
EFIAPI
BoraxPrimitiveSimpleVectorU8Data (
  IN BORAX_INTERPRETER  *Interp,
  IN BORAX_OBJECT       Vector,
  OUT UINTN             *Length,
  OUT UINT8             **Data
  )
{
  return SimpleVectorSubtypeData (
           Interp,
           Interp->Globals[BORAX_GLOBAL_CLASS_SIMPLE_VECTOR_UNSIGNED_BYTE_8],
           sizeof (UINT8),
           Vector,
           Length,
           (VOID **)Data
           );
}

BORAX_OBJECT
EFIAPI
BoraxPrimitiveStringData (
  IN BORAX_INTERPRETER  *Interp,
  IN BORAX_OBJECT       String,
  OUT UINTN             *Length,
  OUT CHAR16            **Data
  )
{
  return SimpleVectorSubtypeData (
           Interp,
           Interp->Globals[BORAX_GLOBAL_CLASS_STRING],
           sizeof (CHAR16),
           String,
           Length,
           (VOID **)Data
           );
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
    PRIMITIVE_ERROR ("Not a valid string object");
    return Status;
  }

  Status = EarlyVectorLength (Record, sizeof (CHAR16), &Chars1);
  if (EFI_ERROR (Status)) {
    PRIMITIVE_ERROR ("Malformed LengthAux");
    return Status;
  }

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
  OUT BORAX_PACKAGE            **Package
  )
{
  EFI_STATUS    Status;
  BORAX_OBJECT  List = Env->Packages;

  while (BORAX_DISCRIMINATE (List) == BORAX_DISCRIM_CONS) {
    BORAX_CONS     *Cons = (BORAX_CONS *)BORAX_GET_POINTER (List);
    BORAX_PACKAGE  *SomePackage;
    BOOLEAN        Match;

    Status = BORAX_GET_OBJECT_RECORD (Cons->Car, &SomePackage);
    if (EFI_ERROR (Status)) {
      PRIMITIVE_ERROR ("Not a valid package object");
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

  PRIMITIVE_ERROR ("Package not found: %s", Name);
  return EFI_INVALID_PARAMETER;
}

STATIC EFI_STATUS
EFIAPI
EarlyFindSymbol (
  IN BORAX_PACKAGE  *Package,
  IN CONST CHAR16   *Name,
  OUT BORAX_SYMBOL  **Symbol
  )
{
  EFI_STATUS    Status;
  BORAX_OBJECT  List = Package->Symbols;

  while (BORAX_DISCRIMINATE (List) == BORAX_DISCRIM_CONS) {
    BORAX_CONS    *Cons = (BORAX_CONS *)BORAX_GET_POINTER (List);
    BORAX_SYMBOL  *SomeSymbol;
    BOOLEAN       Match;

    Status = BORAX_GET_OBJECT_RECORD (Cons->Car, &SomeSymbol);
    if (EFI_ERROR (Status)) {
      PRIMITIVE_ERROR ("Not a valid symbol object");
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

  PRIMITIVE_ERROR ("Symbol not found: %s", Name);
  return EFI_INVALID_PARAMETER;
}

typedef struct {
  enum {
    // Guard against accidentally forgetting to add a descriptor
    GLOBAL_DESC_NOT_IMPLEMENTED = 0,
    GLOBAL_DESC_PACKAGE,
    GLOBAL_DESC_SYMBOL,
    GLOBAL_DESC_CLASS,
  } Tag;
  BORAX_GLOBAL    Package; // Symbol, Class
  CONST CHAR16    *Name;   // Package, Symbol, Class
} GLOBAL_DESC;

STATIC CONST GLOBAL_DESC  gGlobalDesc[BORAX_GLOBAL_COUNT] = {
  // Standard packages
  [BORAX_GLOBAL_PACKAGE_COMMON_LISP] =                                                  {
    .Tag  = GLOBAL_DESC_PACKAGE,
    .Name = L"COMMON-LISP",
  },
  [BORAX_GLOBAL_PACKAGE_KEYWORD] =                                                      {
    .Tag  = GLOBAL_DESC_PACKAGE,
    .Name = L"KEYWORD",
  },
  // Built-in packages
  [BORAX_GLOBAL_PACKAGE_BORAX_RUNTIME] =                                                {
    .Tag  = GLOBAL_DESC_PACKAGE,
    .Name = L"BORAX-RUNTIME",
  },
  // Standard conditions
  [BORAX_GLOBAL_CLASS_SIMPLE_ERROR] =                                                   {
    .Tag     = GLOBAL_DESC_CLASS,
    .Package = BORAX_GLOBAL_PACKAGE_COMMON_LISP,
    .Name    = L"SIMPLE-ERROR",
  },
  [BORAX_GLOBAL_CLASS_SIMPLE_PROGRAM_ERROR] =                                           {
    .Tag     = GLOBAL_DESC_CLASS,
    .Package = BORAX_GLOBAL_PACKAGE_BORAX_RUNTIME,
    .Name    = L"SIMPLE-PROGRAM-ERROR",
  },
  [BORAX_GLOBAL_CLASS_TYPE_ERROR] =                                                     {
    .Tag     = GLOBAL_DESC_CLASS,
    .Package = BORAX_GLOBAL_PACKAGE_COMMON_LISP,
    .Name    = L"TYPE-ERROR",
  },
  [BORAX_GLOBAL_CLASS_UNDEFINED_FUNCTION] =                                             {
    .Tag     = GLOBAL_DESC_CLASS,
    .Package = BORAX_GLOBAL_PACKAGE_COMMON_LISP,
    .Name    = L"UNDEFINED-FUNCTION",
  },
  // Built-in conditions
  [BORAX_GLOBAL_CLASS_HEAP_EXHAUSTED] =                                                 {
    .Tag     = GLOBAL_DESC_CLASS,
    .Package = BORAX_GLOBAL_PACKAGE_BORAX_RUNTIME,
    .Name    = L"HEAP-EXHAUSTED",
  },
  [BORAX_GLOBAL_CLASS_STACK_EXHAUSTED] =                                                {
    .Tag     = GLOBAL_DESC_CLASS,
    .Package = BORAX_GLOBAL_PACKAGE_BORAX_RUNTIME,
    .Name    = L"STACK-EXHAUSTED",
  },
  [BORAX_GLOBAL_CLASS_LOCATION_ERROR] =                                                 {
    .Tag     = GLOBAL_DESC_CLASS,
    .Package = BORAX_GLOBAL_PACKAGE_BORAX_RUNTIME,
    .Name    = L"LOCATION-ERROR",
  },
  // Standard classes
  [BORAX_GLOBAL_CLASS_CONS] =                                                           {
    .Tag     = GLOBAL_DESC_CLASS,
    .Package = BORAX_GLOBAL_PACKAGE_COMMON_LISP,
    .Name    = L"CONS",
  },
  [BORAX_GLOBAL_CLASS_FIXNUM] =                                                         {
    .Tag     = GLOBAL_DESC_CLASS,
    .Package = BORAX_GLOBAL_PACKAGE_COMMON_LISP,
    .Name    = L"FIXNUM",
  },
  [BORAX_GLOBAL_CLASS_FUNCTION] =                                                       {
    .Tag     = GLOBAL_DESC_CLASS,
    .Package = BORAX_GLOBAL_PACKAGE_COMMON_LISP,
    .Name    = L"FUNCTION",
  },
  [BORAX_GLOBAL_CLASS_PACKAGE] =                                                        {
    .Tag     = GLOBAL_DESC_CLASS,
    .Package = BORAX_GLOBAL_PACKAGE_COMMON_LISP,
    .Name    = L"PACKAGE",
  },
  [BORAX_GLOBAL_CLASS_SIMPLE_VECTOR] =                                                  {
    .Tag     = GLOBAL_DESC_CLASS,
    .Package = BORAX_GLOBAL_PACKAGE_COMMON_LISP,
    .Name    = L"SIMPLE-VECTOR",
  },
  [BORAX_GLOBAL_CLASS_STRING] =                                                         {
    .Tag     = GLOBAL_DESC_CLASS,
    .Package = BORAX_GLOBAL_PACKAGE_COMMON_LISP,
    .Name    = L"STRING",
  },
  [BORAX_GLOBAL_CLASS_SYMBOL] =                                                         {
    .Tag     = GLOBAL_DESC_CLASS,
    .Package = BORAX_GLOBAL_PACKAGE_COMMON_LISP,
    .Name    = L"SYMBOL",
  },
  // Built-in classes
  [BORAX_GLOBAL_CLASS_BYTECODE_FUNCTION] =                                              {
    .Tag     = GLOBAL_DESC_CLASS,
    .Package = BORAX_GLOBAL_PACKAGE_BORAX_RUNTIME,
    .Name    = L"BYTECODE-FUNCTION",
  },
  [BORAX_GLOBAL_CLASS_MULTIPLE_VALUES] =                                                {
    .Tag     = GLOBAL_DESC_CLASS,
    .Package = BORAX_GLOBAL_PACKAGE_BORAX_RUNTIME,
    .Name    = L"MULTIPLE-VALUES",
  },
  [BORAX_GLOBAL_CLASS_SIMPLE_VECTOR_UNSIGNED_BYTE_8] =                                  {
    .Tag     = GLOBAL_DESC_CLASS,
    .Package = BORAX_GLOBAL_PACKAGE_BORAX_RUNTIME,
    .Name    = L"SIMPLE-VECTOR-UNSIGNED-BYTE-8",
  },
  // TODO: just create these
  // Keyword symbols
  [BORAX_GLOBAL_KEYWORD_CONSTANT] =                                                     {
    .Tag     = GLOBAL_DESC_SYMBOL,
    .Package = BORAX_GLOBAL_PACKAGE_KEYWORD,
    .Name    = L"CONSTANT",
  },
  [BORAX_GLOBAL_KEYWORD_LOCAL] =                                                        {
    .Tag     = GLOBAL_DESC_SYMBOL,
    .Package = BORAX_GLOBAL_PACKAGE_KEYWORD,
    .Name    = L"LOCAL",
  },
  [BORAX_GLOBAL_KEYWORD_SHARED] =                                                       {
    .Tag     = GLOBAL_DESC_SYMBOL,
    .Package = BORAX_GLOBAL_PACKAGE_KEYWORD,
    .Name    = L"SHARED",
  },
};

EFI_STATUS
EFIAPI
BoraxGlobalInit (
  IN BORAX_INTERPRETER  *Interp
  )
{
  EFI_STATUS  Status;
  UINTN       Done = 0;
  UINTN       I;

  for (I = 0; I < BORAX_GLOBAL_COUNT; ++I) {
    Interp->Globals[I] = BORAX_UNBOUND;
  }

  // The dependency graph needs to be acyclic and we could just insist on the
  // enum being topologically sorted, but it's easy enough to check the
  // dependencies since they're needed anyway, and it prevents Weird Bugs from
  // happening if we do something very silly.
  while (Done < BORAX_GLOBAL_COUNT) {
    UINTN  PrevDone = Done;

    for (I = 0; I < BORAX_GLOBAL_COUNT; ++I) {
      CONST GLOBAL_DESC  *Desc = &gGlobalDesc[I];
      BORAX_OBJECT       *Slot = &Interp->Globals[I];

      if (BORAX_BOUNDP (*Slot)) {
        continue;
      }

      switch (Desc->Tag) {
        case GLOBAL_DESC_NOT_IMPLEMENTED:
          PRIMITIVE_ERROR ("Unimplemented global: %u", I);
          return EFI_UNSUPPORTED;

        case GLOBAL_DESC_PACKAGE:
        {
          BORAX_PACKAGE  *Package;

          Status = EarlyFindPackage (
                     Interp->GlobalEnvironment,
                     Desc->Name,
                     &Package
                     );
          if (EFI_ERROR (Status)) {
            return Status;
          }

          *Slot = BORAX_MAKE_POINTER (Package);
          ++Done;
          break;
        }

        case GLOBAL_DESC_SYMBOL:
        {
          BORAX_PACKAGE  *Package;
          BORAX_SYMBOL   *Symbol;

          if (!BORAX_BOUNDP (Interp->Globals[Desc->Package])) {
            continue;
          }

          Package = (BORAX_PACKAGE *)BORAX_GET_POINTER (
                                       Interp->Globals[Desc->Package]
                                       );

          Status = EarlyFindSymbol (Package, Desc->Name, &Symbol);
          if (EFI_ERROR (Status)) {
            return Status;
          }

          *Slot = BORAX_MAKE_POINTER (Symbol);
          ++Done;
          break;
        }

        case GLOBAL_DESC_CLASS:
        {
          BORAX_PACKAGE         *Package;
          BORAX_SYMBOL          *Symbol;
          BORAX_STANDARD_CLASS  *Class;

          if (!BORAX_BOUNDP (Interp->Globals[Desc->Package])) {
            continue;
          }

          Package = (BORAX_PACKAGE *)BORAX_GET_POINTER (
                                       Interp->Globals[Desc->Package]
                                       );

          Status = EarlyFindSymbol (Package, Desc->Name, &Symbol);
          if (EFI_ERROR (Status)) {
            return Status;
          }

          Status = BORAX_GET_OBJECT_RECORD (Symbol->Class, &Class);
          if (EFI_ERROR (Status)) {
            return Status;
          }

          *Slot = BORAX_MAKE_POINTER (Class);
          ++Done;
          break;
        }

        default:
          PRIMITIVE_ERROR ("Illegal tag: %u", Desc->Tag);
          return EFI_INVALID_PARAMETER;
      }
    }

    if (Done == PrevDone) {
      PRIMITIVE_ERROR (
        "Failed to make progress loading globals"
        " (circular dependency?)"
        );
      return EFI_ABORTED;
    }
  }

  return EFI_SUCCESS;
}
