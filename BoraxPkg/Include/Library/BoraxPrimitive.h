#ifndef BORAX_PRIMITIVE_H
#define BORAX_PRIMITIVE_H

#include <Library/BaseLib.h>
#include <Library/BoraxInterpreter.h>
#include <Library/BoraxMemory.h>

// TODO: Make this file private

/*
 * Lisp primitives
 * ===============
 *
 * The implementation of the interpreter requires the ability to create and
 * manipulate Lisp data from C without relying on the interpreter itself. These
 * primitive implementations of standard Lisp functions (and some non-standard
 * helpers) are kept here so that Interpreter.c can remain focused on the
 * interpreter and its internal data structures.
 *
 * Because these functions cannot use the Lisp call stack, they cannot implement
 * the full functionality of some standard functions -- for instance anything
 * that relies on generic function dispatch. These primitives may be useful in
 * implementing the fully-featured versions of those standard functions, but not
 * without additional logic for type-checking, method dispatch, etc.
 *
 * These functions can access global interpreter state but not task state, and
 * can return conditions but cannot interact with the condition system in its
 * full generality.
 *
 * Globals
 * -------
 *
 * Rather than perform a full symbol lookup every time the interpreter needs to
 * reference an item defined in the global environment, these items are cached
 * on interpreter start-up. The interpreter holds this cache, but it is defined
 * here.
 */

EFI_STATUS
EFIAPI
BoraxGlobalInit (
  IN BORAX_INTERPRETER  *Interp
  );

typedef struct {
  UINTN     Length;
  CHAR16    *Data;
} BORAX_STRING;

typedef struct {
  UINTN           Length;
  CONST CHAR16    *Data;
} BORAX_CONST_STRING;

STATIC inline BORAX_CONST_STRING
EFIAPI
BoraxCString (
  IN CONST CHAR16  *CString
  )
{
  BORAX_CONST_STRING  Result = { StrLen (CString), CString };

  return Result;
}

STATIC inline BORAX_CONST_STRING
EFIAPI
BoraxConstString (
  IN BORAX_STRING  CString
  )
{
  BORAX_CONST_STRING  Result = { CString.Length, CString.Data };

  return Result;
}

typedef struct {
  BORAX_RECORD    Record;
  BORAX_OBJECT    FormatControl;
  BORAX_OBJECT    FormatArguments;
} BORAX_SIMPLE_CONDITION;

BORAX_OBJECT
EFIAPI
BoraxPrimitiveSimpleCondition (
  IN BORAX_INTERPRETER   *Interp,
  IN BORAX_OBJECT        Class,
  IN BORAX_CONST_STRING  Control,
  IN UINTN               ArgsLength,
  IN CONST BORAX_OBJECT  *Args
  );

typedef struct {
  BORAX_RECORD    Record;
  BORAX_OBJECT    Datum;
  BORAX_OBJECT    ExpectedType;
} BORAX_TYPE_ERROR;

BORAX_OBJECT
EFIAPI
BoraxPrimitiveTypeError (
  IN BORAX_INTERPRETER  *Interp,
  IN BORAX_OBJECT       Datum,
  IN BORAX_OBJECT       ExpectedType
  );

typedef struct {
  BORAX_RECORD    Record;
  BORAX_OBJECT    Name;
} BORAX_CELL_ERROR;

BORAX_OBJECT
EFIAPI
BoraxPrimitiveCellError (
  IN BORAX_INTERPRETER  *Interp,
  IN BORAX_OBJECT       Class,
  IN BORAX_OBJECT       Name
  );

BORAX_OBJECT
EFIAPI
BoraxPrimitiveLocalLocationError (
  IN BORAX_INTERPRETER  *Interp,
  IN UINTN              Index
  );

BORAX_OBJECT
EFIAPI
BoraxPrimitiveSharedBlockLocationError (
  IN BORAX_INTERPRETER  *Interp,
  IN UINTN              Index
  );

BORAX_OBJECT
EFIAPI
BoraxPrimitiveConstantLocationError (
  IN BORAX_INTERPRETER  *Interp,
  IN UINTN              Index
  );

BORAX_OBJECT
EFIAPI
BoraxPrimitiveStackExhausted (
  IN BORAX_INTERPRETER  *Interp
  );

BORAX_OBJECT
EFIAPI
BoraxPrimitiveHeapExhausted (
  IN BORAX_INTERPRETER  *Interp
  );

BORAX_OBJECT
EFIAPI
BoraxPrimitiveTheFixnum (
  IN BORAX_INTERPRETER  *Interp,
  IN BORAX_OBJECT       Object,
  OUT INTN              *Fixnum
  );

BORAX_OBJECT
EFIAPI
BoraxPrimitiveTheCharacter (
  IN BORAX_INTERPRETER  *Interp,
  IN BORAX_OBJECT       Object,
  OUT CHAR16            *Character
  );

BORAX_OBJECT
EFIAPI
BoraxPrimitiveFind (
  IN BORAX_INTERPRETER  *Interp,
  IN BORAX_OBJECT       Object,
  IN BORAX_OBJECT       List,
  OUT BOOLEAN           *Found
  );

typedef struct {
  BORAX_RECORD    Record;
  BORAX_OBJECT    Name;
  BORAX_OBJECT    Symbols;
} BORAX_PACKAGE;

BORAX_OBJECT
EFIAPI
BoraxPrimitiveThePackage (
  IN BORAX_INTERPRETER  *Interp,
  IN BORAX_OBJECT       Object,
  OUT BORAX_PACKAGE     **Package
  );

BORAX_OBJECT
EFIAPI
BoraxPrimitiveFindPackage (
  IN BORAX_INTERPRETER   *Interp,
  IN BORAX_CONST_STRING  Name,
  OUT BOOLEAN            *Found,
  OUT BORAX_PACKAGE      **Package
  );

BORAX_OBJECT
EFIAPI
BoraxPrimitiveRequirePackage (
  IN BORAX_INTERPRETER   *Interp,
  IN BORAX_CONST_STRING  Name,
  OUT BORAX_PACKAGE      **Package
  );

typedef struct {
  BORAX_RECORD    Record;
  BORAX_OBJECT    Package;
  BORAX_OBJECT    Name;
  BORAX_OBJECT    Value;
  BORAX_OBJECT    Function;
  BORAX_OBJECT    Class;
} BORAX_SYMBOL;

BORAX_OBJECT
EFIAPI
BoraxPrimitiveTheSymbol (
  IN BORAX_INTERPRETER  *Interp,
  IN BORAX_OBJECT       Object,
  OUT BORAX_SYMBOL      **Symbol
  );

BORAX_OBJECT
EFIAPI
BoraxPrimitiveFindSymbol (
  IN BORAX_INTERPRETER   *Interp,
  IN BORAX_PACKAGE       *Package,
  IN BORAX_CONST_STRING  Name,
  OUT BOOLEAN            *Found,
  OUT BORAX_SYMBOL       **Symbol
  );

BORAX_OBJECT
EFIAPI
BoraxPrimitiveIntern (
  IN BORAX_INTERPRETER   *Interp,
  IN BORAX_PACKAGE       *Package,
  IN BORAX_CONST_STRING  Name,
  OUT BORAX_SYMBOL       **Symbol
  );

BORAX_OBJECT
EFIAPI
BoraxPrimitiveKeyword (
  IN BORAX_INTERPRETER   *Interp,
  IN BORAX_CONST_STRING  Name,
  OUT BORAX_SYMBOL       **Symbol
  );

// TODO: Distinguish between standard-class and built-in-class
typedef struct {
  BORAX_RECORD    Record;
  BORAX_OBJECT    Name;
  BORAX_OBJECT    PrecedenceList;
} BORAX_STANDARD_CLASS;

BORAX_OBJECT
EFIAPI
BoraxPrimitiveTheStandardClass (
  IN BORAX_INTERPRETER      *Interp,
  IN BORAX_OBJECT           Object,
  OUT BORAX_STANDARD_CLASS  **Class
  );

BORAX_OBJECT
EFIAPI
BoraxPrimitiveClassOf (
  IN BORAX_INTERPRETER  *Interp,
  IN BORAX_OBJECT       Object,
  OUT BORAX_OBJECT      *Class
  );

BORAX_OBJECT
EFIAPI
BoraxPrimitiveClassTypep (
  IN BORAX_INTERPRETER  *Interp,
  IN BORAX_OBJECT       Object,
  IN BORAX_OBJECT       Type,
  OUT BOOLEAN           *Match
  );

BORAX_OBJECT
EFIAPI
BoraxPrimitiveMakeList (
  IN BORAX_INTERPRETER   *Interp,
  IN UINTN               Length,
  IN CONST BORAX_OBJECT  *Items,
  OUT BORAX_OBJECT       *List
  );

BORAX_OBJECT
EFIAPI
BoraxPrimitiveSimpleVectorData (
  IN BORAX_INTERPRETER  *Interp,
  IN BORAX_OBJECT       Vector,
  OUT UINTN             *Length,
  OUT BORAX_OBJECT      **Data
  );

BORAX_OBJECT
EFIAPI
BoraxPrimitiveSimpleVectorU8Data (
  IN BORAX_INTERPRETER  *Interp,
  IN BORAX_OBJECT       Vector,
  OUT UINTN             *Length,
  OUT UINT8             **Data
  );

BORAX_OBJECT
EFIAPI
BoraxPrimitiveMakeString (
  IN BORAX_INTERPRETER   *Interp,
  IN BORAX_CONST_STRING  CString,
  OUT BORAX_OBJECT       *String
  );

BORAX_OBJECT
EFIAPI
BoraxPrimitiveStringData (
  IN BORAX_INTERPRETER  *Interp,
  IN BORAX_OBJECT       Object,
  OUT BORAX_STRING      *String
  );

BORAX_OBJECT
EFIAPI
BoraxPrimitiveStringEqual (
  IN BORAX_INTERPRETER   *Interp,
  IN BORAX_OBJECT        String1,
  IN BORAX_CONST_STRING  String2,
  OUT BOOLEAN            *Match
  );

#endif // BORAX_PRIMITIVE_H
