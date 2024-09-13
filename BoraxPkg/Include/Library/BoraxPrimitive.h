#ifndef BORAX_PRIMITIVE_H
#define BORAX_PRIMITIVE_H

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
  BORAX_RECORD    Record;
  BORAX_OBJECT    FormatControl;
  BORAX_OBJECT    FormatArguments;
} BORAX_SIMPLE_CONDITION;

BORAX_OBJECT
EFIAPI
BoraxPrimitiveSimpleCondition (
  IN BORAX_INTERPRETER   *Interp,
  IN BORAX_GLOBAL        Class,
  IN CONST CHAR16        *Control,
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
  IN BORAX_GLOBAL       ExpectedType
  );

typedef struct {
  BORAX_RECORD    Record;
  BORAX_OBJECT    Name;
} BORAX_CELL_ERROR;

BORAX_OBJECT
EFIAPI
BoraxPrimitiveCellError (
  IN BORAX_INTERPRETER  *Interp,
  IN BORAX_GLOBAL       Class,
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

typedef struct {
  BORAX_RECORD    Record;
  BORAX_OBJECT    Name;
  BORAX_OBJECT    Symbols;
} BORAX_PACKAGE;

typedef struct {
  BORAX_RECORD    Record;
  BORAX_OBJECT    Package;
  BORAX_OBJECT    Name;
  BORAX_OBJECT    Value;
  BORAX_OBJECT    Function;
  BORAX_OBJECT    Class;
} BORAX_SYMBOL;

typedef struct {
  BORAX_RECORD    Record;
  BORAX_OBJECT    Name;
} BORAX_STANDARD_CLASS;

#endif // BORAX_PRIMITIVE_H
