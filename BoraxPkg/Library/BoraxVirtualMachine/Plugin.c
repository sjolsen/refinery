#include <Library/BoraxPlugin.h>

BORAX_OBJECT
EFIAPI
BoraxIntern (
  IN BORAX_INTERPRETER              *Interp,
  IN CONST BORAX_DESCRIPTOR_SYMBOL  *Desc,
  OUT BORAX_SYMBOL                  **Symbol
  )
{
  BORAX_OBJECT   Condition;
  BORAX_PACKAGE  *Package;

  Condition = BoraxPrimitiveRequirePackage (
                Interp,
                BoraxCString (Desc->Package),
                &Package
                );
  if (BORAX_BOOL (Condition)) {
    return Condition;
  }

  return BoraxPrimitiveIntern (
           Interp,
           Package,
           BoraxCString (Desc->Name),
           Symbol
           );
}

STATIC BORAX_OBJECT
EFIAPI
DefineConstant (
  IN BORAX_INTERPRETER                *Interp,
  IN BORAX_OBJECT                     TopLevel,
  IN CONST BORAX_DESCRIPTOR_CONSTANT  *Const,
  IN CONST BORAX_PLUGIN_DATA          *Data,
  OUT BORAX_OBJECT                    *Object
  )
{
  BORAX_OBJECT  Condition;

  switch (Const->Tag) {
    case BORAX_CONST_DATA:
      if (Const->Index >= Data->Length) {
        BORAX_OBJECT  Args[] = {
          BORAX_MAKE_FIXNUM (Const->Index),
          BORAX_MAKE_FIXNUM (Data->Length),
        };
        // TODO: Common API for raising index errors
        return BoraxPrimitiveSimpleCondition (
                 Interp,
                 Interp->Globals[BORAX_GLOBAL_CLASS_SIMPLE_ERROR],
                 BoraxCString (L"Invalid data constant: ~S >= ~S"),
                 ARRAY_SIZE (Args),
                 Args
                 );
      }

      *Object = Data->Values[Const->Index];
      return BORAX_NIL;

    case BORAX_CONST_SYMBOL:
    {
      BORAX_PACKAGE  *Package;
      BORAX_SYMBOL   *Symbol;

      Condition = BoraxPrimitiveRequirePackage (
                    Interp,
                    BoraxCString (Const->Symbol.Package),
                    &Package
                    );
      if (BORAX_BOOL (Condition)) {
        return Condition;
      }

      Condition = BoraxPrimitiveIntern (
                    Interp,
                    Package,
                    BoraxCString (Const->Symbol.Name),
                    &Symbol
                    );
      if (BORAX_BOOL (Condition)) {
        return Condition;
      }

      *Object = BORAX_MAKE_POINTER (Symbol);
      return BORAX_NIL;
    }

    case BORAX_CONST_KEYWORD:
    {
      BORAX_PACKAGE  *Package;
      BORAX_SYMBOL   *Symbol;

      Condition = BoraxPrimitiveRequirePackage (Interp, BoraxCString (L"KEYWORD"), &Package);
      if (BORAX_BOOL (Condition)) {
        return Condition;
      }

      Condition = BoraxPrimitiveIntern (
                    Interp,
                    Package,
                    BoraxCString (Const->Symbol.Name),
                    &Symbol
                    );
      if (BORAX_BOOL (Condition)) {
        return Condition;
      }

      *Object = BORAX_MAKE_POINTER (Symbol);
      return BORAX_NIL;
    }

    case BORAX_CONST_CLASS:
    {
      BORAX_PACKAGE  *Package;
      BORAX_SYMBOL   *Symbol;

      Condition = BoraxPrimitiveRequirePackage (
                    Interp,
                    BoraxCString (Const->Symbol.Package),
                    &Package
                    );
      if (BORAX_BOOL (Condition)) {
        return Condition;
      }

      Condition = BoraxPrimitiveIntern (
                    Interp,
                    Package,
                    BoraxCString (Const->Symbol.Name),
                    &Symbol
                    );
      if (BORAX_BOOL (Condition)) {
        return Condition;
      }

      *Object = Symbol->Class;
      return BORAX_NIL;
    }

    case BORAX_CONST_LIST:
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
        Condition = DefineConstant (
                      Interp,
                      TopLevel,
                      &Const->List.Values[I],
                      Data,
                      &Cons->Car
                      );
        if (BORAX_BOOL (Condition)) {
          return Condition;
        }

        List = BORAX_MAKE_POINTER (Cons);
      }

      *Object = List;
      return BORAX_NIL;
    }

    default:
      return BoraxPrimitiveSimpleCondition (
               Interp,
               Interp->Globals[BORAX_GLOBAL_CLASS_SIMPLE_ERROR],
               BoraxCString (L"Invalid descriptor for ~S"),
               1,
               &TopLevel
               );
  }
}

STATIC BORAX_OBJECT
EFIAPI
DefineFunction (
  IN BORAX_INTERPRETER                *Interp,
  IN CONST BORAX_DESCRIPTOR_FUNCTION  *Desc,
  IN CONST BORAX_PLUGIN_DATA          *Data
  )
{
  EFI_STATUS               Status;
  BORAX_OBJECT             Condition;
  BORAX_SYMBOL             *Symbol;
  BORAX_BUILT_IN_FUNCTION  *F;
  UINTN                    I;

  Condition = BoraxIntern (Interp, &Desc->Name, &Symbol);
  if (BORAX_BOOL (Condition)) {
    return Condition;
  }

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
    Condition = DefineConstant (
                  Interp,
                  BORAX_MAKE_POINTER (Symbol),
                  &Desc->Constants.Values[I],
                  Data,
                  &F->Constants[I]
                  );
    if (BORAX_BOOL (Condition)) {
      return Condition;
    }
  }

  Symbol->Function = BORAX_MAKE_POINTER (F);
  return BORAX_NIL;
}

BORAX_OBJECT
EFIAPI
BoraxAddPlugin (
  IN BORAX_INTERPRETER              *Interp,
  IN CONST BORAX_DESCRIPTOR_PLUGIN  *Plugin,
  IN CONST BORAX_PLUGIN_DATA        *Data
  )
{
  BORAX_OBJECT  Condition;
  UINTN         I;

  for (I = 0; I < Plugin->Functions.Length; ++I) {
    Condition = DefineFunction (Interp, Plugin->Functions.Values[I], Data);
    if (BORAX_BOOL (Condition)) {
      return Condition;
    }
  }

  return BORAX_NIL;
}
