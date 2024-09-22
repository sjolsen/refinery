#ifndef BORAX_PLUGIN_H
#define BORAX_PLUGIN_H

#include <Library/BoraxInterpreter.h>
#include <Library/BoraxPrimitive.h>

typedef struct {
  CONST CHAR16    *Package;
  CONST CHAR16    *Name;
} BORAX_DESCRIPTOR_SYMBOL;

typedef struct _BORAX_DESCRIPTOR_CONSTANT BORAX_DESCRIPTOR_CONSTANT;

typedef struct {
  UINTN                              Length;
  CONST BORAX_DESCRIPTOR_CONSTANT    *Values;
} BORAX_DESCRIPTOR_LIST;

struct _BORAX_DESCRIPTOR_CONSTANT {
  enum {
    BORAX_CONST_DATA,
    BORAX_CONST_SYMBOL,
    BORAX_CONST_KEYWORD,
    BORAX_CONST_CLASS,
    BORAX_CONST_LIST,
  } Tag;
  union {
    UINTN                      Index;  // Object
    BORAX_DESCRIPTOR_SYMBOL    Symbol; // Symbol, Keyword, Class
    BORAX_DESCRIPTOR_LIST      List;   // List
  };
};

typedef struct {
  BORAX_DESCRIPTOR_SYMBOL    Name;
  UINTN                      Entry;
  BORAX_BUILT_IN_CODE        Code;
  UINTN                      Locals;
  BORAX_DESCRIPTOR_LIST      Constants;
} BORAX_DESCRIPTOR_FUNCTION;

typedef struct {
  UINTN                                     Length;
  CONST BORAX_DESCRIPTOR_FUNCTION *CONST    *Values;
} BORAX_DESCRIPTOR_FUNCTION_LIST;

typedef struct {
  BORAX_DESCRIPTOR_FUNCTION_LIST    Functions;
} BORAX_DESCRIPTOR_PLUGIN;

typedef struct {
  UINTN           Length;
  BORAX_OBJECT    *Values;
} BORAX_PLUGIN_DATA;

// TODO: Provide object resolved from descriptors via an out parameter of
// BoraxAddPlugin
BORAX_OBJECT
EFIAPI
BoraxIntern (
  IN BORAX_INTERPRETER              *Interp,
  IN CONST BORAX_DESCRIPTOR_SYMBOL  *Desc,
  OUT BORAX_SYMBOL                  **Symbol
  );

BORAX_OBJECT
EFIAPI
BoraxAddPlugin (
  IN BORAX_INTERPRETER              *Interp,
  IN CONST BORAX_DESCRIPTOR_PLUGIN  *Plugin,
  IN CONST BORAX_PLUGIN_DATA        *Data
  );

#endif // BORAX_PLUGIN_H
