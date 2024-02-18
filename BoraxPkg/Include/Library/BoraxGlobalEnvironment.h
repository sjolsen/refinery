#ifndef BORAX_GLOBAL_ENVIRONMENT_H
#define BORAX_GLOBAL_ENVIRONMENT_H

#include <Library/BoraxMemory.h>

/*
 * The global environment
 * ======================
 *
 * The global environment consists of name bindings in a number of namespaces:
 *
 * - The value namespace, corresponding to bound symbols;
 * - The function namespace, corresponding to fbound symbols;
 * - The package registry, an implicit mapping of names to package objects;
 * - The class registry, an implicit mapping of names to class objects.
 *
 * The global namespace is shared across all tasks and is the first thing that
 * needs to be populated before we can start working with lisp data.
 */

typedef struct _BORAX_GLOBAL_ENVIRONMENT BORAX_GLOBAL_ENVIRONMENT;

/*
 * Classes
 * =======
 *
 * Classes are the most basic facility for object discrimination in the lisp
 * environment. Each object has exactly one direct class, which describes what
 * kind of object it is (a cons cell, a fixnum, a string, etc.). We must at
 * least outline classes before lisp code can do anything useful with the
 * objects in its environment.
 *
 * There are two important and distinct relations involving objects and classes:
 * the instance relation and the subclass relation (classes, being objects, are
 * subject to both). The subclass relation is encoded at the class level. For
 * records, the instance relation is encoded by the Class field; for all other
 * primitive types, it is derived from the object's tag(s).
 *
 * The subclass relation forms a chain from any class leading to the class named
 * by the symbol T; likewise, the instance relation forms a chain from any
 * object leading to the class STANDARD-CLASS, which is an instance of itself
 * and serves as the metaclass for all object record classes. We therefore begin
 * by defining STANDARD-CLASS.
 *
 * Each class names its direct superclasses and its slots. The slot definitions
 * are objects describing instance layout to the lisp system.
 */

typedef struct {
  BORAX_RECORD    Record;
  BORAX_OBJECT    Name;
  BORAX_OBJECT    DirectSuperclasses;
  BORAX_OBJECT    Slots;
} BORAX_STANDARD_CLASS;

typedef struct {
  BORAX_RECORD    Record;
  BORAX_OBJECT    Class;
  BORAX_OBJECT    Name;
  BORAX_OBJECT    Index;
} BORAX_SLOT_DEFINITION;

/*
 * Primitive types
 * ===============
 *
 * The primitive types are:
 *
 * - FIXNUM
 * - CHARACTER
 * - CONS
 * - WEAK-POINTER
 * - PIN
 *
 * Each primitive type is a class corresponding to one of the object
 * representations defined by the garbage collector. Each of these classes is an
 * instance of the metaclass BUILT-IN-CLASS.
 *
 * Note that there is no primitive type corresponding to word records. Word
 * records are exposed to the lisp environment through the WORD-RECORD-CLASS
 * metaclass, which is a subclass of BUILT-IN-CLASS.
 */

typedef struct {
  BORAX_RECORD    Record;
  BORAX_OBJECT    Name;
} BORAX_BUILT_IN_CLASS;

typedef struct {
  BORAX_BUILT_IN_CLASS    BuiltInClass;
  BORAX_OBJECT            ElementClass;
  BORAX_OBJECT            ElementBits;
} BORAX_WORD_RECORD_CLASS;

/*
 * Symbols and packages
 * ====================
 *
 * Symbols are objects whose identities correspond logically to the values of
 * their name strings. Maintaining this correspondence in general requires the
 * use of hash tables or a similar associative data structure. In the early
 * environment, this correspondence is maintained manually.
 *
 * Packages associate symbol names with symbol objects. This is done with a pair
 * of hash tables: one for internal symbols and one for external symbols. A
 * symbol may not be both internal and external in the same package, but a
 * symbol may be be internal in one package and external in another.
 *
 * A symbol object may appear in any number of packages, including zero. A
 * symbol may have a single "home" package indicated by its PACKAGE
 * slot. Multiple otherwise unrelated symbol objects with the same name may be
 * interned in different packages, but only one symbol with a given name may be
 * interned in a given package.
 */

typedef struct {
  BORAX_RECORD    Record;
  BORAX_OBJECT    Name;
  BORAX_OBJECT    Package;
  BORAX_OBJECT    Plist;
  BORAX_OBJECT    Value;
  BORAX_OBJECT    Function;
} BORAX_SYMBOL;

typedef struct {
  BORAX_RECORD    Record;
  BORAX_OBJECT    Name;
  BORAX_OBJECT    InternalSymbols;
  BORAX_OBJECT    ExternalSymbols;
} BORAX_PACKAGE;

/*
 * Hash tables
 * ===========
 *
 * Hash tables are plain lisp data structures. They are included in the
 * bootstrapping process for the global environment because they are used to
 * establish the name mappings for the package and class registries and to
 * implement string interning.
 */

typedef struct {
  BORAX_RECORD    Record;
  BORAX_OBJECT    Test;
  BORAX_OBJECT    Buckets;
} BORAX_HASH_TABLE;

/*
 * The environment cache
 * =====================
 *
 * Normally, predefined objects are exposed to both lisp and C by binding them
 * to names in the global environment. Many of the most basic objects are
 * referenced frequently during system initialization and we'd like to defer the
 * implementation of hash tables to lisp, so we cache the basic objects in one
 * flat record.
 */

typedef struct {
  BORAX_RECORD    Record;
  // Basic metaobjects
  BORAX_OBJECT    StandardClass;
  BORAX_OBJECT    ClassT;
  BORAX_OBJECT    Class;
  BORAX_OBJECT    BuiltInClass;
  BORAX_OBJECT    StandardObject;
  BORAX_OBJECT    SlotDefinition;
  // Primitive types
  BORAX_OBJECT    Fixnum;
  BORAX_OBJECT    Character;
  BORAX_OBJECT    Cons;
  BORAX_OBJECT    WeakPointer;
  BORAX_OBJECT    Pin;
  // Vector types
  BORAX_OBJECT    WordRecordClass;
  BORAX_OBJECT    SimpleVector;
  BORAX_OBJECT    SimpleVectorUnsignedByte8;
  BORAX_OBJECT    SimpleString;
  // Hash tables
  BORAX_OBJECT    HashTable;
  // Symbols and packages
  BORAX_OBJECT    Symbol;
  BORAX_OBJECT    Package;
  BORAX_OBJECT    CommonLisp;
  BORAX_OBJECT    BoraxRuntime;
  BORAX_OBJECT    Nil;
  BORAX_OBJECT    T;
  BORAX_OBJECT    Equal;
} BORAX_ENVIRONMENT_CACHE;

struct _BORAX_GLOBAL_ENVIRONMENT {
  BORAX_RECORD    Record;
  BORAX_OBJECT    Cache;
  BORAX_OBJECT    PackageRegistry;
  BORAX_OBJECT    ClassRegistry;
};

EFI_STATUS
EFIAPI
BoraxBootstrapGlobalEnvironment (
  IN BORAX_ALLOCATOR  *Alloc,
  OUT BORAX_PIN       **GlobalEnvironment
  );

#endif // BORAX_GLOBAL_ENVIRONMENT_H
