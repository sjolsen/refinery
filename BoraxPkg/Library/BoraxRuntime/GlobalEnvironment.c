#include <Library/BoraxGlobalEnvironment.h>

STATIC INLINE
EFI_STATUS
EFIAPI
EarlyAllocateStandardClass (
  IN BORAX_ALLOCATOR          *Alloc,
  IN BORAX_ENVIRONMENT_CACHE  *Cache,
  OUT BORAX_STANDARD_CLASS    **Class,
  OUT BORAX_OBJECT            *Object
  )
{
  EFI_STATUS  Status;

  Status = BoraxAllocateRecord (
             Alloc,
             BORAX_WIDETAG_OBJECT_RECORD,
             Cache.StandardClass,
             BORAX_RECORD_LENGTH (BORAX_STANDARD_CLASS),
             0,
             BORAX_IMMEDIATE_UNBOUND,
             (BORAX_RECORD **)Class
             );
  if (EFI_ERROR (Status)) {
    return Status;
  }

  *Object = BORAX_MAKE_POINTER (*Class);
  return EFI_SUCCESS;
}

STATIC INLINE
EFI_STATUS
EFIAPI
EarlyAllocateBuiltInClass (
  IN BORAX_ALLOCATOR          *Alloc,
  IN BORAX_ENVIRONMENT_CACHE  *Cache,
  OUT BORAX_BUILT_IN_CLASS    **Class,
  OUT BORAX_OBJECT            *Object
  )
{
  EFI_STATUS  Status;

  Status = BoraxAllocateRecord (
             Alloc,
             BORAX_WIDETAG_OBJECT_RECORD,
             Cache.BuiltInClass,
             RECORD_LENGTH (BORAX_BUILT_IN_CLASS),
             0,
             BORAX_IMMEDIATE_UNBOUND,
             (BORAX_RECORD **)Class
             );
  if (EFI_ERROR (Status)) {
    return Status;
  }

  *Object = BORAX_MAKE_POINTER (*Class);
  return EFI_SUCCESS;
}

STATIC INLINE
EFI_STATUS
EFIAPI
EarlyAllocateWordRecordClass (
  IN BORAX_ALLOCATOR           *Alloc,
  IN BORAX_ENVIRONMENT_CACHE   *Cache,
  IN BORAX_OBJECT              ElementClass,
  IN UINTN                     ElementBits,
  OUT BORAX_WORD_RECORD_CLASS  **Class,
  OUT BORAX_OBJECT             *Object
  )
{
  EFI_STATUS               Status;
  BORAX_WORD_RECORD_CLASS  *NewClass;

  Status = BoraxAllocateRecord (
             Alloc,
             BORAX_WIDETAG_OBJECT_RECORD,
             Cache.WordRecordClass,
             RECORD_LENGTH (BORAX_WORD_RECORD_CLASS),
             0,
             BORAX_IMMEDIATE_UNBOUND,
             (BORAX_RECORD **)&NewClass
             );
  if (EFI_ERROR (Status)) {
    return Status;
  }

  NewClass->ElementClass = ElementClass;
  NewClass->ElementBits  = BORAX_MAKE_FIXNUM (ElementBits);

  *Class  = NewClass;
  *Object = BORAX_MAKE_POINTER (NewClass);
  return EFI_SUCCESS;
}

EFI_STATUS
EFIAPI
BoraxBootstrapGlobalEnvironment (
  IN BORAX_ALLOCATOR  *Alloc,
  OUT BORAX_PIN       **GlobalEnvironment
  )
{
  EFI_STATUS                Status;
  BORAX_GLOBAL_ENVIRONMENT  *Env;
  BORAX_ENVIRONMENT_CACHE   *Cache;
  BORAX_STANDARD_CLASS      *StandardClass;
  BORAX_STANDARD_CLASS      *ClassT;
  BORAX_STANDARD_CLASS      *Class;
  BORAX_STANDARD_CLASS      *BuiltInClass;
  BORAX_STANDARD_CLASS      *StandardObject;
  BORAX_STANDARD_CLASS      *SlotDefinition;
  BORAX_BUILT_IN_CLASS      *Fixnum;
  BORAX_BUILT_IN_CLASS      *Character;
  BORAX_BUILT_IN_CLASS      *Cons;
  BORAX_BUILT_IN_CLASS      *WeakPointer;
  BORAX_BUILT_IN_CLASS      *Pin;
  BORAX_STANDARD_CLASS      *WordRecordClass;
  BORAX_BUILT_IN_CLASS      *SimpleVector;
  BORAX_WORD_RECORD_CLASS   *SimpleVectorUnsignedByte8;
  BORAX_WORD_RECORD_CLASS   *SimpleString;
  BORAX_STANDARD_CLASS      *HashTable;
  BORAX_STANDARD_CLASS      *Symbol;
  BORAX_STANDARD_CLASS      *Package;
  BORAX_PACKAGE             *CommonLisp;
  BORAX_PACKAGE             *BoraxRuntime;
  BORAX_SYMBOL              *Nil;
  BORAX_SYMBOL              *T;
  BORAX_SYMBOL              *Equal;

  #define TRY(_expr)  do {    \
    Status = (_expr);         \
    if (EFI_ERROR (Status)) { \
      return Status;          \
    }                         \
  } while (0)

  // Allocate the global environment
  TRY (
    BoraxAllocateRecord (
      Alloc,
      OBJECT_RECORD,
      BORAX_IMMEDIATE_UNBOUND,
      RECORD_LENGTH (BORAX_GLOBAL_ENVIRONMENT),
      0,
      (BORAX_RECORD **)&Env
      )
    );
  TRY (BoraxAllocatePin (Alloc, BORAX_MAKE_POINTER (Env), GlobalEnvironment));

  // Allocate the environment cache
  TRY (
    BoraxAllocateRecord (
      Alloc,
      OBJECT_RECORD,
      BORAX_IMMEDIATE_UNBOUND,
      RECORD_LENGTH (BORAX_ENVIRONMENT_CACHE),
      0,
      (BORAX_RECORD **)&Cache
      )
    );
  Env->Cache = BORAX_MAKE_POINTER (Cache);

  // Allocate the standard classes
  #define MAKE_STANDARD_CLASS(_ident) \
  TRY (EarlyAllocateStandardClass (Alloc, Cache, &_ident, &Cache->_ident))

  MAKE_STANDARD_CLASS (StandardClass);
  StandardClass->Record.Class = Cache->StandardClass;  // was unbound
  MAKE_STANDARD_CLASS (ClassT);
  MAKE_STANDARD_CLASS (Class);
  MAKE_STANDARD_CLASS (BuiltInClass);
  MAKE_STANDARD_CLASS (StandardObject);
  MAKE_STANDARD_CLASS (SlotDefinition);
  MAKE_STANDARD_CLASS (WordRecordClass);
  MAKE_STANDARD_CLASS (HashTable);
  MAKE_STANDARD_CLASS (Symbol);
  MAKE_STANDARD_CLASS (Package);

  // Allocate the built-in classes
  #define MAKE_BUILT_IN_CLASS(_ident) \
  TRY (EarlyAllocateBuiltInClass (Alloc, Cache, &_ident, &Cache->_ident))

  MAKE_BUILT_IN_CLASS (Fixnum);
  MAKE_BUILT_IN_CLASS (Character);
  MAKE_BUILT_IN_CLASS (Cons);
  MAKE_BUILT_IN_CLASS (WeakPointer);
  MAKE_BUILT_IN_CLASS (Pin);
  MAKE_BUILT_IN_CLASS (SimpleVector);

  // Allocate the word record classes
  #define MAKE_WORD_RECORD_CLASS(_ident, _class, _bits) \
  TRY (EarlyAllocateWordRecordClass (Alloc, Cache, _class, _bits, &_ident, &Cache->_ident))

  MAKE_WORD_RECORD_CLASS (SimpleVectorUnsignedByte8, Cache->Fixnum, 8);
  MAKE_WORD_RECORD_CLASS (SimpleString, Cache->Character, 16);

  // Allocate the basic symbols
  #define MAKE_SYMBOL(_ident, _name) \
  TRY (EarlyAllocateSymbol (Alloc, Cache, _name, &_ident, &Cache->_ident))

  MAKE_SYMBOL (Nil, L"NIL");
  MAKE_SYMBOL (T, L"T");
  MAKE_SYMBOL (Equal, L"NIL");

  // The packages and global registries are still unallocated and most slots of
  // most objects remain unbound. We have now bootstrapped enough of the global
  // environment to start calling out to normal runtime libraries to fix this.

  #undef TRY

  return EFI_SUCCESS;
}
