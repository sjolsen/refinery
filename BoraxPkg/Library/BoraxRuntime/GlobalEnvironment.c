#include <Library/BoraxGlobalEnvironment.h>

STATIC INLINE
EFI_STATUS
EFIAPI
EarlyAllocateStandardClass (
  IN BORAX_ALLOCATOR          *Alloc,
  IN BORAX_ENVIRONMENT_CACHE  *Cache,
  OUT BORAX_OBJECT            *Object
  )
{
  EFI_STATUS    Status;
  BORAX_RECORD  *Record;

  Status = BoraxAllocateRecord (
             Alloc,
             BORAX_WIDETAG_OBJECT_RECORD,
             Cache.StandardClass,
             BORAX_RECORD_LENGTH (BORAX_STANDARD_CLASS),
             0,
             BORAX_IMMEDIATE_UNBOUND,
             &Record
             );
  if (EFI_ERROR (Status)) {
    return Status;
  }

  *Object = BORAX_MAKE_POINTER (Record);
  return EFI_SUCCESS;
}

STATIC INLINE
EFI_STATUS
EFIAPI
EarlyAllocateBuiltInClass (
  IN BORAX_ALLOCATOR          *Alloc,
  IN BORAX_ENVIRONMENT_CACHE  *Cache,
  OUT BORAX_OBJECT            *Object
  )
{
  EFI_STATUS    Status;
  BORAX_RECORD  *Record;

  Status = BoraxAllocateRecord (
             Alloc,
             BORAX_WIDETAG_OBJECT_RECORD,
             Cache.BuiltInClass,
             RECORD_LENGTH (BORAX_BUILT_IN_CLASS),
             0,
             BORAX_IMMEDIATE_UNBOUND,
             &Record
             );
  if (EFI_ERROR (Status)) {
    return Status;
  }

  *Object = BORAX_MAKE_POINTER (Record);
  return EFI_SUCCESS;
}

STATIC INLINE
EFI_STATUS
EFIAPI
EarlyAllocateWordRecordClass (
  IN BORAX_ALLOCATOR          *Alloc,
  IN BORAX_ENVIRONMENT_CACHE  *Cache,
  IN BORAX_OBJECT             ElementClass,
  IN UINTN                    ElementBits,
  OUT BORAX_OBJECT            *Object
  )
{
  EFI_STATUS               Status;
  BORAX_WORD_RECORD_CLASS  *Class;

  Status = BoraxAllocateRecord (
             Alloc,
             BORAX_WIDETAG_OBJECT_RECORD,
             Cache.WordRecordClass,
             RECORD_LENGTH (BORAX_WORD_RECORD_CLASS),
             0,
             BORAX_IMMEDIATE_UNBOUND,
             (BORAX_RECORD **)&Class
             );
  if (EFI_ERROR (Status)) {
    return Status;
  }

  Class->ElementClass = ElementClass;
  Class->ElementBits  = BORAX_MAKE_FIXNUM (ElementBits);

  *Object = BORAX_MAKE_POINTER (Class);
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
  BORAX_OBJECT              String;

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

  // Allocate the class objects
  TRY (EarlyAllocateStandardClass (Alloc, Cache, &Cache->StandardClass));
  {
    BORAX_STANDARD_CLASS  *StandardClass =
      (BORAX_STANDARD_CLASS *)BORAX_GET_POINTER (Cache->StandardClass);

    StandardClass->Record.Class = Cache->StandardClass;  // was unbound
  }

  TRY (EarlyAllocateStandardClass (Alloc, Cache, &Cache->ClassT));
  TRY (EarlyAllocateStandardClass (Alloc, Cache, &Cache->Class));
  TRY (EarlyAllocateStandardClass (Alloc, Cache, &Cache->BuiltInClass));
  TRY (EarlyAllocateStandardClass (Alloc, Cache, &Cache->StandardObject));
  TRY (EarlyAllocateStandardClass (Alloc, Cache, &Cache->SlotDefinition));
  TRY (EarlyAllocateBuiltInClass (Alloc, Cache, &Cache->Fixnum));
  TRY (EarlyAllocateBuiltInClass (Alloc, Cache, &Cache->Character));
  TRY (EarlyAllocateBuiltInClass (Alloc, Cache, &Cache->Cons));
  TRY (EarlyAllocateBuiltInClass (Alloc, Cache, &Cache->WeakPointer));
  TRY (EarlyAllocateBuiltInClass (Alloc, Cache, &Cache->Pin));
  TRY (EarlyAllocateStandardClass (Alloc, Cache, &Cache->WordRecordClass));
  TRY (EarlyAllocateBuiltInClass (Alloc, Cache, &Cache->SimpleVector));
  TRY (
    EarlyAllocateWordRecordClass (
      Alloc,
      Cache,
      Cache->Fixnum,
      8,
      &Cache->SimpleVectorUnsignedByte8
      )
    );
  TRY (
    EarlyAllocateWordRecordClass (
      Alloc,
      Cache,
      Cache->Character,
      16,
      &Cache->SimpleString
      )
    );
  TRY (EarlyAllocateStandardClass (Alloc, Cache, &Cache->HashTable));
  TRY (EarlyAllocateStandardClass (Alloc, Cache, &Cache->Symbol));
  TRY (EarlyAllocateStandardClass (Alloc, Cache, &Cache->Package));

  // With the identities of the class objects established we can now start
  // calling out to normal runtime libraries, but we still need to be mindful of
  // unbound slots.
  TRY (EarlyAllocateSymbol (Alloc, Env, &Cache->Nil));
  TRY (EarlyAllocateSymbol (Alloc, Env, &Cache->T));
  TRY (EarlyAllocateSymbol (Alloc, Env, &Cache->Equal));

  TRY (BoraxMakeHashTable (Alloc, Env, Cache->Equal, &Env->PackageRegistry));
  TRY (BoraxMakeHashTable (Alloc, Env, Cache->Equal, &Env->ClassRegistry));

  TRY (BoraxMakeString (Alloc, Env, L"COMMON-LISP", &String));
  TRY (BoraxMakePackage (Alloc, Env, String, &Cache->CommonLisp));
  TRY (BoraxMakeString (Alloc, Env, L"BORAX-RUNTIME", &String));
  TRY (BoraxMakePackage (Alloc, Env, String, &Cache->BoraxRuntime));

  #undef TRY

  return BoraxAllocatePin (Alloc, BORAX_MAKE_POINTER (Env), GlobalEnvironment);
}
