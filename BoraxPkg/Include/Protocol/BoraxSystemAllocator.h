#ifndef BORAX_SYSTEM_ALLOCATOR_H
#define BORAX_SYSTEM_ALLOCATOR_H

#include <Uefi.h>

typedef struct _BORAX_SYSTEM_ALLOCATOR_PROTOCOL BORAX_SYSTEM_ALLOCATOR_PROTOCOL;

typedef
VOID *
(EFIAPI *BORAX_ALLOCATE_PAGES)(
  IN BORAX_SYSTEM_ALLOCATOR_PROTOCOL *This,
  IN UINTN Pages
  );

typedef
VOID
(EFIAPI *BORAX_FREE_PAGES)(
  IN BORAX_SYSTEM_ALLOCATOR_PROTOCOL *This,
  IN VOID   *Buffer,
  IN UINTN  Pages
  );

typedef
VOID *
(EFIAPI *BORAX_ALLOCATE_POOL)(
  IN BORAX_SYSTEM_ALLOCATOR_PROTOCOL *This,
  IN UINTN  AllocationSize
  );

typedef
VOID
(EFIAPI *BORAX_FREE_POOL)(
  IN BORAX_SYSTEM_ALLOCATOR_PROTOCOL *This,
  IN VOID  *Buffer
  );

struct _BORAX_SYSTEM_ALLOCATOR_PROTOCOL {
  BORAX_ALLOCATE_PAGES    AllocatePages;
  BORAX_FREE_PAGES        FreePages;
  BORAX_ALLOCATE_POOL     AllocatePool;
  BORAX_FREE_POOL         FreePool;
};

#endif // BORAX_SYSTEM_ALLOCATOR_H
