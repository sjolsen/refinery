#include <Library/BoraxSystemAllocator.h>

#include <Library/MemoryAllocationLib.h>

STATIC VOID *
EFIAPI
SysAllocAllocatePages (
  IN BORAX_SYSTEM_ALLOCATOR_PROTOCOL  *This,
  IN UINTN                            Pages
  )
{
  (VOID)This;
  return AllocatePages (Pages);
}

STATIC VOID
EFIAPI
SysAllocFreePages (
  IN BORAX_SYSTEM_ALLOCATOR_PROTOCOL  *This,
  IN VOID                             *Buffer,
  IN UINTN                            Pages
  )
{
  (VOID)This;
  return FreePages (Buffer, Pages);
}

STATIC VOID *
EFIAPI
SysAllocAllocatePool (
  IN BORAX_SYSTEM_ALLOCATOR_PROTOCOL  *This,
  IN UINTN                            AllocationSize
  )
{
  (VOID)This;
  return AllocatePool (AllocationSize);
}

STATIC VOID
EFIAPI
SysAllocFreePool (
  IN BORAX_SYSTEM_ALLOCATOR_PROTOCOL  *This,
  IN VOID                             *Buffer
  )
{
  (VOID)This;
  return FreePool (Buffer);
}

BORAX_SYSTEM_ALLOCATOR_PROTOCOL  gSystemAllocator = {
  .AllocatePages = &SysAllocAllocatePages,
  .FreePages     = &SysAllocFreePages,
  .AllocatePool  = &SysAllocAllocatePool,
  .FreePool      = &SysAllocFreePool,
};
