#ifndef BORAX_STACK_H
#define BORAX_STACK_H

#include <Library/BoraxMemory.h>

typedef struct _BORAX_STACK_PAGE BORAX_STACK_PAGE;

struct _BORAX_STACK_PAGE {
  BORAX_STACK_PAGE    *Next;
  UINTN               FillIndex;    // Array index, not byte offset
  UINTN               Data[];
};

typedef struct {
  BORAX_SYSTEM_ALLOCATOR_PROTOCOL    *SysAlloc;
  BORAX_STACK_PAGE                   *Top;
} BORAX_STACK;

VOID
EFIAPI
BoraxStackInit (
  OUT BORAX_STACK                     *Stack,
  IN BORAX_SYSTEM_ALLOCATOR_PROTOCOL  *SysAlloc
  );

VOID
EFIAPI
BoraxStackCleanup (
  IN BORAX_STACK  *Stack
  );

EFI_STATUS
EFIAPI
BoraxStackPush (
  IN BORAX_STACK  *Stack,
  IN UINTN        Word
  );

BOOLEAN
EFIAPI
BoraxStackPop (
  IN BORAX_STACK  *Stack,
  OUT UINTN       *Word
  );

#endif // BORAX_STACK_H
