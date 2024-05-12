#include "Stack.h"

VOID
EFIAPI
BoraxStackInit (
  OUT BORAX_STACK                     *Stack,
  IN BORAX_SYSTEM_ALLOCATOR_PROTOCOL  *SysAlloc
  )
{
  Stack->SysAlloc = SysAlloc;
  Stack->Top      = NULL;
}

VOID
EFIAPI
BoraxStackCleanup (
  IN BORAX_STACK  *Stack
  )
{
  BORAX_STACK_PAGE  *Page;

  Page = Stack->Top;
  while (Page != NULL) {
    BORAX_STACK_PAGE  *Next = Page->Next;
    Stack->SysAlloc->FreePages (Stack->SysAlloc, Page, 1);
    Page = Next;
  }
}

#define MAX_FILL \
((BORAX_PAGE_SIZE - OFFSET_OF (BORAX_STACK_PAGE, Data)) / sizeof (UINTN))

STATIC_ASSERT (
  OFFSET_OF (BORAX_STACK_PAGE, Data[MAX_FILL]) <= BORAX_PAGE_SIZE,
  "We calculated MAX_FILL incorrectly"
  );

EFI_STATUS
EFIAPI
BoraxStackPush (
  IN BORAX_STACK  *Stack,
  IN UINTN        Word
  )
{
  if ((Stack->Top == NULL) || (Stack->Top->FillIndex == MAX_FILL)) {
    BORAX_STACK_PAGE  *Page;

    Page = Stack->SysAlloc->AllocatePages (Stack->SysAlloc, 1);
    if (Page == NULL) {
      return EFI_OUT_OF_RESOURCES;
    }

    Page->Next      = Stack->Top;
    Page->FillIndex = 0;
    Stack->Top      = Page;
  }

  Stack->Top->Data[Stack->Top->FillIndex++] = Word;
  return EFI_SUCCESS;
}

BOOLEAN
EFIAPI
BoraxStackPop (
  IN BORAX_STACK  *Stack,
  OUT UINTN       *Word
  )
{
  if (Stack->Top == NULL) {
    return FALSE;
  }

  *Word = Stack->Top->Data[--Stack->Top->FillIndex];
  if (Stack->Top->FillIndex == 0) {
    BORAX_STACK_PAGE  *Page = Stack->Top;
    Stack->Top = Page->Next;
    Stack->SysAlloc->FreePages (Stack->SysAlloc, Page, 1);
  }

  return TRUE;
}
