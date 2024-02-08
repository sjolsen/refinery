#include "GreyList.h"

VOID
EFIAPI
BoraxGreyListInit (
  OUT BORAX_GREY_LIST                 *GreyList,
  IN BORAX_SYSTEM_ALLOCATOR_PROTOCOL  *SysAlloc
  )
{
  GreyList->SysAlloc = SysAlloc;
  GreyList->Top      = NULL;
}

VOID
EFIAPI
BoraxGreyListCleanup (
  IN BORAX_GREY_LIST  *GreyList
  )
{
  BORAX_GREY_PAGE  *Page;

  Page = GreyList->Top;
  while (Page != NULL) {
    BORAX_GREY_PAGE  *Next = Page->Next;
    GreyList->SysAlloc->FreePages (GreyList->SysAlloc, Page, 1);
    Page = Next;
  }
}

#define MAX_FILL \
((BORAX_PAGE_SIZE - OFFSET_OF (BORAX_GREY_PAGE, Objects)) \
 / sizeof (BORAX_OBJECT_HEADER *))

STATIC_ASSERT (
  OFFSET_OF (BORAX_GREY_PAGE, Objects[MAX_FILL]) <= BORAX_PAGE_SIZE,
  "We calculated MAX_FILL incorrectly"
  );

EFI_STATUS
EFIAPI
BoraxGreyListPush (
  IN BORAX_GREY_LIST      *GreyList,
  IN BORAX_OBJECT_HEADER  *Object
  )
{
  if ((GreyList->Top == NULL) || (GreyList->Top->FillIndex == MAX_FILL)) {
    BORAX_GREY_PAGE  *Page;

    Page = GreyList->SysAlloc->AllocatePages (GreyList->SysAlloc, 1);
    if (Page == NULL) {
      return EFI_OUT_OF_RESOURCES;
    }

    Page->Next      = GreyList->Top;
    Page->FillIndex = 0;
    GreyList->Top   = Page;
  }

  GreyList->Top->Objects[GreyList->Top->FillIndex++] = Object;
  return EFI_SUCCESS;
}

BOOLEAN
EFIAPI
BoraxGreyListPop (
  IN BORAX_GREY_LIST       *GreyList,
  OUT BORAX_OBJECT_HEADER  **Object
  )
{
  if (GreyList->Top == NULL) {
    return FALSE;
  }

  *Object = GreyList->Top->Objects[--GreyList->Top->FillIndex];
  if (GreyList->Top->FillIndex == 0) {
    BORAX_GREY_PAGE  *Page = GreyList->Top;
    GreyList->Top = Page->Next;
    GreyList->SysAlloc->FreePages (GreyList->SysAlloc, Page, 1);
  }

  return TRUE;
}
