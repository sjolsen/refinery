#ifndef BORAX_GREY_LIST_H
#define BORAX_GREY_LIST_H

typedef struct _BORAX_GREY_PAGE BORAX_GREY_PAGE;

struct _BORAX_GREY_PAGE {
  BORAX_GREY_PAGE    *Next;
  UINTN              FillIndex; // Array index, not byte offset
  BORAX_OBJECT       Objects[];
};

typedef struct {
  BORAX_SYSTEM_ALLOCATOR_PROTOCOL    *SysAlloc;
  BORAX_GREY_PAGE                    *Top;
} BORAX_GREY_LIST;

VOID
EFIAPI
BoraxGreyListInit (
  OUT BORAX_GREY_LIST                 *GreyList,
  IN BORAX_SYSTEM_ALLOCATOR_PROTOCOL  *SysAlloc
  );

VOID
EFIAPI
BoraxGreyListCleanup (
  IN BORAX_GREY_LIST  *GreyList
  );

EFI_STATUS
EFIAPI
BoraxGreyListPush (
  IN BORAX_GREY_LIST  *GreyList,
  IN BORAX_OBJECT     Object
  );

BOOLEAN
EFIAPI
BoraxGreyListPop (
  IN BORAX_GREY_LIST  *GreyList,
  OUT BORAX_OBJECT    *Object
  );

#endif // BORAX_GREY_LIST_H
