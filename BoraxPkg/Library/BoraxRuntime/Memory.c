#include "Memory.h"

STATIC VOID *
EFIAPI
FFAllocatePages (
  IN BORAX_ALLOCATOR  *Alloc,
  IN UINTN            Pages
  )
{
  VOID  *Pages;

  Pages = Alloc->SysAlloc->AllocatePages (Alloc->SysAlloc, Pages);
  if (Pages == NULL) {
    return NULL;
  }

  SetMem (Pages, BORAX_PAGE_SIZE * Pages, -1);
  return Pages;
}

STATIC VOID *
EFIAPI
FFAllocatePool (
  IN BORAX_ALLOCATOR  *Alloc,
  IN UINTN            AllocationSize
  )
{
  VOID  *Mem;

  Mem = Alloc->SysAlloc->AllocatePool (Alloc->SysAlloc, AllocationSize);
  if (Mem == NULL) {
    return NULL;
  }

  SetMem (Mem, AllocationSize, -1);
  return Mem;
}

VOID
EFIAPI
BoraxAllocatorInit (
  OUT BORAX_ALLOCATOR                 *Alloc,
  IN BORAX_SYSTEM_ALLOCATOR_PROTOCOL  *SysAlloc
  )
{
  SetMem (Alloc, sizeof (*Alloc), 0);
  Alloc->Pins.Next = &Alloc->Pins;
  Alloc->Pins.Prev = &Alloc->Pins;
  Alloc->SysAlloc  = SysAlloc;
}

STATIC VOID
EFIAPI
ClearSpace (
  IN BORAX_ALLOCATOR   *Alloc,
  IN BORAX_COPY_SPACE  *Space
  )
{
  BORAX_CONS_PAGE     *ConsPage;
  BORAX_OBJECT_CHUNK  *ObjChunk;
  UINTN               Bin;

  // Free cons pages
  ConsPage = Space->Cons.Pages;
  while (ConsPage != NULL) {
    BORAX_CONS_PAGE  *Next = ConsPage->Next;
    Alloc->SysAlloc->FreePages (
                       Alloc->SysAlloc,
                       ConsPage,
                       1
                       );
    ConsPage = Next;
  }

  // Free object page chunks
  for (Bin = 0; Bin < BORAX_ALLOC_BIN_COUNT; ++Bin) {
    ObjChunk = Space->Object.Chunks[Bin];
    while (ObjChunk != NULL) {
      BORAX_OBJECT_CHUNK  *Next = ObjChunk->Next;
      Alloc->SysAlloc->FreePages (
                         Alloc->SysAlloc,
                         ObjChunk,
                         ObjChunk->Pages
                         );
      ObjChunk = Next;
    }
  }
}

VOID
EFIAPI
BoraxAllocatorCleanup (
  IN BORAX_ALLOCATOR  *Alloc
  )
{
  BORAX_PIN_LIST  *PinEntry;

  // We may have aborted in the middle of a cycle, so clean both spaces
  ClearSpace (Alloc, &Alloc->FromSpace);
  ClearSpace (Alloc, &Alloc->ToSpace);

  // Free pin objects
  PinEntry = Alloc->Pins.Next;
  while (PinEntry != &Alloc->Pins) {
    BORAX_PIN_LIST  *Next = PinEntry->Next;
    BORAX_PIN       *Pin  = BASE_CR (PinEntry, BORAX_PIN, ListEntry);
    Alloc->SysAlloc->FreePool (Alloc->SysAlloc, Pin);
    PinEntry = Next;
  }

  // Weak pointers were stored in the object page chunks
}

#define PAGE_ADDRESS(_addr) \
((UINTN)(_addr) & ~(BORAX_PAGE_SIZE - 1))

#define CONS_PAGE(_addr) \
((BORAX_CONS_PAGE *) PAGE_ADDRESS(_addr))

#define CONS_PAGE_OFFSET(_addr) \
((UINTN)(_addr) & (BORAX_PAGE_SIZE - 1))

#define CONS_BITMAP_INDEX(_addr) \
(CONS_PAGE_OFFSET(_addr) / sizeof (BORAX_CONS))

#define CONS_BITMAP_WORD(_addr) \
(CONS_BITMAP_INDEX(_addr) / sizeof (BORAX_WORD_BITS))

#define CONS_BITMAP_BIT(_addr) \
(CONS_BITMAP_INDEX(_addr) % sizeof (BORAX_WORD_BITS))

STATIC BOOLEAN
EFIAPI
ConsGreyBitmapGet (
  IN CONST BORAX_CONS  *Cons
  )
{
  BORAX_CONS_PAGE  *Page = CONS_PAGE (Cons);
  UINTN            Word  = CONS_BITMAP_WORD (Cons);
  UINTN            Bit   = CONS_BITMAP_BIT (Cons);

  return Page->GreyBitmap[Word] & (1 << Bit);
}

STATIC VOID
EFIAPI
ConsGreyBitmapSet (
  IN CONST BORAX_CONS  *Cons,
  IN BOOLEAN           Value
  )
{
  BORAX_CONS_PAGE  *Page = CONS_PAGE (Cons);
  UINTN            Word  = CONS_BITMAP_WORD (Cons);
  UINTN            Bit   = CONS_BITMAP_BIT (Cons);

  if (Value) {
    Page->GreyBitmap[Word] |= (1 << Bit);
  } else {
    Page->GreyBitmap[Word] &= ~(1 << Bit);
  }
}

STATIC EFI_STATUS
EFIAPI
GetObjectColor (
  IN BORAX_ALLOCATOR       *Alloc,
  IN BORAX_OBJECT          Object,
  OUT BORAX_OBJECT_GCDATA  *GcData
  )
{
  if (BORAX_IS_FIXNUM (Object)) {
    // Nothing to do for fixnums, so bypass processing by reporting black
    *GcData = BORAX_OBJECT_GCDATA_BLACK;
    return EFI_SUCCESS;
  } else if (BORAX_IS_POINTER (Object)) {
    BORAX_OBJECT_HEADER  *Obj = BORAX_GET_POINTER (Object);
    if (BORAX_IS_CONS (Obj->HeaderWords[0])) {
      BORAX_CONS  *Cons = (BORAX_CONS *)Obj;

      if (ConsGreyBitmapGet (Cons)) {
        *GcData = BORAX_OBJECT_GCDATA_GREY;
      } else if (Page->SpaceParity == Alloc->ToSpaceParity) {
        *GcData = BORAX_OBJECT_GCDATA_BLACK;
      } else {
        *GcData = BORAX_OBJECT_GCDATA_WHITE;
      }

      return EFI_SUCCESS;
    } else {
      switch (Obj->WideTag) {
        case BORAX_WIDETAG_WEAK_POINTER:
        case BORAX_WIDETAG_PIN:
        case BORAX_WIDETAG_MOVED:
          *GcData = Obj->GcData;
          return EFI_SUCCESS;
        case BORAX_WIDETAG_UNINITIALIZED:
          // The GcData field in this case is unlikely to be initialized. It
          // also doesn't really matter what we return, but for consistency with
          // fixnums, return black.
          *GcData = BORAX_OBJECT_GCDATA_BLACK;
          return EFI_SUCCESS;
        default:
          // We should never see this case
          return EFI_INVALID_PARAMETER;
      }
    }
  } else {
    // We should never see this case
    return EFI_INVALID_PARAMETER;
  }
}

STATIC EFI_STATUS
EFIAPI
MarkObjectIfWhite (
  IN BORAX_ALLOCATOR  *Alloc,
  IN BORAX_GREY_LIST  *GreyList,
  IN BORAX_OBJECT     Object
  )
{
  EFI_STATUS           Status;
  BORAX_OBJECT_GCDATA  GcData;

  Status = GetObjectColor (Alloc, Object, &GcData);
  if (EFI_ERROR (Status)) {
    return Status;
  }

  if (GcData != BORAX_OBJECT_GCDATA_WHITE) {
    return EFI_SUCCESS;
  }

  if (BORAX_IS_FIXNUM (Object)) {
    // Nothing to do for fixnums
    return EFI_SUCCESS;
  } else if (BORAX_IS_POINTER (Object)) {
    BORAX_OBJECT_HEADER  *OldObj = BORAX_GET_POINTER (Object);
    BORAX_OBJECT_HEADER  *NewObj;

    if (BORAX_IS_CONS (OldObj->HeaderWords[0])) {
      Status = BoraxAllocateCons (Alloc, &NewObj);
      if (EFI_ERROR (Status)) {
        return Status;
      }

      // The mark on the old cons cell is not strictly necessary since future
      // reads will see the moved widetag.
      CopyMem (NewObj, OldObj, sizeof (BORAX_CONS));
      ConsGreyBitmapSet ((BORAX_CONS *)OldObj, 1);
      ConsGreyBitmapSet ((BORAX_CONS *)NewObj, 1);
      OldObj->WideTag        = BORAX_WIDETAG_MOVED;
      OldObj->GcData         = BORAX_OBJECT_GCDATA_GREY;
      OldObj->HeaderWords[1] = (UINTN)NewObj;
    } else {
      switch (OldObj->WideTag) {
        case BORAX_WIDETAG_WEAK_POINTER:
        {
          UINTN  Size = sizeof (BORAX_WEAK_POINTER);

          Status = BoraxAllocateObject (Alloc, Size, &NewObj);
          if (EFI_ERROR (Status)) {
            return Status;
          }

          CopyMem (NewObj, OldObj, Size);
          NewObj->GcData         = BORAX_OBJECT_GCDATA_GREY;
          OldObj->WideTag        = BORAX_WIDETAG_MOVED;
          OldObj->GcData         = BORAX_OBJECT_GCDATA_GREY;
          OldObj->HeaderWords[1] = (UINTN)NewObj;
          break;
        }
        case BORAX_WIDETAG_PIN:
          // Don't move pins
          OldObj->GcData = BORAX_OBJECT_GCDATA_GREY;
          break;
        case BORAX_WIDETAG_MOVED:
          // We've already processed this object
          break;
        case BORAX_WIDETAG_UNINITIALIZED:
          // Not an object
          break;
        default:
          // We should never see this case
          return EFI_INVALID_PARAMETER;
      }
    }
  } else {
    // We should never see this case
    return EFI_INVALID_PARAMETER;
  }

  Status = BoraxGreyListPush (&GreyList, Object);
  return Status;
}

STATIC EFI_STATUS
EFIAPI
MarkSubObjectsIfWhite (
  IN BORAX_ALLOCATOR  *Alloc,
  IN BORAX_GREY_LIST  *GreyList,
  IN BORAX_OBJECT     Object
  )
{
  if (BORAX_IS_FIXNUM (Object)) {
    // Nothing to do for fixnums
    return EFI_SUCCESS;
  } else if (BORAX_IS_POINTER (Object)) {
    BORAX_OBJECT_HEADER  *Obj = BORAX_GET_POINTER (Object);
    if (BORAX_IS_CONS (Obj->HeaderWords[0])) {
      BORAX_CONS  *Cons = (BORAX_CONS *)Obj;

      Status = MarkObjectIfWhite (Alloc, &GreyList, Cons->Car);
      if (EFI_ERROR (Status)) {
        return Status;
      }

      // Mark CDR last to ensure it gets copied first
      Status = MarkObjectIfWhite (Alloc, &GreyList, Cons->Cdr);
      return status;
    } else {
      switch (Obj->WideTag) {
        case BORAX_WIDETAG_PIN:
        {
          BORAX_PIN  *Pin = (BORAX_PIN *)Obj;
          Status = MarkObjectIfWhite (Alloc, &GreyList, Pin->Object);
          return status;
        }
        case BORAX_WIDETAG_WEAK_POINTER:
        case BORAX_WIDETAG_MOVED:
        case BORAX_WIDETAG_UNINITIALIZED:
          // Nothing to do
          return EFI_SUCCESS;
        default:
          // We should never see this case
          return EFI_INVALID_PARAMETER;
      }
    }
  } else {
    // We should never see this case
    return EFI_INVALID_PARAMETER;
  }
}

EFI_STATUS
EFIAPI
BoraxAllocatorCollect (
  IN BORAX_ALLOCATOR               *Alloc,
  IN CONST BORAX_ROOTSET_ITERATOR  *RootSet
  )
{
  EFI_STATUS       Status;
  BORAX_GREY_LIST  GreyList;
  BORAX_OBJECT     Object;

  // Begin by flipping spaces
  Alloc->FromSpace = Alloc->ToSpace;
  SetMem (Alloc->ToSpace, sizeof (Alloc->ToSpace), 0);
  Alloc->ToSpaceParity = !Alloc->ToSpaceParity;

  // Mark the initial set of root objects grey
  BoraxGreyListInit (&GreyList, Alloc->SysAlloc);
  while (RootSet->Next (RootSet->Ctx, &Object)) {
    Status = MarkObjectIfWhite (Alloc, &GreyList, Object);
    if (EFI_ERROR (Status)) {
      goto cleanup;
    }
  }

  // Walk the graph
  while (BoraxGreyListPop (&GreyList, &Object)) {
    Status = MarkSubObjectsIfWhite (Alloc, &GreyList, Object);
    if (EFI_ERROR (Status)) {
      goto cleanup;
    }

    Status = MarkObjectBlack (Alloc, Object);
    if (EFI_ERROR (Status)) {
      goto cleanup;
    }
  }

  // TODO: Mark everything white... or just reinterpret black and white?

  ClearSpace (Alloc, &Alloc->FromSpace);

cleanup:
  BoraxGreyListCleanup (&GreyList);
  return Status;
}

EFI_STATUS
EFIAPI
BoraxAllocateCons (
  IN BORAX_ALLOCATOR  *Alloc,
  OUT BORAX_CONS      **Cons
  )
{
  BORAX_CONS_PAGE  *Page;

  if ((Alloc->ToSpace.Cons.Pages == NULL) ||
      (Alloc->ToSpace.Cons.FillIndex == BORAX_PAGE_SIZE))
  {
    // No page or page is full; allocate one
    Page = FFAllocatePages (Alloc, 1);
    if (Page == NULL) {
      return EFI_OUT_OF_RESOURCES;
    }

    // Prepare page
    Page->Next        = Alloc->ToSpace.Cons.Pages;
    Page->SpaceParity = Alloc->ToSpaceParity;
    SetMem (Page->GreyBitmap, sizeof (Page->GreyBitmap), 0);

    // Push it onto the page list
    Alloc->ToSpace.Cons.Pages     = Page;
    Alloc->ToSpace.Cons.FillIndex = BORAX_CONS_FIRST_INDEX;
  }

  // Bump allocate
  *Cons                          = (BORAX_CONS *)((CHAR *)Page + Alloc->ToSpace.Cons.FillIndex);
  Alloc->ToSpace.Cons.FillIndex += sizeof (BORAX_CONS);
  return EFI_SUCCESS;
}

STATIC CONST UINTN  gBinSizes[BORAX_ALLOC_BIN_COUNT] = {
  BORAX_ALLOC_BIN_FULL,
  BORAX_ALLOC_BIN_64,
  BORAX_ALLOC_BIN_128,
  BORAX_ALLOC_BIN_256,
  BORAX_ALLOC_BIN_512,
  BORAX_ALLOC_BIN_1024,
  BORAX_ALLOC_BIN_2048,
  BORAX_ALLOC_BIN_MAX,
};

EFI_STATUS
EFIAPI
BoraxAllocateObject (
  IN BORAX_ALLOCATOR       *Alloc,
  IN UINTN                 Size,
  OUT BORAX_OBJECT_HEADER  **Object
  )
{
  BORAX_OBJECT_CHUNK   *Chunk = NULL;
  BORAX_OBJECT_HEADER  *NewObject;
  UINTN                Bin;
  UINTN                Remainder;

  // Search for the smallest bin with an available chunk
  for (Bin = 1; Bin < BORAX_ALLOC_BIN_COUNT; ++Bin) {
    if ((Size <= gBinSizes[Bin]) &&
        (Alloc->ToSpace.Object.Chunks[Bin] != NULL))
    {
      Chunk                             = Alloc->ToSpace.Object.Chunks[Bin];
      Alloc->ToSpace.Object.Chunks[Bin] = Chunk->Next;
      break;
    }
  }

  // Allocate a new chunk if we didn't find a suitable one
  if (Chunk == NULL) {
    UINTN  Bytes = BORAX_OBJECT_FIRST_INDEX + Size;
    UINTN  Pages = (Bytes + BORAX_PAGE_SIZE - 1) / BORAX_PAGE_SIZE;

    Chunk = FFAllocatePages (Alloc, Pages);
    if (Chunk == NULL) {
      return EFI_OUT_OF_RESOURCES;
    }

    Chunk->FillIndex = BORAX_OBJECT_FIRST_INDEX;
    Chunk->Pages     = Pages;
  }

  // Allocate the object from the chunk
  NewObject         = (BORAX_OBJECT_HEADER *)((CHAR *)Chunk + Chunk->FillIndex);
  NewObject->GcData = BORAX_OBJECT_GCDATA_WHITE;
  Chunk->FillIndex  = BORAX_ALIGN (Chunk->FillIndex + Size);

  // Store the chunk according to its remaining space
  Remainder = (Chunk->Pages * BORAX_PAGE_SIZE) - Chunk->FillIndex;
  for (Bin = BORAX_ALLOC_BIN_COUNT - 1; TRUE; --Bin) {
    // The "full" bin guarantees termination
    if (Remainder >= gBinSizes[Bin]) {
      Chunk->Next                       = Alloc->ToSpace.Object.Chunks[Bin];
      Alloc->ToSpace.Object.Chunks[Bin] = Chunk;
      break;
    }
  }

  *Object = NewObject;
  return EFI_SUCCESS;
}

EFI_STATUS
EFIAPI
BoraxAllocatePin (
  IN BORAX_ALLOCATOR  *Alloc,
  IN BORAX_OBJECT     Object,
  OUT BORAX_PIN       **Pin
  )
{
  BORAX_PIN  *NewPin;

  // Get the memory for the pin
  NewPin = FFAllocatePool (Alloc, sizeof (*NewPin));
  if (NewPin == NULL) {
    return EFI_OUT_OF_RESOURCES;
  }

  // Initialize the pin and add it to the list
  NewPin->Header.WideTag       = BORAX_WIDETAG_PIN;
  NewPin->Header.GcData        = BORAX_OBJECT_GCDATA_WHITE;
  NewPin->ListEntry.Next       = &Alloc->Pins;
  NewPin->ListEntry.Prev       = Alloc->Pins.Prev;
  NewPin->ListEntry.Prev->Next = &NewPin->ListEntry;
  NewPin->ListEntry.Next->Prev = &NewPin->ListEntry;
  NewPin->Object               = Object;

  *Pin = NewPin;
  return EFI_SUCCESS;
}

EFI_STATUS
EFIAPI
BoraxAllocateWeakPointer (
  IN BORAX_ALLOCATOR      *Alloc,
  IN BORAX_OBJECT         Object,
  OUT BORAX_WEAK_POINTER  **WeakPointer
  )
{
  EFI_STATUS          Status;
  BORAX_WEAK_POINTER  *NewWp;

  // Allocate a regular lisp object
  Status = BoraxAllocateObject (
             Alloc,
             sizeof (*NewWp),
             (BORAX_OBJECT_HEADER *)&NewWp
             );
  if (EFI_ERROR (Status)) {
    return Status;
  }

  // Initialize the weak pointer and add it to the list
  NewWp->Header.WideTag = BORAX_WIDETAG_WEAK_POINTER;
  NewWp->Value          = Object;
  NewWp->Next           = Alloc->WeakPointers;
  Alloc->WeakPointers   = NewWp;

  *WeakPointer = NewWp;
  return EFI_SUCCESS;
}
