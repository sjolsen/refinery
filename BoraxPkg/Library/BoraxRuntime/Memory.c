#include <Library/BoraxMemory.h>

#include <Library/BaseMemoryLib.h>
#include <Library/DebugLib.h>

#include "GreyList.h"

STATIC VOID *
EFIAPI
FFAllocatePages (
  IN BORAX_ALLOCATOR  *Alloc,
  IN UINTN            Pages
  )
{
  VOID  *Mem;

  Mem = Alloc->SysAlloc->AllocatePages (Alloc->SysAlloc, Pages);
  if (Mem == NULL) {
    return NULL;
  }

  SetMem (Mem, BORAX_PAGE_SIZE * Pages, -1);
  return Mem;
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

STATIC EFI_STATUS
EFIAPI
GetObjectGcData (
  IN BORAX_ALLOCATOR      *Alloc,
  IN BORAX_OBJECT_HEADER  *Object,
  OUT UINTN               *GcData
  )
{
  UINTN  Result = 0;

  if (BORAX_IS_CONS (Object)) {
    BORAX_CONS       *Cons = (BORAX_CONS *)Object;
    BORAX_CONS_PAGE  *Page = CONS_PAGE (Cons);
    UINTN            Word  = CONS_BITMAP_WORD (Cons);
    UINTN            Bit   = CONS_BITMAP_BIT (Cons);

    if (Page->SpaceParity) {
      Result |= BORAX_OBJECT_GCDATA_SPACEBIT;
    }

    if (Page->GreyBitmap[Word] & (1 << Bit)) {
      Result |= BORAX_OBJECT_GCDATA_GREYBIT;
    }
  } else {
    switch (Object->WideTag) {
      case BORAX_WIDETAG_WEAK_POINTER:
      case BORAX_WIDETAG_PIN:
      case BORAX_WIDETAG_MOVED:
      case BORAX_WIDETAG_UNINITIALIZED:
        // The GcData field should be set even for uninitialized objects
        Result = Object->GcData;
        break;
      default:
        // We should never see this case
        DEBUG ((
          DEBUG_ERROR,
          "%a: invalid widetag (%u)\n",
          __func__,
          Object->WideTag
          ));
        return EFI_INVALID_PARAMETER;
    }
  }

  *GcData = Result;
  return EFI_SUCCESS;
}

STATIC EFI_STATUS
EFIAPI
SetObjectGcData (
  IN BORAX_ALLOCATOR      *Alloc,
  IN BORAX_OBJECT_HEADER  *Object,
  IN UINTN                GcData
  )
{
  if (BORAX_IS_CONS (Object)) {
    BORAX_CONS       *Cons = (BORAX_CONS *)Object;
    BORAX_CONS_PAGE  *Page = CONS_PAGE (Cons);
    UINTN            Word  = CONS_BITMAP_WORD (Cons);
    UINTN            Bit   = CONS_BITMAP_BIT (Cons);

    if (GcData & BORAX_OBJECT_GCDATA_GREYBIT) {
      Page->GreyBitmap[Word] |= (1 << Bit);
    } else {
      Page->GreyBitmap[Word] &= ~(1 << Bit);
    }

    // We can't set space parity at cell granularity, but we shouldn't need to
    if ((GcData & BORAX_OBJECT_GCDATA_SPACEBIT) != Page->SpaceParity) {
      DEBUG ((DEBUG_ERROR, "%a: cons space parity violated\n", __func__));
      return EFI_INVALID_PARAMETER;
    }
  } else {
    switch (Object->WideTag) {
      case BORAX_WIDETAG_WEAK_POINTER:
      case BORAX_WIDETAG_PIN:
      case BORAX_WIDETAG_MOVED:
      case BORAX_WIDETAG_UNINITIALIZED:
        Object->GcData = GcData;
        break;
      default:
        // We should never see this case
        DEBUG ((
          DEBUG_ERROR,
          "%a: invalid widetag (%u)\n",
          __func__,
          Object->WideTag
          ));
        return EFI_INVALID_PARAMETER;
    }
  }

  return EFI_SUCCESS;
}

typedef enum {
  WHITE, GREY, BLACK
} COLOR;

STATIC COLOR
EFIAPI
DecodeColor (
  IN BORAX_ALLOCATOR  *Alloc,
  IN UINTN            GcData
  )
{
  UINTN  ToSpace = Alloc->ToSpaceParity;

  if (GcData & BORAX_OBJECT_GCDATA_GREYBIT) {
    return GREY;
  } else if ((GcData & BORAX_OBJECT_GCDATA_SPACEBIT) == ToSpace) {
    return BLACK;
  } else {
    return WHITE;
  }
}

STATIC EFI_STATUS
EFIAPI
UpdateColor (
  IN BORAX_ALLOCATOR  *Alloc,
  IN OUT UINTN        *GcData,
  IN COLOR            Color
  )
{
  switch (Color) {
    case GREY:
      *GcData |= BORAX_OBJECT_GCDATA_GREYBIT;
      return EFI_SUCCESS;
    case BLACK:
      *GcData = Alloc->ToSpaceParity;
      return EFI_SUCCESS;
    case WHITE:
      *GcData = Alloc->ToSpaceParity ^ BORAX_OBJECT_GCDATA_SPACEBIT;
      return EFI_SUCCESS;
    default:
      // We should never see this case
      DEBUG ((DEBUG_ERROR, "%a: invalid color (%u)\n", __func__, Color));
      return EFI_INVALID_PARAMETER;
  }
}

STATIC EFI_STATUS
EFIAPI
MarkObjectIfWhite (
  IN BORAX_ALLOCATOR      *Alloc,
  IN BORAX_GREY_LIST      *GreyList,
  IN BORAX_OBJECT_HEADER  *Object
  )
{
  EFI_STATUS           Status;
  UINTN                GcData;
  BORAX_OBJECT_HEADER  *NewObj = NULL;

  Status = GetObjectGcData (Alloc, Object, &GcData);
  if (EFI_ERROR (Status)) {
    return Status;
  }

  if (DecodeColor (Alloc, GcData) != WHITE) {
    // Nothing to do
    return EFI_SUCCESS;
  }

  // Copy from FromSpace to ToSpace
  if (BORAX_IS_CONS (Object)) {
    Status = BoraxAllocateCons (Alloc, (BORAX_CONS **)&NewObj);
    if (EFI_ERROR (Status)) {
      return Status;
    }

    // Future reads to Object will interpret it as a "moved" object and will
    // access GcData instead of the bitmap.
    CopyMem (NewObj, Object, sizeof (BORAX_CONS));
    Object->WideTag        = BORAX_WIDETAG_MOVED;
    Object->HeaderWords[1] = BORAX_MAKE_POINTER (NewObj);
  } else {
    switch (Object->WideTag) {
      // TODO: Implement move optimization for large objects
      case BORAX_WIDETAG_WEAK_POINTER:
      {
        UINTN  Size = sizeof (BORAX_WEAK_POINTER);

        Status = BoraxAllocateObject (Alloc, Size, &NewObj);
        if (EFI_ERROR (Status)) {
          return Status;
        }

        CopyMem (NewObj, Object, Size);
        Object->WideTag        = BORAX_WIDETAG_MOVED;
        Object->HeaderWords[1] = BORAX_MAKE_POINTER (NewObj);
        break;
      }
      case BORAX_WIDETAG_PIN:  // Don't move pins
      case BORAX_WIDETAG_MOVED:
      case BORAX_WIDETAG_UNINITIALIZED:
        break;
      default:
        // We should never see this case
        DEBUG ((
          DEBUG_ERROR,
          "%a: invalid widetag (%u)\n",
          __func__,
          Object->WideTag
          ));
        return EFI_INVALID_PARAMETER;
    }
  }

  // Mark the old and new copy (if it exists) grey
  (VOID)UpdateColor (Alloc, &GcData, GREY);
  Status = SetObjectGcData (Alloc, Object, GcData);
  if (EFI_ERROR (Status)) {
    return Status;
  }

  if (NewObj == NULL) {
    Status = BoraxGreyListPush (GreyList, Object);
    if (EFI_ERROR (Status)) {
      return Status;
    }
  } else {
    GcData ^= BORAX_OBJECT_GCDATA_SPACEBIT;
    Status = SetObjectGcData (Alloc, NewObj, GcData);
    if (EFI_ERROR (Status)) {
      return Status;
    }

    Status = BoraxGreyListPush (GreyList, NewObj);
    if (EFI_ERROR (Status)) {
      return Status;
    }
  }

  return EFI_SUCCESS;
}

STATIC EFI_STATUS
EFIAPI
MarkObjectWordIfWhite (
  IN BORAX_ALLOCATOR  *Alloc,
  IN BORAX_GREY_LIST  *GreyList,
  IN BORAX_OBJECT     ObjectWord
  )
{
  EFI_STATUS           Status;
  BORAX_OBJECT_HEADER  *Object;

  // There's only work to be done if we're looking at an unmarked heap object
  if (!BORAX_IS_POINTER (ObjectWord)) {
    return EFI_SUCCESS;
  }

  Object = BORAX_GET_POINTER (ObjectWord);
  Status = MarkObjectIfWhite (Alloc, GreyList, Object);
  return Status;
}

STATIC EFI_STATUS
EFIAPI
MarkSubObjectsIfWhite (
  IN BORAX_ALLOCATOR      *Alloc,
  IN BORAX_GREY_LIST      *GreyList,
  IN BORAX_OBJECT_HEADER  *Object
  )
{
  EFI_STATUS  Status;

  if (BORAX_IS_CONS (Object)) {
    BORAX_CONS  *Cons = (BORAX_CONS *)Object;

    Status = MarkObjectWordIfWhite (Alloc, GreyList, Cons->Car);
    if (EFI_ERROR (Status)) {
      return Status;
    }

    // Mark CDR last to ensure it gets copied first
    Status = MarkObjectWordIfWhite (Alloc, GreyList, Cons->Cdr);
    return Status;
  } else {
    switch (Object->WideTag) {
      case BORAX_WIDETAG_PIN:
      {
        BORAX_PIN  *Pin = (BORAX_PIN *)Object;
        Status = MarkObjectWordIfWhite (Alloc, GreyList, Pin->Object);
        return Status;
      }
      case BORAX_WIDETAG_WEAK_POINTER:
      case BORAX_WIDETAG_MOVED:
      case BORAX_WIDETAG_UNINITIALIZED:
        // Nothing to do
        return EFI_SUCCESS;
      default:
        // We should never see this case
        DEBUG ((
          DEBUG_ERROR,
          "%a: invalid widetag (%u)\n",
          __func__,
          Object->WideTag
          ));
        return EFI_INVALID_PARAMETER;
    }
  }
}

STATIC VOID
EFIAPI
UpdateIfMoved (
  IN OUT BORAX_OBJECT  *Object
  )
{
  BORAX_OBJECT_HEADER  *Obj;

  if (!BORAX_IS_POINTER (*Object)) {
    return;
  }

  Obj = BORAX_GET_POINTER (*Object);
  if (!BORAX_IS_HEAP (Obj->HeaderWords[0])) {
    return;
  }

  if (Obj->WideTag != BORAX_WIDETAG_MOVED) {
    return;
  }

  *Object = Obj->HeaderWords[1];
}

STATIC EFI_STATUS
EFIAPI
MarkObjectBlack (
  IN BORAX_ALLOCATOR      *Alloc,
  IN BORAX_OBJECT_HEADER  *Object
  )
{
  EFI_STATUS  Status;
  UINTN       GcData;

  if (BORAX_IS_CONS (Object)) {
    BORAX_CONS  *Cons = (BORAX_CONS *)Object;

    UpdateIfMoved (&Cons->Car);
    UpdateIfMoved (&Cons->Cdr);
  } else {
    switch (Object->WideTag) {
      case BORAX_WIDETAG_PIN:
      {
        BORAX_PIN  *Pin = (BORAX_PIN *)Object;
        UpdateIfMoved (&Pin->Object);
        break;
      }
      case BORAX_WIDETAG_WEAK_POINTER:
      {
        BORAX_WEAK_POINTER  *Wp = (BORAX_WEAK_POINTER *)Object;
        UpdateIfMoved (&Wp->Value);
        break;
      }
      case BORAX_WIDETAG_MOVED:
      case BORAX_WIDETAG_UNINITIALIZED:
        break;
      default:
        // We should never see this case
        DEBUG ((
          DEBUG_ERROR,
          "%a: invalid widetag (%u)\n",
          __func__,
          Object->WideTag
          ));
        return EFI_INVALID_PARAMETER;
    }
  }

  Status = GetObjectGcData (Alloc, Object, &GcData);
  if (EFI_ERROR (Status)) {
    return Status;
  }

  (VOID)UpdateColor (Alloc, &GcData, BLACK);
  Status = SetObjectGcData (Alloc, Object, GcData);
  return Status;
}

STATIC EFI_STATUS
EFIAPI
SweepPins (
  IN BORAX_ALLOCATOR  *Alloc
  )
{
  EFI_STATUS      Status;
  UINTN           GcData;
  BORAX_PIN_LIST  *PinEntry;

  PinEntry = Alloc->Pins.Next;
  while (PinEntry != &Alloc->Pins) {
    BORAX_PIN_LIST  *Next = PinEntry->Next;
    BORAX_PIN_LIST  *Prev = PinEntry->Prev;
    BORAX_PIN       *Pin  = BASE_CR (PinEntry, BORAX_PIN, ListEntry);

    Status = GetObjectGcData (Alloc, &Pin->Header, &GcData);
    if (EFI_ERROR (Status)) {
      return Status;
    }

    switch (DecodeColor (Alloc, GcData)) {
      case WHITE:
        Next->Prev = Prev;
        Prev->Next = Next;
        Alloc->SysAlloc->FreePool (Alloc->SysAlloc, Pin);
        break;
      case GREY:
        DEBUG ((DEBUG_ERROR, "grey pin found during sweep\n"));
        return EFI_INVALID_PARAMETER;
      case BLACK:
        break;
    }

    PinEntry = Next;
  }

  return EFI_SUCCESS;
}

EFI_STATUS
EFIAPI
BoraxAllocatorCollect (
  IN BORAX_ALLOCATOR               *Alloc,
  IN CONST BORAX_ROOTSET_ITERATOR  *RootSet
  )
{
  EFI_STATUS           Status;
  BORAX_GREY_LIST      GreyList;
  BORAX_OBJECT         ObjectWord;
  BORAX_OBJECT_HEADER  *Object;

  // Begin by flipping spaces
  Alloc->FromSpace = Alloc->ToSpace;
  SetMem (&Alloc->ToSpace, sizeof (Alloc->ToSpace), 0);
  Alloc->ToSpaceParity = !Alloc->ToSpaceParity;

  // Mark the initial set of root objects grey
  BoraxGreyListInit (&GreyList, Alloc->SysAlloc);
  while (RootSet->Next (RootSet->Ctx, &ObjectWord)) {
    if (!BORAX_IS_POINTER (ObjectWord)) {
      continue;
    }

    Object = BORAX_GET_POINTER (ObjectWord);
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

  // Remove white objects
  ClearSpace (Alloc, &Alloc->FromSpace);
  SetMem (&Alloc->FromSpace, sizeof (Alloc->FromSpace), 0);

  Status = SweepPins (Alloc);
  if (EFI_ERROR (Status)) {
    goto cleanup;
  }

  // All remaining objects are marked black; the next collection will flip
  // Alloc->ToSpaceParity, which will effectively mark those object white
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
  BORAX_CONS_PAGE  *Page = Alloc->ToSpace.Cons.Pages;

  if ((Page == NULL) || (Alloc->ToSpace.Cons.FillIndex == BORAX_PAGE_SIZE)) {
    // No page or page is full; allocate one
    Page = FFAllocatePages (Alloc, 1);
    if (Page == NULL) {
      DEBUG ((DEBUG_ERROR, "%a: out of memory\n", __func__));
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
  *Cons                          = (BORAX_CONS *)((CHAR8 *)Page + Alloc->ToSpace.Cons.FillIndex);
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
      DEBUG ((DEBUG_ERROR, "%a: out of memory\n", __func__));
      return EFI_OUT_OF_RESOURCES;
    }

    Chunk->FillIndex = BORAX_OBJECT_FIRST_INDEX;
    Chunk->Pages     = Pages;
  }

  // Allocate the object from the chunk
  NewObject         = (BORAX_OBJECT_HEADER *)((CHAR8 *)Chunk + Chunk->FillIndex);
  NewObject->GcData = Alloc->ToSpaceParity;
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
    DEBUG ((DEBUG_ERROR, "%a: out of memory\n", __func__));
    return EFI_OUT_OF_RESOURCES;
  }

  // Initialize the pin and add it to the list
  NewPin->Header.WideTag       = BORAX_WIDETAG_PIN;
  NewPin->Header.GcData        = Alloc->ToSpaceParity;
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
             (BORAX_OBJECT_HEADER **)&NewWp
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
