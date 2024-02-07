#include "Memory.h"

VOID
EFIAPI
BoraxAllocatorInit (
  OUT BORAX_ALLOCATOR                 *Alloc,
  IN BORAX_SYSTEM_ALLOCATOR_PROTOCOL  *SysAlloc
  )
{
  SetMem (Alloc, sizeof (*Alloc), 0);
  Alloc->Cons.FillIndex = BORAX_PAGE_SIZE;
  Alloc->Pins.Next      = &Alloc->Pins;
  Alloc->Pins.Prev      = &Alloc->Pins;
  Alloc->SysAlloc       = SysAlloc;
}

VOID
EFIAPI
BoraxAllocatorCleanup (
  IN BORAX_ALLOCATOR  *Alloc
  )
{
  BORAX_CONS_PAGE     *ConsPage;
  BORAX_OBJECT_CHUNK  *ObjChunk;
  BORAX_PIN_LIST      *PinEntry;
  UINTN               Bin;

  // Free cons pages
  ConsPage = Alloc->Cons.Pages;
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
    ObjChunk = Alloc->Object.Chunks[Bin];
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

EFI_STATUS
EFIAPI
BoraxAllocatorCollect (
  IN BORAX_ALLOCATOR               *Alloc,
  IN CONST BORAX_ROOTSET_ITERATOR  *RootSet
  );

EFI_STATUS
EFIAPI
BoraxAllocateCons (
  IN BORAX_ALLOCATOR  *Alloc,
  OUT BORAX_CONS      **Cons
  )
{
  BORAX_CONS_PAGE  *Page;

  if (Alloc->Cons.FillIndex == BORAX_PAGE_SIZE) {
    // No page or page is full; allocate one
    Page = Alloc->SysAlloc->AllocatePages (Alloc->SysAlloc, 1);
    if (Page == NULL) {
      return EFI_OUT_OF_RESOURCES;
    }

    // Prepare page
    SetMem (Page, BORAX_PAGE_SIZE, -1);
    Page->Next        = Alloc->Cons.Pages;
    Page->SpaceParity = Alloc->Cons.ToSpaceParity;
    SetMem (Page->GreyBitmap, sizeof (Page->GreyBitmap), 0);

    // Push it onto the page list
    Alloc->Cons.Pages     = Page;
    Alloc->Cons.FillIndex = BORAX_CONS_FIRST_INDEX;
  }

  // Bump allocate
  *Cons                  = (BORAX_CONS *)((CHAR *)Page + Alloc->Cons.FillIndex);
  Alloc->Cons.FillIndex += sizeof (BORAX_CONS);
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
    if ((Size <= gBinSizes[Bin]) && (Alloc->Object.Chunks[Bin] != NULL)) {
      Chunk                     = Alloc->Object.Chunks[Bin];
      Alloc->Object.Chunks[Bin] = Chunk->Next;
      break;
    }
  }

  // Allocate a new chunk if we didn't find a suitable one
  if (Chunk == NULL) {
    UINTN  Bytes = BORAX_OBJECT_FIRST_INDEX + Size;
    UINTN  Pages = (Bytes + BORAX_PAGE_SIZE - 1) / BORAX_PAGE_SIZE;

    Chunk = Alloc->SysAlloc->AllocatePages (Alloc->SysAlloc, Pages);
    if (Chunk == NULL) {
      return EFI_OUT_OF_RESOURCES;
    }

    SetMem (Chunk, Bytes, -1);
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
      Chunk->Next               = Alloc->Object.Chunks[Bin];
      Alloc->Object.Chunks[Bin] = Chunk;
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
  NewPin = Alloc->SysAlloc->AllocatePool (Alloc->SysAlloc, sizeof (*NewPin));
  if (NewPin == NULL) {
    return EFI_OUT_OF_RESOURCES;
  }

  // Initialize the pin and add it to the list
  SetMem (NewPin, sizeof (*NewPin), -1);
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
