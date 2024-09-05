#include <Library/BoraxMemory.h>

#include <Library/BaseMemoryLib.h>
#include <Library/DebugLib.h>

#include "Stack.h"

STATIC EFI_STATUS
EFIAPI
GcHooks (
  IN BORAX_OBJECT_HEADER    *Object,
  OUT CONST BORAX_GC_HOOKS  **Hooks
  );

STATIC EFI_STATUS
EFIAPI
GcHookCopy (
  IN BORAX_ALLOCATOR       *Alloc,
  IN BORAX_OBJECT_HEADER   *OldObject,
  OUT BORAX_OBJECT_HEADER  **NewObject
  )
{
  EFI_STATUS            Status;
  CONST BORAX_GC_HOOKS  *Hooks;

  Status = GcHooks (OldObject, &Hooks);
  if (EFI_ERROR (Status)) {
    return Status;
  }

  return Hooks->Copy (Alloc, OldObject, NewObject);
}

STATIC EFI_STATUS
EFIAPI
GcHookSubObjects (
  IN BORAX_OBJECT_HEADER          *Object,
  IN VOID                         *Ctx,
  IN BORAX_GC_SUBOBJECT_CALLBACK  Callback
  )
{
  EFI_STATUS            Status;
  CONST BORAX_GC_HOOKS  *Hooks;

  Status = GcHooks (Object, &Hooks);
  if (EFI_ERROR (Status)) {
    return Status;
  }

  return Hooks->SubObjects (Object, Ctx, Callback);
}

STATIC VOID *
EFIAPI
InternalAllocatePages (
  IN BORAX_ALLOCATOR  *Alloc,
  IN UINTN            Pages
  )
{
  VOID  *Mem;

  Mem = Alloc->SysAlloc->AllocatePages (Alloc->SysAlloc, Pages);
  if (Mem == NULL) {
    return NULL;
  }

  Alloc->UsedPages += Pages;
  SetMem (Mem, BORAX_PAGE_SIZE * Pages, -1);
  return Mem;
}

STATIC VOID
EFIAPI
InternalFreePages (
  IN BORAX_ALLOCATOR  *Alloc,
  IN VOID             *Buffer,
  IN UINTN            Pages
  )
{
  Alloc->SysAlloc->FreePages (Alloc->SysAlloc, Buffer, Pages);
  Alloc->UsedPages -= Pages;
}

STATIC VOID *
EFIAPI
InternalAllocatePool (
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

STATIC VOID
EFIAPI
InternalFreePool (
  IN BORAX_ALLOCATOR  *Alloc,
  IN VOID             *Buffer
  )
{
  Alloc->SysAlloc->FreePool (Alloc->SysAlloc, Buffer);
}

VOID
EFIAPI
BoraxAllocatorInit (
  OUT BORAX_ALLOCATOR                 *Alloc,
  IN BORAX_SYSTEM_ALLOCATOR_PROTOCOL  *SysAlloc
  )
{
  SetMem (Alloc, sizeof (*Alloc), 0);
  Alloc->SysAlloc = SysAlloc;
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
    InternalFreePages (Alloc, ConsPage, ConsPage->Pages);
    ConsPage = Next;
  }

  // Free object page chunks
  for (Bin = 0; Bin < BORAX_ALLOC_BIN_COUNT; ++Bin) {
    ObjChunk = Space->Object.Chunks[Bin];
    while (ObjChunk != NULL) {
      BORAX_OBJECT_CHUNK  *Next = ObjChunk->Next;
      InternalFreePages (Alloc, ObjChunk, ObjChunk->Pages);
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
  BORAX_PIN_RECORD  *Pin;

  // We may have aborted in the middle of a cycle, so clean both spaces
  ClearSpace (Alloc, &Alloc->FromSpace);
  ClearSpace (Alloc, &Alloc->ToSpace);

  // Free pin objects
  Pin = Alloc->Pins;
  while (Pin != NULL) {
    BORAX_PIN_RECORD  *Next = Pin->Next;
    InternalFreePool (Alloc, Pin);
    Pin = Next;
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
(CONS_BITMAP_INDEX(_addr) / BORAX_WORD_BITS)

#define CONS_BITMAP_BIT(_addr) \
(CONS_BITMAP_INDEX(_addr) % BORAX_WORD_BITS)

STATIC UINTN
EFIAPI
GetObjectGcData (
  IN BORAX_ALLOCATOR      *Alloc,
  IN BORAX_OBJECT_HEADER  *Object
  )
{
  if (BORAX_IS_CONS (Object)) {
    BORAX_CONS       *Cons  = (BORAX_CONS *)Object;
    BORAX_CONS_PAGE  *Page  = CONS_PAGE (Cons);
    UINTN            Word   = CONS_BITMAP_WORD (Cons);
    UINTN            Bit    = CONS_BITMAP_BIT (Cons);
    UINTN            Result = 0;

    if (Page->SpaceParity) {
      Result |= BORAX_OBJECT_GCDATA_SPACEBIT;
    }

    if (Page->GreyBitmap[Word] & (1 << Bit)) {
      Result |= BORAX_OBJECT_GCDATA_GREYBIT;
    }

    return Result;
  } else {
    // The GcData field should be set even for uninitialized objects
    return Object->GcData;
  }
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
    Object->GcData = GcData;
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
MarkObjectGrey (
  IN BORAX_ALLOCATOR  *Alloc,
  IN BORAX_STACK      *GreyList,
  IN BORAX_OBJECT     Object
  )
{
  EFI_STATUS           Status;
  UINTN                GcData;
  BORAX_OBJECT_HEADER  *OldObject;
  BORAX_OBJECT_HEADER  *NewObject = NULL;

  // There is only work to be done for white heap objects
  if (!BORAX_IS_POINTER (Object)) {
    return EFI_SUCCESS;
  }

  OldObject = BORAX_GET_POINTER (Object);
  GcData    = GetObjectGcData (Alloc, OldObject);
  if (DecodeColor (Alloc, GcData) != WHITE) {
    return EFI_SUCCESS;
  }

  // Copy from FromSpace to ToSpace. NewObj will be NULL if the object cannot be
  // moved.
  Status = GcHookCopy (Alloc, OldObject, &NewObject);
  if (EFI_ERROR (Status)) {
    return Status;
  }

  (VOID)UpdateColor (Alloc, &GcData, GREY);
  if (NewObject == NULL) {
    // If the object was not copied, simply mark it grey
    Status = SetObjectGcData (Alloc, OldObject, GcData);
    if (EFI_ERROR (Status)) {
      return Status;
    }

    Status = BoraxStackPush (GreyList, (UINTN)OldObject);
    if (EFI_ERROR (Status)) {
      return Status;
    }
  } else {
    // If the object was copied, mark the original "moved" and mark _both_
    // copies grey -- as further accesses to the object's original location need
    // to see that it has already been visited -- but only push the new object
    // onto the grey list
    OldObject->WideTag        = BORAX_WIDETAG_MOVED;
    OldObject->HeaderWords[1] = BORAX_MAKE_POINTER (NewObject);

    Status = SetObjectGcData (Alloc, OldObject, GcData);
    if (EFI_ERROR (Status)) {
      return Status;
    }

    GcData ^= BORAX_OBJECT_GCDATA_SPACEBIT;
    Status  = SetObjectGcData (Alloc, NewObject, GcData);
    if (EFI_ERROR (Status)) {
      return Status;
    }

    Status = BoraxStackPush (GreyList, (UINTN)NewObject);
    if (EFI_ERROR (Status)) {
      return Status;
    }
  }

  return EFI_SUCCESS;
}

typedef struct {
  BORAX_ALLOCATOR    *Alloc;
  BORAX_STACK        *GreyList;
} MARK_GREY_SUBOBJECT_CTX;

STATIC EFI_STATUS
EFIAPI
MarkSubObjectGrey (
  IN VOID          *Ctx,
  IN BORAX_OBJECT  *SubObject
  )
{
  MARK_GREY_SUBOBJECT_CTX  *TheCtx = (MARK_GREY_SUBOBJECT_CTX *)Ctx;

  return MarkObjectGrey (TheCtx->Alloc, TheCtx->GreyList, *SubObject);
}

STATIC EFI_STATUS
EFIAPI
UpdateSubObjectIfMoved (
  IN VOID              *Ctx,
  IN OUT BORAX_OBJECT  *SubObject
  )
{
  if (BORAX_DISCRIMINATE (*SubObject) == BORAX_DISCRIM_MOVED) {
    *SubObject = BORAX_GET_POINTER (*SubObject)->HeaderWords[1];
  }

  return EFI_SUCCESS;
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

  (VOID)GcHookSubObjects (Object, NULL, UpdateSubObjectIfMoved);

  GcData = GetObjectGcData (Alloc, Object);
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
  UINTN             GcData;
  BORAX_PIN_RECORD  **Iter;
  BORAX_PIN_RECORD  *Pin;
  BOOLEAN           Live;

  Iter = &Alloc->Pins;
  while (*Iter != NULL) {
    Pin    = *Iter;
    GcData = GetObjectGcData (Alloc, &Pin->Header);

    switch (DecodeColor (Alloc, GcData)) {
      case WHITE:
        Live = Pin->Live;
        break;
      case GREY:
        DEBUG ((DEBUG_ERROR, "grey pin found during sweep\n"));
        return EFI_INVALID_PARAMETER;
      case BLACK:
        Live = TRUE;
        break;
    }

    if (Live) {
      Iter = &Pin->Next;
    } else {
      *Iter = Pin->Next;
      InternalFreePool (Alloc, Pin);
    }
  }

  return EFI_SUCCESS;
}

STATIC EFI_STATUS
EFIAPI
SweepWeakPointers (
  IN BORAX_ALLOCATOR  *Alloc
  )
{
  UINTN                GcData;
  BORAX_WEAK_POINTER   *Wp;
  BORAX_OBJECT_HEADER  *Value;

  // Look for referents that are about to get collected
  for (Wp = Alloc->ToSpace.WeakPointers; Wp != NULL; Wp = Wp->Next) {
    // If the weak pointer was marked before its referent, it will not have
    // had a chance to see the moved tag, so we delay that logic until here.
    UpdateSubObjectIfMoved (NULL, &Wp->Value);
    if (!BORAX_IS_POINTER (Wp->Value)) {
      continue;
    }

    Value  = BORAX_GET_POINTER (Wp->Value);
    GcData = GetObjectGcData (Alloc, Value);
    switch (DecodeColor (Alloc, GcData)) {
      case WHITE:
        Wp->Value = BORAX_IMMEDIATE_UNBOUND;
        break;
      case GREY:
        DEBUG ((DEBUG_ERROR, "grey weak referent found during sweep\n"));
        return EFI_INVALID_PARAMETER;
      case BLACK:
        break;
    }
  }

  return EFI_SUCCESS;
}

EFI_STATUS
EFIAPI
BoraxAllocatorCollect (
  IN BORAX_ALLOCATOR  *Alloc
  )
{
  EFI_STATUS               Status;
  BORAX_PIN_RECORD         *Pin;
  BORAX_STACK              GreyList;
  BORAX_OBJECT_HEADER      *Object;
  MARK_GREY_SUBOBJECT_CTX  MarkGreyCtx;

  // Begin by flipping spaces
  Alloc->FromSpace = Alloc->ToSpace;
  SetMem (&Alloc->ToSpace, sizeof (Alloc->ToSpace), 0);
  Alloc->ToSpaceParity = !Alloc->ToSpaceParity;

  // Mark the initial set of root objects grey
  BoraxStackInit (&GreyList, Alloc->SysAlloc);
  for (Pin = Alloc->Pins; Pin != NULL; Pin = Pin->Next) {
    if (Pin->Live) {
      Status = MarkObjectGrey (Alloc, &GreyList, BORAX_MAKE_POINTER (&Pin->Header));
      if (EFI_ERROR (Status)) {
        goto cleanup;
      }
    }
  }

  // Walk the graph
  MarkGreyCtx.Alloc    = Alloc;
  MarkGreyCtx.GreyList = &GreyList;
  while (BoraxStackPop (&GreyList, (UINTN *)&Object)) {
    Status = GcHookSubObjects (Object, &MarkGreyCtx, MarkSubObjectGrey);
    if (EFI_ERROR (Status)) {
      goto cleanup;
    }

    Status = MarkObjectBlack (Alloc, Object);
    if (EFI_ERROR (Status)) {
      goto cleanup;
    }
  }

  // Remove white objects
  Status = SweepPins (Alloc);
  if (EFI_ERROR (Status)) {
    goto cleanup;
  }

  Status = SweepWeakPointers (Alloc);
  if (EFI_ERROR (Status)) {
    goto cleanup;
  }

  ClearSpace (Alloc, &Alloc->FromSpace);
  SetMem (&Alloc->FromSpace, sizeof (Alloc->FromSpace), 0);

  // All remaining objects are marked black; the next collection will flip
  // Alloc->ToSpaceParity, which will effectively mark those object white
cleanup:
  BoraxStackCleanup (&GreyList);
  return Status;
}

VOID *
EFIAPI
BoraxAllocateExternalPages (
  IN BORAX_ALLOCATOR  *Alloc,
  IN UINTN            Pages
  )
{
  return InternalAllocatePages (Alloc, Pages);
}

VOID
EFIAPI
BoraxFreeExternalPages (
  IN BORAX_ALLOCATOR  *Alloc,
  IN VOID             *Buffer,
  IN UINTN            Pages
  )
{
  return InternalFreePages (Alloc, Buffer, Pages);
}

VOID
EFIAPI
BoraxInjectExternalConsPages (
  IN BORAX_ALLOCATOR  *Alloc,
  IN VOID             *Buffer,
  IN UINTN            Pages
  )
{
  BORAX_CONS_PAGE  *FirstPage = (BORAX_CONS_PAGE *)Buffer;
  UINTN            I;

  // Buffer:Pages may be NULL:0
  if (Pages == 0) {
    return;
  }

  // Initialize the first header
  FirstPage->Next        = Alloc->ToSpace.Cons.Pages;
  FirstPage->Pages       = Pages;
  FirstPage->SpaceParity = Alloc->ToSpaceParity;
  SetMem (FirstPage->GreyBitmap, sizeof (FirstPage->GreyBitmap), 0);

  // Initialize the remaining headers
  for (I = 1; I < Pages; ++I) {
    BORAX_CONS_PAGE  *Page = (BORAX_CONS_PAGE *)
                             ((CHAR8 *)Buffer + I * BORAX_PAGE_SIZE);

    SetMem (Page, sizeof (*Page), 0);
    Page->SpaceParity = Alloc->ToSpaceParity;
  }

  // Push the chunk onto the page list (for simplicity, assume it's full)
  Alloc->ToSpace.Cons.Pages     = FirstPage;
  Alloc->ToSpace.Cons.FillIndex = BORAX_PAGE_SIZE * Pages;
}

#define PAGE_END(_page)  (BORAX_PAGE_SIZE * (_page)->Pages)

EFI_STATUS
EFIAPI
BoraxAllocateCons (
  IN BORAX_ALLOCATOR  *Alloc,
  IN BORAX_OBJECT     Car,
  IN BORAX_OBJECT     Cdr,
  OUT BORAX_CONS      **Cons
  )
{
  BORAX_CONS_PAGE  *Page = Alloc->ToSpace.Cons.Pages;
  UINTN            FillIndex;

  if ((Page == NULL) || (Alloc->ToSpace.Cons.FillIndex == PAGE_END (Page))) {
    // No page or page is full; allocate one
    Page = InternalAllocatePages (Alloc, 1);
    if (Page == NULL) {
      DEBUG ((DEBUG_ERROR, "%a: out of memory\n", __func__));
      return EFI_OUT_OF_RESOURCES;
    }

    // Prepare page
    Page->Next        = Alloc->ToSpace.Cons.Pages;
    Page->Pages       = 1;
    Page->SpaceParity = Alloc->ToSpaceParity;
    SetMem (Page->GreyBitmap, sizeof (Page->GreyBitmap), 0);

    // Push it onto the page list
    Alloc->ToSpace.Cons.Pages     = Page;
    Alloc->ToSpace.Cons.FillIndex = BORAX_CONS_FIRST_INDEX;
  }

  // Bump allocate
  *Cons        = (BORAX_CONS *)((CHAR8 *)Page + Alloc->ToSpace.Cons.FillIndex);
  (*Cons)->Car = Car;
  (*Cons)->Cdr = Cdr;

  FillIndex = Alloc->ToSpace.Cons.FillIndex + sizeof (BORAX_CONS);
  if ((FillIndex < PAGE_END (Page)) && ((FillIndex % BORAX_PAGE_SIZE) == 0)) {
    FillIndex += BORAX_CONS_FIRST_INDEX;
  }

  Alloc->ToSpace.Cons.FillIndex = FillIndex;
  return EFI_SUCCESS;
}

STATIC EFI_STATUS
EFIAPI
CopyCons (
  IN BORAX_ALLOCATOR       *Alloc,
  IN BORAX_OBJECT_HEADER   *OldObject,
  OUT BORAX_OBJECT_HEADER  **NewObject
  )
{
  BORAX_CONS  *Cons = (BORAX_CONS *)OldObject;

  return BoraxAllocateCons (
           Alloc,
           Cons->Car,
           Cons->Cdr,
           (BORAX_CONS **)NewObject
           );
}

STATIC EFI_STATUS
EFIAPI
ConsSubObjects (
  IN BORAX_OBJECT_HEADER          *Object,
  IN VOID                         *Ctx,
  IN BORAX_GC_SUBOBJECT_CALLBACK  Callback
  )
{
  EFI_STATUS  Status;
  BORAX_CONS  *Cons = (BORAX_CONS *)Object;

  Status = Callback (Ctx, &Cons->Car);
  if (EFI_ERROR (Status)) {
    return Status;
  }

  // Mark CDR last to ensure it gets copied first
  return Callback (Ctx, &Cons->Cdr);
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

STATIC VOID
EFIAPI
StoreObjectChunk (
  IN BORAX_ALLOCATOR     *Alloc,
  IN BORAX_OBJECT_CHUNK  *Chunk
  )
{
  UINTN  Remainder = (Chunk->Pages * BORAX_PAGE_SIZE) - Chunk->FillIndex;
  UINTN  Bin;

  // Store the chunk according to its remaining space
  for (Bin = BORAX_ALLOC_BIN_COUNT - 1; TRUE; --Bin) {
    // The "full" bin guarantees termination
    if (Remainder >= gBinSizes[Bin]) {
      Chunk->Next                       = Alloc->ToSpace.Object.Chunks[Bin];
      Alloc->ToSpace.Object.Chunks[Bin] = Chunk;
      break;
    }
  }
}

EFI_STATUS
EFIAPI
BoraxInjectExternalObjectPages (
  IN BORAX_ALLOCATOR  *Alloc,
  IN VOID             *Buffer,
  IN UINTN            Pages
  )
{
  BORAX_OBJECT_CHUNK  *Chunk = (BORAX_OBJECT_CHUNK *)Buffer;
  UINTN               Index  = BORAX_OBJECT_FIRST_INDEX;

  // Buffer:Pages may be NULL:0
  if (Pages == 0) {
    return EFI_SUCCESS;
  }

  // Initialize the objects' gcdata
  while (Index < BORAX_PAGE_SIZE * Pages) {
    BORAX_OBJECT_HEADER  *Header = (BORAX_OBJECT_HEADER *)
                                   ((CHAR8 *)Buffer + Index);

    Header->GcData = Alloc->ToSpaceParity;

    // Assume objects are in-bounds (in practice, this is enfoced by
    // ObjectFile.c:TranslateObject)
    switch (BORAX_DISCRIMINATE_POINTER (Header)) {
      case BORAX_DISCRIM_WORD_RECORD:
      case BORAX_DISCRIM_OBJECT_RECORD:
      {
        BORAX_RECORD  *Record = (BORAX_RECORD *)Header;

        Index += sizeof (BORAX_RECORD) + sizeof (UINTN) * Record->Length;
        Index  = BORAX_ALIGN (Index);
        break;
      }
      case BORAX_DISCRIM_UNINITIALIZED:
        // We're done early
        goto gcdata_done;
      default:
        // Bad object
        return EFI_LOAD_ERROR;
    }
  }

gcdata_done:
  // Initialize the header
  Chunk->FillIndex = Index;
  Chunk->Pages     = Pages;

  // Store the chunk
  StoreObjectChunk (Alloc, Chunk);
  return EFI_SUCCESS;
}

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

    Chunk = InternalAllocatePages (Alloc, Pages);
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

  // Store the chunk
  StoreObjectChunk (Alloc, Chunk);

  *Object = NewObject;
  return EFI_SUCCESS;
}

EFI_STATUS
EFIAPI
BoraxCopyObject (
  IN BORAX_ALLOCATOR       *Alloc,
  IN UINTN                 Size,
  IN BORAX_OBJECT_HEADER   *OldObject,
  OUT BORAX_OBJECT_HEADER  **NewObject
  )
{
  EFI_STATUS           Status;
  UINTN                GcData;
  BORAX_OBJECT_HEADER  *TheNewObject;

  Status = BoraxAllocateObject (Alloc, Size, &TheNewObject);
  if (EFI_ERROR (Status)) {
    return Status;
  }

  // GcData is the only field we need to worry about clobbering
  GcData = TheNewObject->GcData;
  CopyMem (TheNewObject, OldObject, Size);
  TheNewObject->GcData = GcData;

  *NewObject = TheNewObject;
  return EFI_SUCCESS;
}

EFI_STATUS
EFIAPI
BoraxAllocatePinRecord (
  IN BORAX_ALLOCATOR    *Alloc,
  IN UINTN              WideTag,
  IN UINTN              Size,
  OUT BORAX_PIN_RECORD  **Record
  )
{
  BORAX_PIN_RECORD  *NewPin;

  // Get the memory for the pin
  NewPin = InternalAllocatePool (Alloc, Size);
  if (NewPin == NULL) {
    DEBUG ((DEBUG_ERROR, "%a: out of memory\n", __func__));
    return EFI_OUT_OF_RESOURCES;
  }

  // Initialize the pin and add it to the list
  NewPin->Header.WideTag = WideTag;
  NewPin->Header.GcData  = Alloc->ToSpaceParity;
  NewPin->Live           = TRUE;
  NewPin->Next           = Alloc->Pins;
  Alloc->Pins            = NewPin;

  *Record = NewPin;
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
  EFI_STATUS  Status;
  BORAX_PIN   *NewPin;

  Status = BoraxAllocatePinRecord (
             Alloc,
             BORAX_WIDETAG_PIN,
             sizeof (BORAX_PIN),
             (BORAX_PIN_RECORD **)&NewPin
             );
  if (EFI_ERROR (Status)) {
    return Status;
  }

  NewPin->Object = Object;
  *Pin           = NewPin;
  return EFI_SUCCESS;
}

VOID
EFIAPI
BoraxReleasePinRecord (
  IN BORAX_PIN_RECORD  *Record
  )
{
  Record->Live = FALSE;
}

VOID
EFIAPI
BoraxReleasePin (
  IN BORAX_PIN  *Pin
  )
{
  BoraxReleasePinRecord (&Pin->Record);
}

STATIC EFI_STATUS
EFIAPI
PinSubObjects (
  IN BORAX_OBJECT_HEADER          *Object,
  IN VOID                         *Ctx,
  IN BORAX_GC_SUBOBJECT_CALLBACK  Callback
  )
{
  BORAX_PIN  *Pin = (BORAX_PIN *)Object;

  return Callback (Ctx, &Pin->Object);
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
  NewWp->Header.WideTag       = BORAX_WIDETAG_WEAK_POINTER;
  NewWp->Value                = Object;
  NewWp->Next                 = Alloc->ToSpace.WeakPointers;
  Alloc->ToSpace.WeakPointers = NewWp;

  *WeakPointer = NewWp;
  return EFI_SUCCESS;
}

STATIC EFI_STATUS
EFIAPI
CopyWeakPointer (
  IN BORAX_ALLOCATOR       *Alloc,
  IN BORAX_OBJECT_HEADER   *OldObject,
  OUT BORAX_OBJECT_HEADER  **NewObject
  )
{
  BORAX_WEAK_POINTER  *Wp = (BORAX_WEAK_POINTER *)OldObject;

  return BoraxAllocateWeakPointer (
           Alloc,
           Wp->Value,
           (BORAX_WEAK_POINTER **)NewObject
           );
}

EFI_STATUS
EFIAPI
BoraxAllocateRecord (
  IN BORAX_ALLOCATOR  *Alloc,
  IN UINTN            WideTag,
  IN BORAX_OBJECT     Class,
  IN UINTN            Length,
  IN BORAX_HALFWORD   LengthAux,
  IN UINTN            InitialElement,
  OUT BORAX_RECORD    **Record
  )
{
  EFI_STATUS  Status;

  Status = BoraxAllocateRecordUninitialized (
             Alloc,
             WideTag,
             Class,
             Length,
             LengthAux,
             Record
             );
  if (EFI_ERROR (Status)) {
    return Status;
  }

  SetMemN ((*Record)->Data, sizeof (UINTN) * Length, InitialElement);
  return EFI_SUCCESS;
}

EFI_STATUS
EFIAPI
BoraxAllocateRecordUninitialized (
  IN BORAX_ALLOCATOR  *Alloc,
  IN UINTN            WideTag,
  IN BORAX_OBJECT     Class,
  IN UINTN            Length,
  IN BORAX_HALFWORD   LengthAux,
  OUT BORAX_RECORD    **Record
  )
{
  EFI_STATUS    Status;
  BORAX_RECORD  *NewRecord;

  switch (WideTag) {
    case BORAX_WIDETAG_WORD_RECORD:
    case BORAX_WIDETAG_OBJECT_RECORD:
      break;
    default:
      DEBUG ((DEBUG_ERROR, "%a: invalid widetag (%u)\n", __func__, WideTag));
      return EFI_INVALID_PARAMETER;
  }

  // Allocate a regular lisp object
  Status = BoraxAllocateObject (
             Alloc,
             sizeof (BORAX_RECORD) + sizeof (UINTN) * Length,
             (BORAX_OBJECT_HEADER **)&NewRecord
             );
  if (EFI_ERROR (Status)) {
    return Status;
  }

  // Initialize the record
  NewRecord->Header.WideTag = WideTag;
  NewRecord->LengthAux      = LengthAux;
  NewRecord->Length         = Length;
  NewRecord->Class          = Class;

  *Record = NewRecord;
  return EFI_SUCCESS;
}

STATIC EFI_STATUS
EFIAPI
CopyRecord (
  IN BORAX_ALLOCATOR       *Alloc,
  IN BORAX_OBJECT_HEADER   *OldObject,
  OUT BORAX_OBJECT_HEADER  **NewObject
  )
{
  BORAX_RECORD  *Record = (BORAX_RECORD *)OldObject;
  UINTN         Size    = sizeof (BORAX_RECORD) + sizeof (UINTN) * Record->Length;

  return BoraxCopyObject (Alloc, Size, OldObject, NewObject);
}

STATIC EFI_STATUS
EFIAPI
WordRecordSubObjects (
  IN BORAX_OBJECT_HEADER          *Object,
  IN VOID                         *Ctx,
  IN BORAX_GC_SUBOBJECT_CALLBACK  Callback
  )
{
  BORAX_RECORD  *Record = (BORAX_RECORD *)Object;

  return Callback (Ctx, &Record->Class);
}

STATIC EFI_STATUS
EFIAPI
ObjectRecordSubObjects (
  IN BORAX_OBJECT_HEADER          *Object,
  IN VOID                         *Ctx,
  IN BORAX_GC_SUBOBJECT_CALLBACK  Callback
  )
{
  EFI_STATUS    Status;
  BORAX_RECORD  *Record = (BORAX_RECORD *)Object;
  UINTN         I;

  Status = Callback (Ctx, &Record->Class);
  if (EFI_ERROR (Status)) {
    return Status;
  }

  for (I = 0; I < Record->Length; ++I) {
    Status = Callback (Ctx, &Record->Data[I]);
    if (EFI_ERROR (Status)) {
      return Status;
    }
  }

  return EFI_SUCCESS;
}

EFI_STATUS
EFIAPI
BoraxGetRecord (
  IN CONST CHAR8    *DebugFunc,
  IN INTN           DebugLine,
  IN BORAX_OBJECT   Object,
  IN UINTN          WideTag,
  IN UINTN          MinLength,
  OUT BORAX_RECORD  **Record
  )
{
  UINTN         Discrim;
  BORAX_RECORD  *TheRecord;

  Discrim = BORAX_DISCRIMINATE (Object);
  if (Discrim != WideTag) {
    DEBUG ((
      DEBUG_ERROR,
      "%a:%d: incorrect object type (%u != %u)\n",
      DebugFunc,
      DebugLine,
      Discrim,
      WideTag
      ));
    return EFI_INVALID_PARAMETER;
  }

  TheRecord = (BORAX_RECORD *)BORAX_GET_POINTER (Object);
  if (TheRecord->Length < MinLength) {
    DEBUG ((
      DEBUG_ERROR,
      "%a:%d: invalid object length (%u < %u)\n",
      DebugFunc,
      DebugLine,
      TheRecord->Length,
      MinLength
      ));
    return EFI_INVALID_PARAMETER;
  }

  *Record = TheRecord;
  return EFI_SUCCESS;
}

EFI_STATUS
EFIAPI
BoraxGcHookNoCopy (
  IN BORAX_ALLOCATOR       *Alloc,
  IN BORAX_OBJECT_HEADER   *OldObject,
  OUT BORAX_OBJECT_HEADER  **NewObject
  )
{
  *NewObject = NULL;
  return EFI_SUCCESS;
}

EFI_STATUS
EFIAPI
BoraxGcHookNoSubObjects (
  IN BORAX_OBJECT_HEADER          *Object,
  IN VOID                         *Ctx,
  IN BORAX_GC_SUBOBJECT_CALLBACK  Callback
  )
{
  return EFI_SUCCESS;
}

STATIC CONST BORAX_GC_HOOKS  gConsGcHooks = {
  .Copy       = &CopyCons,
  .SubObjects = &ConsSubObjects,
};

STATIC CONST BORAX_GC_HOOKS  gWordRecordGcHooks = {
  .Copy       = &CopyRecord,
  .SubObjects = &WordRecordSubObjects,
};

STATIC CONST BORAX_GC_HOOKS  gObjectRecordGcHooks = {
  .Copy       = &CopyRecord,
  .SubObjects = &ObjectRecordSubObjects,
};

STATIC CONST BORAX_GC_HOOKS  gWeakPointerGcHooks = {
  .Copy       = &CopyWeakPointer,
  .SubObjects = &BoraxGcHookNoSubObjects, // Handled in sweep
};

STATIC CONST BORAX_GC_HOOKS  gPinGcHooks = {
  .Copy       = &BoraxGcHookNoCopy, // Don't move pins
  .SubObjects = &PinSubObjects,
};

STATIC CONST BORAX_GC_HOOKS  gMovedGcHooks = {
  .Copy       = &BoraxGcHookNoCopy,       // Not an object
  .SubObjects = &BoraxGcHookNoSubObjects, // Not an object
};

STATIC EFI_STATUS
EFIAPI
GcHooks (
  IN BORAX_OBJECT_HEADER    *Object,
  OUT CONST BORAX_GC_HOOKS  **Hooks
  )
{
  switch (BORAX_DISCRIMINATE_POINTER (Object)) {
    case BORAX_DISCRIM_CONS:
      *Hooks = &gConsGcHooks;
      return EFI_SUCCESS;
    case BORAX_DISCRIM_WORD_RECORD:
      *Hooks =  &gWordRecordGcHooks;
      return EFI_SUCCESS;
    case BORAX_DISCRIM_OBJECT_RECORD:
      *Hooks = &gObjectRecordGcHooks;
      return EFI_SUCCESS;
    case BORAX_DISCRIM_BUILT_IN_FUNCTION:
      *Hooks = &gBuiltInFunctionGcHooks;
      return EFI_SUCCESS;
    case BORAX_DISCRIM_TASK:
      *Hooks = &gTaskGcHooks;
      return EFI_SUCCESS;
    case BORAX_DISCRIM_WEAK_POINTER:
      *Hooks = &gWeakPointerGcHooks;
      return EFI_SUCCESS;
    case BORAX_WIDETAG_PIN:
      *Hooks = &gPinGcHooks;
      return EFI_SUCCESS;
    case BORAX_WIDETAG_MOVED:
      *Hooks = &gMovedGcHooks;
      return EFI_SUCCESS;
    default:
      DEBUG ((DEBUG_ERROR, "gc not implemented for widetag %u\n", Object->WideTag));
      return EFI_INVALID_PARAMETER;
  }
}
