#include <Library/BoraxMemory.h>

#include <Library/BaseMemoryLib.h>
#include <Library/DebugLib.h>

#include "GreyList.h"

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
    InternalFreePages (Alloc, ConsPage, 1);
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
  BORAX_PIN  *Pin;

  // We may have aborted in the middle of a cycle, so clean both spaces
  ClearSpace (Alloc, &Alloc->FromSpace);
  ClearSpace (Alloc, &Alloc->ToSpace);

  // Free pin objects
  Pin = Alloc->Pins;
  while (Pin != NULL) {
    BORAX_PIN  *Next = Pin->Next;
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
MarkObjectIfWhite (
  IN BORAX_ALLOCATOR      *Alloc,
  IN BORAX_GREY_LIST      *GreyList,
  IN BORAX_OBJECT_HEADER  *Object
  )
{
  EFI_STATUS           Status;
  UINTN                GcData;
  BORAX_OBJECT_HEADER  *NewObj = NULL;

  GcData = GetObjectGcData (Alloc, Object);
  if (DecodeColor (Alloc, GcData) != WHITE) {
    // Nothing to do
    return EFI_SUCCESS;
  }

  // Copy from FromSpace to ToSpace
  switch (BORAX_DISCRIMINATE_POINTER (Object)) {
    case BORAX_DISCRIM_CONS:
    {
      BORAX_CONS  *OldCons = (BORAX_CONS *)Object;
      Status = BoraxAllocateCons (
                 Alloc,
                 OldCons->Car,
                 OldCons->Cdr,
                 (BORAX_CONS **)&NewObj
                 );
      if (EFI_ERROR (Status)) {
        return Status;
      }

      // Future reads to Object will interpret it as a "moved" object and will
      // access GcData instead of the bitmap.
      Object->WideTag        = BORAX_WIDETAG_MOVED;
      Object->HeaderWords[1] = BORAX_MAKE_POINTER (NewObj);
      break;
    }
    // TODO: Implement move optimization for large objects
    case BORAX_DISCRIM_STRING:
    {
      BORAX_STRING  *String = (BORAX_STRING *)Object;
      BORAX_STRING  *NewStr;

      Status = BoraxAllocateString (Alloc, String->Length, &NewStr);
      if (EFI_ERROR (Status)) {
        return Status;
      }

      NewObj = &NewStr->Header;
      CopyMem (NewStr->Data, String->Data, 2 * String->Length);
      Object->WideTag        = BORAX_WIDETAG_MOVED;
      Object->HeaderWords[1] = BORAX_MAKE_POINTER (NewObj);
      break;
    }
    case BORAX_DISCRIM_VECTOR:
    {
      BORAX_VECTOR  *Vector = (BORAX_VECTOR *)Object;
      BORAX_VECTOR  *NewVec;

      Status = BoraxAllocateVectorUninitialized (
                 Alloc,
                 Vector->Length,
                 &NewVec
                 );
      if (EFI_ERROR (Status)) {
        return Status;
      }

      NewObj = &NewVec->Header;
      CopyMem (
        NewVec->Data,
        Vector->Data,
        sizeof (BORAX_OBJECT) * Vector->Length
        );
      Object->WideTag        = BORAX_WIDETAG_MOVED;
      Object->HeaderWords[1] = BORAX_MAKE_POINTER (NewObj);
      break;
    }
    case BORAX_DISCRIM_RECORD:
    {
      BORAX_RECORD  *Record = (BORAX_RECORD *)Object;
      BORAX_RECORD  *NewRec;

      Status = BoraxAllocateRecord (
                 Alloc,
                 Record->Type,
                 Record->Length,
                 &NewRec
                 );
      if (EFI_ERROR (Status)) {
        return Status;
      }

      NewObj = &NewRec->Header;
      CopyMem (
        NewRec->Slots,
        Record->Slots,
        sizeof (BORAX_OBJECT) * Record->Length
        );
      Object->WideTag        = BORAX_WIDETAG_MOVED;
      Object->HeaderWords[1] = BORAX_MAKE_POINTER (NewObj);
      break;
    }
    case BORAX_DISCRIM_WEAK_POINTER:
    {
      BORAX_WEAK_POINTER  *Wp = (BORAX_WEAK_POINTER *)Object;
      Status = BoraxAllocateWeakPointer (
                 Alloc,
                 Wp->Value,
                 (BORAX_WEAK_POINTER **)&NewObj
                 );
      if (EFI_ERROR (Status)) {
        return Status;
      }

      Object->WideTag        = BORAX_WIDETAG_MOVED;
      Object->HeaderWords[1] = BORAX_MAKE_POINTER (NewObj);
      break;
    }
    case BORAX_DISCRIM_PIN:   // Don't move pins
    case BORAX_DISCRIM_MOVED: // Not an object
      break;
    default:
      // If a new widetag is added, we need to add support for it
      DEBUG ((
        DEBUG_ERROR,
        "%a: gc not implemented for widetag (%u)\n",
        __func__,
        Object->WideTag
        ));
      return EFI_INVALID_PARAMETER;
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
    Status  = SetObjectGcData (Alloc, NewObj, GcData);
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

  switch (BORAX_DISCRIMINATE_POINTER (Object)) {
    case BORAX_DISCRIM_CONS:
    {
      BORAX_CONS  *Cons = (BORAX_CONS *)Object;

      Status = MarkObjectWordIfWhite (Alloc, GreyList, Cons->Car);
      if (EFI_ERROR (Status)) {
        return Status;
      }

      // Mark CDR last to ensure it gets copied first
      Status = MarkObjectWordIfWhite (Alloc, GreyList, Cons->Cdr);
      return Status;
    }
    case BORAX_DISCRIM_PIN:
    {
      BORAX_PIN  *Pin = (BORAX_PIN *)Object;
      Status = MarkObjectWordIfWhite (Alloc, GreyList, Pin->Object);
      return Status;
    }
    case BORAX_DISCRIM_VECTOR:
    {
      BORAX_VECTOR  *Vector = (BORAX_VECTOR *)Object;
      UINTN         I;

      for (I = 0; I < Vector->Length; ++I) {
        Status = MarkObjectWordIfWhite (Alloc, GreyList, Vector->Data[I]);
        if (EFI_ERROR (Status)) {
          return Status;
        }
      }

      return EFI_SUCCESS;
    }
    case BORAX_DISCRIM_RECORD:
    {
      BORAX_RECORD  *Record = (BORAX_RECORD *)Object;
      UINTN         I;

      Status = MarkObjectWordIfWhite (Alloc, GreyList, Record->Type);
      if (EFI_ERROR (Status)) {
        return Status;
      }

      for (I = 0; I < Record->Length; ++I) {
        Status = MarkObjectWordIfWhite (Alloc, GreyList, Record->Slots[I]);
        if (EFI_ERROR (Status)) {
          return Status;
        }
      }

      return EFI_SUCCESS;
    }
    case BORAX_DISCRIM_STRING:
    case BORAX_DISCRIM_WEAK_POINTER:
    case BORAX_DISCRIM_MOVED:
      // Nothing to do
      return EFI_SUCCESS;
    default:
      // If a new widetag is added, we need to add support for it
      DEBUG ((
        DEBUG_ERROR,
        "%a: gc not implemented for widetag (%u)\n",
        __func__,
        Object->WideTag
        ));
      return EFI_INVALID_PARAMETER;
  }
}

STATIC VOID
EFIAPI
UpdateIfMoved (
  IN OUT BORAX_OBJECT  *Object
  )
{
  if (BORAX_DISCRIMINATE (*Object) == BORAX_DISCRIM_MOVED) {
    *Object = BORAX_GET_POINTER (*Object)->HeaderWords[1];
  }
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

  switch (BORAX_DISCRIMINATE_POINTER (Object)) {
    case BORAX_DISCRIM_CONS:
    {
      BORAX_CONS  *Cons = (BORAX_CONS *)Object;

      UpdateIfMoved (&Cons->Car);
      UpdateIfMoved (&Cons->Cdr);
      break;
    }
    case BORAX_DISCRIM_PIN:
    {
      BORAX_PIN  *Pin = (BORAX_PIN *)Object;
      UpdateIfMoved (&Pin->Object);
      break;
    }
    case BORAX_DISCRIM_WEAK_POINTER:
    {
      BORAX_WEAK_POINTER  *Wp = (BORAX_WEAK_POINTER *)Object;
      UpdateIfMoved (&Wp->Value);
      break;
    }
    case BORAX_DISCRIM_VECTOR:
    {
      BORAX_VECTOR  *Vector = (BORAX_VECTOR *)Object;
      UINTN         I;

      for (I = 0; I < Vector->Length; ++I) {
        UpdateIfMoved (&Vector->Data[I]);
      }

      break;
    }
    case BORAX_DISCRIM_RECORD:
    {
      BORAX_RECORD  *Record = (BORAX_RECORD *)Object;
      UINTN         I;

      UpdateIfMoved (&Record->Type);
      for (I = 0; I < Record->Length; ++I) {
        UpdateIfMoved (&Record->Slots[I]);
      }

      break;
    }
    case BORAX_DISCRIM_STRING: // No subobjects
    case BORAX_DISCRIM_MOVED:  // Not an object
      break;
    default:
      // If a new widetag is added, we need to add support for it
      DEBUG ((
        DEBUG_ERROR,
        "%a: gc not implemented for widetag (%u)\n",
        __func__,
        Object->WideTag
        ));
  }

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
  UINTN      GcData;
  BORAX_PIN  **Iter;
  BORAX_PIN  *Pin;

  Iter = &Alloc->Pins;
  while (*Iter != NULL) {
    Pin    = *Iter;
    GcData = GetObjectGcData (Alloc, &Pin->Header);

    switch (DecodeColor (Alloc, GcData)) {
      case WHITE:
        *Iter = Pin->Next;
        InternalFreePool (Alloc, Pin);
        break;
      case GREY:
        DEBUG ((DEBUG_ERROR, "grey pin found during sweep\n"));
        return EFI_INVALID_PARAMETER;
      case BLACK:
        Iter = &Pin->Next;
        break;
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
  BoraxGreyListCleanup (&GreyList);
  return Status;
}

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

  if ((Page == NULL) || (Alloc->ToSpace.Cons.FillIndex == BORAX_PAGE_SIZE)) {
    // No page or page is full; allocate one
    Page = InternalAllocatePages (Alloc, 1);
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
  (*Cons)->Car                   = Car;
  (*Cons)->Cdr                   = Cdr;
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
  NewPin = InternalAllocatePool (Alloc, sizeof (*NewPin));
  if (NewPin == NULL) {
    DEBUG ((DEBUG_ERROR, "%a: out of memory\n", __func__));
    return EFI_OUT_OF_RESOURCES;
  }

  // Initialize the pin and add it to the list
  NewPin->Header.WideTag = BORAX_WIDETAG_PIN;
  NewPin->Header.GcData  = Alloc->ToSpaceParity;
  NewPin->Next           = Alloc->Pins;
  Alloc->Pins            = NewPin;
  NewPin->Object         = Object;

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
  NewWp->Header.WideTag       = BORAX_WIDETAG_WEAK_POINTER;
  NewWp->Value                = Object;
  NewWp->Next                 = Alloc->ToSpace.WeakPointers;
  Alloc->ToSpace.WeakPointers = NewWp;

  *WeakPointer = NewWp;
  return EFI_SUCCESS;
}

EFI_STATUS
EFIAPI
BoraxAllocateString (
  IN BORAX_ALLOCATOR  *Alloc,
  IN UINTN            Length,
  OUT BORAX_STRING    **String
  )
{
  EFI_STATUS    Status;
  BORAX_STRING  *NewString;

  // Allocate a regular lisp object
  Status = BoraxAllocateObject (
             Alloc,
             sizeof (BORAX_STRING) + 2 * Length,
             (BORAX_OBJECT_HEADER **)&NewString
             );
  if (EFI_ERROR (Status)) {
    return Status;
  }

  // Initialize the string
  NewString->Header.WideTag = BORAX_WIDETAG_STRING;
  NewString->Length         = Length;
  SetMem (NewString->Data, 2 * Length, 0);

  *String = NewString;
  return EFI_SUCCESS;
}

EFI_STATUS
EFIAPI
BoraxAllocateVector (
  IN BORAX_ALLOCATOR  *Alloc,
  IN UINTN            Length,
  IN BORAX_OBJECT     InitialElement,
  OUT BORAX_VECTOR    **Vector
  )
{
  EFI_STATUS  Status;

  Status = BoraxAllocateVectorUninitialized (Alloc, Length, Vector);
  if (EFI_ERROR (Status)) {
    return Status;
  }

  SetMemN ((*Vector)->Data, sizeof (BORAX_OBJECT) * Length, InitialElement);
  return EFI_SUCCESS;
}

EFI_STATUS
EFIAPI
BoraxAllocateVectorUninitialized (
  IN BORAX_ALLOCATOR  *Alloc,
  IN UINTN            Length,
  OUT BORAX_VECTOR    **Vector
  )
{
  EFI_STATUS    Status;
  BORAX_VECTOR  *NewVector;

  // Allocate a regular lisp object
  Status = BoraxAllocateObject (
             Alloc,
             sizeof (BORAX_VECTOR) + sizeof (BORAX_OBJECT) * Length,
             (BORAX_OBJECT_HEADER **)&NewVector
             );
  if (EFI_ERROR (Status)) {
    return Status;
  }

  // Initialize the vector
  NewVector->Header.WideTag = BORAX_WIDETAG_VECTOR;
  NewVector->Length         = Length;

  *Vector = NewVector;
  return EFI_SUCCESS;
}

EFI_STATUS
EFIAPI
BoraxAllocateRecord (
  IN BORAX_ALLOCATOR  *Alloc,
  IN BORAX_OBJECT     Type,
  IN UINTN            Length,
  OUT BORAX_RECORD    **Record
  )
{
  EFI_STATUS    Status;
  BORAX_RECORD  *NewRecord;

  // Halfword length limits the possible size of the record
  if (Length > BORAX_HALFWORD_MAX) {
    DEBUG ((DEBUG_ERROR, "%a: oversized record (%u)\n", __func__, Length));
    return EFI_INVALID_PARAMETER;
  }

  // Allocate a regular lisp object
  Status = BoraxAllocateObject (
             Alloc,
             sizeof (BORAX_RECORD) + sizeof (BORAX_OBJECT) * Length,
             (BORAX_OBJECT_HEADER **)&NewRecord
             );
  if (EFI_ERROR (Status)) {
    return Status;
  }

  // Initialize the record
  NewRecord->Header.WideTag = BORAX_WIDETAG_RECORD;
  NewRecord->Length         = Length;
  NewRecord->Type           = Type;
  SetMemN (
    NewRecord->Slots,
    sizeof (BORAX_OBJECT) * Length,
    BORAX_IMMEDIATE_UNBOUND
    );

  *Record = NewRecord;
  return EFI_SUCCESS;
}
