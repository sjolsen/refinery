#include <Library/BoraxObjectFile.h>

#include <Library/BaseMemoryLib.h>
#include <Library/MemoryAllocationLib.h>
#include <Library/SafeIntLib.h>
#include <Library/UefiBootServicesTableLib.h>

STATIC EFI_STATUS
EFIAPI
SafePageCountRoundingUp (
  IN UINTN   Bytes,
  OUT UINTN  *PageCount
  )
{
  EFI_STATUS  Status;

  Status = SafeUintnAdd (Bytes, BORAX_PAGE_SIZE - 1, PageCount);
  if (EFI_ERROR (Status)) {
    return Status;
  }

  *PageCount /= BORAX_PAGE_SIZE;
  return EFI_SUCCESS;
}

#define UNSAFE_PAGE_COUNT_ROUNDING_UP(_bytes) \
(((_bytes) + (BORAX_PAGE_SIZE - 1)) / BORAX_PAGE_SIZE)

typedef EFI_STATUS EFIAPI IO_CALLBACK(IN BORAX_STAGED_OBJECT_FILE  *Staged);

#define OBJECTS_PER_PAGE              (BORAX_PAGE_SIZE / BORAX_ALIGNMENT)
#define OBJECT_BITMAP_WORDS_PER_PAGE  (OBJECTS_PER_PAGE / BORAX_WORD_BITS)

struct _BORAX_STAGED_OBJECT_FILE_IMPL {
  BORAX_ALLOCATOR       *Alloc;
  EFI_FILE_PROTOCOL     *File;
  BOOLEAN               Cancelled;
  UINT64                FileSize;
  EFI_FILE_IO_TOKEN     IO;
  IO_CALLBACK           *IOCallback;
  BXO_HEADER            Header;
  BORAX_CONS_PAGE       *Cons;
  BORAX_OBJECT_CHUNK    *Object;
  UINTN                 *ObjectBitmap;
};

STATIC VOID
EFIAPI
BoraxStageObjectFileCleanup (
  IN BORAX_STAGED_OBJECT_FILE  *Staged
  )
{
  BORAX_STAGED_OBJECT_FILE_IMPL  *Impl = Staged->Impl;

  if (Impl != NULL) {
    if (Impl->ObjectBitmap != NULL) {
      FreePool (Impl->ObjectBitmap);
    }

    if (Impl->Object != NULL) {
      // The page-count calculation must not have overflowed for us to make it
      // here
      BoraxFreeExternalPages (
        Impl->Alloc,
        Impl->Object,
        UNSAFE_PAGE_COUNT_ROUNDING_UP (Impl->Header.Object.Size)
        );
    }

    if (Impl->Cons != NULL) {
      // The page-count calculation must not have overflowed for us to make it
      // here
      BoraxFreeExternalPages (
        Impl->Alloc,
        Impl->Cons,
        UNSAFE_PAGE_COUNT_ROUNDING_UP (Impl->Header.Cons.Size)
        );
    }

    if (Impl->IO.Event != NULL) {
      gBS->CloseEvent (Impl->IO.Event);
    }

    FreePool (Impl);
  }
}

STATIC VOID
EFIAPI
BoraxStageObjectFileFailure (
  IN BORAX_STAGED_OBJECT_FILE  *Staged,
  IN EFI_STATUS                Status
  )
{
  BoraxStageObjectFileCleanup (Staged);
  Staged->Status = Status;
  gBS->SignalEvent (Staged->Complete);
}

STATIC VOID
EFIAPI
BoraxStageObjectFileSuccess (
  IN BORAX_STAGED_OBJECT_FILE  *Staged
  )
{
  // Cleanup will be performed by UnStage/Inject
  Staged->Status = EFI_SUCCESS;
  gBS->SignalEvent (Staged->Complete);
}

STATIC EFI_STATUS
EFIAPI
BoraxStageObjectFileAllocateChunk (
  IN BORAX_STAGED_OBJECT_FILE  *Staged,
  IN UINTN                     Bytes,
  OUT VOID                     **Chunk
  )
{
  EFI_STATUS  Status;
  UINTN       PageCount;

  Status = SafePageCountRoundingUp (Bytes, &PageCount);
  if (EFI_ERROR (Status)) {
    return Status;
  }

  *Chunk = BoraxAllocateExternalPages (Staged->Impl->Alloc, PageCount);
  if (*Chunk == NULL) {
    return EFI_OUT_OF_RESOURCES;
  }

  return EFI_SUCCESS;
}

STATIC EFI_STATUS
EFIAPI
BoraxStageObjectFileRead (
  IN BORAX_STAGED_OBJECT_FILE  *Staged,
  OUT VOID                     *Buffer,
  IN UINT64                    Offset,
  IN UINTN                     Size
  )
{
  EFI_STATUS         Status;
  EFI_FILE_PROTOCOL  *File = Staged->Impl->File;
  EFI_FILE_IO_TOKEN  *IO   = &Staged->Impl->IO;

  if (Offset + Size > Staged->Impl->FileSize) {
    return EFI_END_OF_FILE;
  }

  Status = File->SetPosition (File, Offset);
  if (EFI_ERROR (Status)) {
    return Status;
  }

  if (File->Revision >= 2) {
    IO->BufferSize = Size;
    IO->Buffer     = Buffer;
    return File->ReadEx (File, IO);
  } else {
    // Blocking read if ReadEx is not available
    IO->Status = File->Read (File, &Size, Buffer);
    gBS->SignalEvent (IO->Event);
    return EFI_SUCCESS;
  }
}

STATIC VOID
EFIAPI
BoraxStageObjectFileIOCallback (
  IN EFI_EVENT  Event,
  IN VOID       *Context
  )
{
  EFI_STATUS                Status;
  BORAX_STAGED_OBJECT_FILE  *Staged = Context;

  // Check for failure or cancellation
  if (EFI_ERROR (Staged->Impl->IO.Status)) {
    BoraxStageObjectFileFailure (Staged, Staged->Impl->IO.Status);
    return;
  }

  if (Staged->Impl->Cancelled) {
    BoraxStageObjectFileFailure (Staged, EFI_ABORTED);
    return;
  }

  // Execute the callback
  Status = Staged->Impl->IOCallback (Staged);
  if (EFI_ERROR (Status)) {
    BoraxStageObjectFileFailure (Staged, Status);
    return;
  }
}

STATIC IO_CALLBACK  BoraxStageObjectFile1;
STATIC IO_CALLBACK  BoraxStageObjectFile2;
STATIC IO_CALLBACK  BoraxStageObjectFile3;

VOID
EFIAPI
BoraxStageObjectFile (
  IN BORAX_ALLOCATOR            *Alloc,
  IN EFI_FILE_PROTOCOL          *File,
  OUT BORAX_STAGED_OBJECT_FILE  *Staged
  )
{
  EFI_STATUS                     Status;
  BORAX_STAGED_OBJECT_FILE_IMPL  *Impl;

  // Initialize private state. The Impl struct contains all state that must be
  // carried across I/O callbacks and is set up so that resources are released
  // when their pointers are non-NULL. This simplifies the shared cleanup code.
  Impl = AllocateZeroPool (sizeof (BORAX_STAGED_OBJECT_FILE_IMPL));
  if (Impl == NULL) {
    BoraxStageObjectFileFailure (Staged, EFI_OUT_OF_RESOURCES);
    return;
  }

  Staged->Impl = Impl;
  Impl->Alloc  = Alloc;
  Impl->File   = File;

  // Set up I/O events to complete in callback mode
  Status = gBS->CreateEvent (
                  EVT_NOTIFY_SIGNAL,
                  TPL_CALLBACK,
                  BoraxStageObjectFileIOCallback,
                  Staged,
                  &Impl->IO.Event
                  );
  if (EFI_ERROR (Status)) {
    BoraxStageObjectFileFailure (Staged, Status);
    return;
  }

  // Probe the file size
  Status = File->SetPosition (File, MAX_UINT64);
  if (EFI_ERROR (Status)) {
    BoraxStageObjectFileFailure (Staged, Status);
    return;
  }

  Status = File->GetPosition (File, &Impl->FileSize);
  if (EFI_ERROR (Status)) {
    BoraxStageObjectFileFailure (Staged, Status);
    return;
  }

  // Kick off the read of the file header
  Impl->IOCallback = BoraxStageObjectFile1;
  Status           = BoraxStageObjectFileRead (
                       Staged,
                       &Impl->Header,
                       0,
                       sizeof (Impl->Header)
                       );
  if (EFI_ERROR (Status)) {
    BoraxStageObjectFileFailure (Staged, Status);
    return;
  }
}

STATIC CONST UINT8  BXO_64BIT_LE[8] = {
  BXO_MAGIC0,
  BXO_MAGIC1,
  BXO_MAGIC2,
  BXO_MAGIC3,
  BXO_64BIT,
  BXO_LITTLE_ENDIAN,
  0x00,              // Version
  0x00,              // Padding
};

STATIC EFI_STATUS
EFIAPI
BoraxStageObjectFile1 (
  IN BORAX_STAGED_OBJECT_FILE  *Staged
  )
{
  EFI_STATUS                     Status;
  BORAX_STAGED_OBJECT_FILE_IMPL  *Impl = Staged->Impl;

  // Parse the file header (assume 64-bit little-endian for now)
  if (CompareMem (&Impl->Header, BXO_64BIT_LE, 8) != 0) {
    return EFI_LOAD_ERROR;
  }

  // No support for relocations yet
  if (Impl->Header.String.Size != 0) {
    return EFI_LOAD_ERROR;
  }

  if (Impl->Header.Package.Size != 0) {
    return EFI_LOAD_ERROR;
  }

  if (Impl->Header.Symbol.Size != 0) {
    return EFI_LOAD_ERROR;
  }

  if (Impl->Header.Class.Size != 0) {
    return EFI_LOAD_ERROR;
  }

  // Allocate the cons and object chunks
  Status = BoraxStageObjectFileAllocateChunk (
             Staged,
             Impl->Header.Cons.Size,
             (VOID **)&Impl->Cons
             );
  if (EFI_ERROR (Status)) {
    return Status;
  }

  Status = BoraxStageObjectFileAllocateChunk (
             Staged,
             Impl->Header.Object.Size,
             (VOID **)&Impl->Object
             );
  if (EFI_ERROR (Status)) {
    return Status;
  }

  Impl->ObjectBitmap = AllocateZeroPool (
                         OBJECT_BITMAP_WORDS_PER_PAGE *
                         UNSAFE_PAGE_COUNT_ROUNDING_UP (Impl->Header.Object.Size)
                         );
  if (Impl->ObjectBitmap == NULL) {
    return EFI_OUT_OF_RESOURCES;
  }

  // Read the cons chunk
  Impl->IOCallback = BoraxStageObjectFile2;
  return BoraxStageObjectFileRead (
           Staged,
           Impl->Cons,
           Impl->Header.Cons.Offset,
           Impl->Header.Cons.Size
           );
}

STATIC EFI_STATUS
EFIAPI
BoraxStageObjectFile2 (
  IN BORAX_STAGED_OBJECT_FILE  *Staged
  )
{
  BORAX_STAGED_OBJECT_FILE_IMPL  *Impl = Staged->Impl;

  // Read the object chunk
  Impl->IOCallback = BoraxStageObjectFile3;
  return BoraxStageObjectFileRead (
           Staged,
           Impl->Object,
           Impl->Header.Object.Offset,
           Impl->Header.Object.Size
           );
}

STATIC EFI_STATUS
EFIAPI
BoraxStageObjectFile3 (
  IN BORAX_STAGED_OBJECT_FILE  *Staged
  )
{
  // Done with I/O
  BoraxStageObjectFileSuccess (Staged);
  return EFI_SUCCESS;
}

VOID
EFIAPI
BoraxCancelObjectFile (
  IN BORAX_STAGED_OBJECT_FILE  *Staged
  )
{
  Staged->Impl->Cancelled = TRUE;
}

VOID
EFIAPI
BoraxUnStageObjectFile (
  IN BORAX_STAGED_OBJECT_FILE  *Staged
  )
{
  BoraxStageObjectFileCleanup (Staged);
}

STATIC EFI_STATUS
EFIAPI
ScanObjects (
  IN BORAX_STAGED_OBJECT_FILE  *Staged
  )
{
  EFI_STATUS                     Status;
  BORAX_STAGED_OBJECT_FILE_IMPL  *Impl = Staged->Impl;
  UINTN                          Index = BORAX_OBJECT_FIRST_INDEX;

  while (Index < Impl->Header.Object.Size) {
    BORAX_OBJECT_HEADER  *Header = (BORAX_OBJECT_HEADER *)
                                   ((CHAR8 *)Impl->Object + Index);
    UINTN  Word = Index / BORAX_WORD_BITS;
    UINTN  Bit  = Index % BORAX_WORD_BITS;

    switch (BORAX_DISCRIMINATE_POINTER (Header)) {
      case BORAX_DISCRIM_WORD_RECORD:
      case BORAX_DISCRIM_OBJECT_RECORD:
      {
        BORAX_RECORD  *Record = (BORAX_RECORD *)Header;
        UINTN         Size;

        // Bounds check the record object header
        if ((Impl->Header.Object.Size - Index) < sizeof (BORAX_RECORD)) {
          return EFI_BUFFER_TOO_SMALL;
        }

        // Find the end of the record object
        Status = SafeUintnMult (sizeof (UINTN), Record->Length, &Size);
        if (EFI_ERROR (Status)) {
          return Status;
        }

        Status = SafeUintnAdd (Size, sizeof (BORAX_RECORD), &Size);
        if (EFI_ERROR (Status)) {
          return Status;
        }

        Status = SafeUintnAdd (Index, Size, &Index);
        if (EFI_ERROR (Status)) {
          return Status;
        }

        Impl->ObjectBitmap[Word] |= 1 << Bit;
        Index                     = BORAX_ALIGN (Index);
        break;
      }
      case BORAX_DISCRIM_UNINITIALIZED:
        // We're done early
        return EFI_SUCCESS;
      default:
        // Bad object
        return EFI_LOAD_ERROR;
    }
  }

  return EFI_SUCCESS;
}

#define TAG_SHIFT  (BORAX_WORD_BITS - 3)
#define TAG_MASK   0x7

#define OFFSET_SHIFT  3
#define OFFSET_MASK   ((1UL << (BORAX_WORD_BITS - 6)) - 1)

STATIC EFI_STATUS
EFIAPI
TranslateObject (
  IN BORAX_STAGED_OBJECT_FILE  *Staged,
  IN OUT BORAX_OBJECT          *Object
  )
{
  BORAX_STAGED_OBJECT_FILE_IMPL  *Impl = Staged->Impl;

  if (BORAX_IS_POINTER (*Object)) {
    UINTN  Address = (UINTN)BORAX_GET_POINTER (*Object);
    UINTN  Tag     = TAG_MASK & (Address >> TAG_SHIFT);
    UINTN  Offset  = OFFSET_MASK & (Address >> OFFSET_SHIFT);

    switch (Tag) {
      case BXO_SECTION_CONS:
        Offset <<= 3;
        // Must point to valid memory
        if (Offset >= Impl->Header.Cons.Size) {
          return EFI_BUFFER_TOO_SMALL;
        }

        // Must not point into a page header
        if ((Offset % BORAX_PAGE_SIZE) < BORAX_CONS_FIRST_INDEX) {
          return EFI_LOAD_ERROR;
        }

        // Must be aligned
        if ((Offset % BORAX_ALIGNMENT) != 0) {
          return EFI_LOAD_ERROR;
        }

        *Object = BORAX_MAKE_POINTER ((CHAR8 *)Impl->Cons + Offset);
        return EFI_SUCCESS;

      case BXO_SECTION_OBJECT:
        Offset <<= 3;
        // Must point to valid memory
        if (Offset >= Impl->Header.Object.Size) {
          return EFI_BUFFER_TOO_SMALL;
        }

        // Must be aligned
        if ((Offset % BORAX_ALIGNMENT) != 0) {
          return EFI_LOAD_ERROR;
        }

        // Must point to a valid object
        {
          UINTN  Index = Offset / BORAX_ALIGNMENT;
          UINTN  Word  = Index / BORAX_WORD_BITS;
          UINTN  Bit   = Index % BORAX_WORD_BITS;

          if (!(Impl->ObjectBitmap[Word] & (1 << Bit))) {
            return EFI_LOAD_ERROR;
          }
        }

        *Object = BORAX_MAKE_POINTER ((CHAR8 *)Impl->Object + Offset);
        return EFI_SUCCESS;

      case BXO_SECTION_STRING:
      case BXO_SECTION_REL_PACKAGE:
      case BXO_SECTION_REL_SYMBOL:
      case BXO_SECTION_REL_CLASS:
      // Relocations not supported yet
      default:
        return EFI_LOAD_ERROR;
    }
  } else {
    return EFI_SUCCESS;
  }
}

STATIC EFI_STATUS
EFIAPI
TranslateConsSection (
  IN BORAX_STAGED_OBJECT_FILE  *Staged
  )
{
  EFI_STATUS                     Status;
  BORAX_STAGED_OBJECT_FILE_IMPL  *Impl = Staged->Impl;

  UINTN  PageCount = UNSAFE_PAGE_COUNT_ROUNDING_UP (Impl->Header.Cons.Size);
  UINTN  Page;

  for (Page = 0; Page < PageCount; ++Page) {
    UINTN  PageOffset;
    for (PageOffset = BORAX_CONS_FIRST_INDEX;
         PageOffset < BORAX_PAGE_SIZE;
         PageOffset += sizeof (BORAX_CONS))
    {
      UINTN       Offset = Page * BORAX_PAGE_SIZE + PageOffset;
      BORAX_CONS  *Cons  = (BORAX_CONS *)((CHAR8 *)Impl->Cons + Offset);
      if (Offset >= Impl->Header.Cons.Size) {
        // The rest of the page is uninitialized
        return EFI_SUCCESS;
      }

      Status = TranslateObject (Staged, &Cons->Car);
      if (EFI_ERROR (Status)) {
        return Status;
      }

      Status = TranslateObject (Staged, &Cons->Cdr);
      if (EFI_ERROR (Status)) {
        return Status;
      }
    }
  }

  return EFI_SUCCESS;
}

STATIC EFI_STATUS
EFIAPI
TranslateObjectSection (
  IN BORAX_STAGED_OBJECT_FILE  *Staged
  )
{
  EFI_STATUS                     Status;
  BORAX_STAGED_OBJECT_FILE_IMPL  *Impl = Staged->Impl;
  UINTN                          Index = BORAX_OBJECT_FIRST_INDEX;

  while (Index < Impl->Header.Object.Size) {
    BORAX_OBJECT_HEADER  *Header = (BORAX_OBJECT_HEADER *)
                                   ((CHAR8 *)Impl->Object + Index);

    switch (BORAX_DISCRIMINATE_POINTER (Header)) {
      case BORAX_DISCRIM_WORD_RECORD:
      case BORAX_DISCRIM_OBJECT_RECORD:
      {
        // We already bounds-checked everything in ScanObjects
        BORAX_RECORD  *Record = (BORAX_RECORD *)Header;

        Status = TranslateObject (Staged, &Record->Class);
        if (EFI_ERROR (Status)) {
          return Status;
        }

        if (Header->WideTag == BORAX_WIDETAG_OBJECT_RECORD) {
          UINTN  I;
          for (I = 0; I < Record->Length; ++I) {
            Status = TranslateObject (Staged, &Record->Data[I]);
            if (EFI_ERROR (Status)) {
              return Status;
            }
          }
        }

        Index += sizeof (BORAX_RECORD) + sizeof (UINTN) * Record->Length;
        Index  = BORAX_ALIGN (Index);
        break;
      }
      case BORAX_DISCRIM_UNINITIALIZED:
        // We're done early
        return EFI_SUCCESS;
      default:
        // Bad object
        return EFI_LOAD_ERROR;
    }
  }

  return EFI_SUCCESS;
}

EFI_STATUS
EFIAPI
BoraxInjectObjectFile (
  IN BORAX_STAGED_OBJECT_FILE  *Staged,
  OUT BORAX_PIN                **Pin
  )
{
  EFI_STATUS                     Status;
  BORAX_STAGED_OBJECT_FILE_IMPL  *Impl = Staged->Impl;

  // Scan the object chunk for valid objects
  Status = ScanObjects (Staged);
  if (EFI_ERROR (Status)) {
    goto end;
  }

  // Fix up pointers
  Status = TranslateObject (Staged, &Impl->Header.RootObject);
  if (EFI_ERROR (Status)) {
    goto end;
  }

  Status = TranslateConsSection (Staged);
  if (EFI_ERROR (Status)) {
    goto end;
  }

  Status = TranslateObjectSection (Staged);
  if (EFI_ERROR (Status)) {
    goto end;
  }

  // Inject the pages
  BoraxInjectExternalConsPages (
    Impl->Alloc,
    Impl->Cons,
    UNSAFE_PAGE_COUNT_ROUNDING_UP (Impl->Header.Cons.Size)
    );

  Impl->Cons = NULL;

  Status = BoraxInjectExternalObjectPages (
             Impl->Alloc,
             Impl->Object,
             UNSAFE_PAGE_COUNT_ROUNDING_UP (Impl->Header.Object.Size)
             );
  if (EFI_ERROR (Status)) {
    goto end;
  }

  Impl->Object = NULL;

  // Pin the root object
  Status = BoraxAllocatePin (Impl->Alloc, Impl->Header.RootObject, Pin);

end:
  BoraxStageObjectFileCleanup (Staged);
  return Status;
}

EFI_STATUS
EFIAPI
BoraxLoadObjectFile (
  IN BORAX_ALLOCATOR    *Alloc,
  IN EFI_FILE_PROTOCOL  *File,
  OUT BORAX_PIN         **Pin
  )
{
  EFI_STATUS                Status;
  BORAX_STAGED_OBJECT_FILE  Staged;
  UINTN                     Index;

  SetMem (&Staged, sizeof (Staged), 0);

  Status = gBS->CreateEvent (0, 0, NULL, NULL, &Staged.Complete);
  if (EFI_ERROR (Status)) {
    goto end;
  }

  BoraxStageObjectFile (Alloc, File, &Staged);
  Status = gBS->WaitForEvent (1, &Staged.Complete, &Index);
  if (EFI_ERROR (Status)) {
    goto end;
  }

  Status = Staged.Status;
  if (EFI_ERROR (Status)) {
    goto end;
  }

  Status = BoraxInjectObjectFile (&Staged, Pin);

end:
  if (Staged.Complete != NULL) {
    gBS->CloseEvent (Staged.Complete);
  }

  return Status;
}
