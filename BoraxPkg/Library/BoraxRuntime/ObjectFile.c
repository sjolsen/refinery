#include <Library/BoraxObjectFile.h>

#include <Library/SafeIntLib.h>

typedef EFI_STATUS (*EFIAPI IO_CALLBACK)(IN BORAX_STAGED_OBJECT_FILE  *Staged);

typedef struct {
  UINTN    SectionIndex;
  UINTN    SectionChunkIndex;
  UINTN    FirstPageIndex;
  UINTN    PageCount;
  VOID     *Pages;
  UINTN    *ObjectBitmap;
} CHUNK_DATA;

#define OBJECTS_PER_PAGE              (BORAX_PAGE_SIZE / (2 * sizeof (UINTN)))
#define OBJECT_BITMAP_WORDS_PER_PAGE  (OBJECTS_PER_PAGE / BORAX_WORD_BITS)

struct _BORAX_STAGED_OBJECT_FILE_IMPL {
  BORAX_ALLOCATOR       *Alloc;
  EFI_FILE_PROTOCOL     *File;
  BOOLEAN               Cancelled;
  UINT64                FileSize;
  EFI_FILE_IO_TOKEN     IO;
  IO_CALLBACK           IOCallback;
  BXO_HEADER            Header;
  BXO_SECTION_HEADER    *SectionHeaders;
  CHUNK_DATA            *ChunkData;
  UINTN                 ChunkCount;
  UINTN                 LoadChunkIndex;
};

STATIC VOID
EFIAPI
BoraxStageObjectFileCleanup (
  IN BORAX_STAGED_OBJECT_FILE  *Staged
  )
{
  BORAX_STAGED_OBJECT_FILE_IMPL  *Impl = Staged->Impl;

  if (Impl != NULL) {
    if (Impl->ChunkData != NULL) {
      UINTN  I;
      for (I = 0; I < Impl->ChunkCount; ++I) {
        if (Impl->ChunkData[I].Pages != NULL) {
          UINTN  SectionIndex = Impl->ChunkData[I].SectionIndex;
          UINTN  PageCount    = Impl->ChunkData[I].PageCount;

          switch (Impl->SectionHeaders[SectionIndex].Tag) {
            case BXO_SECTION_CONS:
            case BXO_SECTION_OBJECT:
              Impl->Alloc->SysAlloc->FreePages (
                                       Impl->Alloc->SysAlloc,
                                       Impl->ChunkData[I].Pages,
                                       PageCount
                                       );
              break;
            default:
              FreePages (Impl->ChunkData[I].Pages, PageCount);
              break;
          }
        }

        if (Impl->ChunkData[I].ObjectBitmap != NULL) {
          FreePool (Impl->ChunkData[I].ObjectBitmap);
        }
      }

      FreePool (Impl->ChunkData);
    }

    if (Impl->SectionHeaders != NULL) {
      FreePool (Impl->SectionHeaders);
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
    IO->Status = File->Read (File, Size, Buffer);
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
    return Staged->Impl->IO.Status;
  }

  if (Staged->Impl->Cancelled) {
    return EFI_ABORTED;
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

  Staged->Alloc = Alloc;
  Staged->File  = File;
  Staged->Impl  = Impl;

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
BoraxStageObjectFile1 (
  IN BORAX_STAGED_OBJECT_FILE  *Staged
  )
{
  EFI_STATUS                     Status;
  BORAX_STAGED_OBJECT_FILE_IMPL  *Impl = Staged->Impl;
  UINTN                          SectionHeaderBytes;

  // Parse the file header (assume 64-bit little-endian for now)
  if (CompareMem (&Impl->Header, BXO_64BIT_LE, 8) != 0) {
    return EFI_LOAD_ERROR;
  }

  // Load the section headers
  Status = SafeUintnMult (
             sizeof (BXO_SECTION_HEADER),
             Impl->Header.SectionHeaderCount,
             &SectionHeaderBytes
             );
  if (EFI_ERROR (Status)) {
    return Status;
  }

  Impl->SectionHeaders = AllocatePool (SectionHeaderBytes);
  if (Impl->Sections == NULL) {
    return OUT_OF_RESOURCES;
  }

  Impl->IOCallback = BoraxStageObjectFile2;
  return BoraxStageObjectFileRead (
           Staged,
           Impl->SectionHeaders,
           Impl->Header.SectionHeaderOffset,
           SectionHeaderBytes
           );
}

#define DIVIDE_ROUND_UP(_a, _b)  (((_a) + (_b) - 1) / (_b))

STATIC EFI_STATUS
BoraxStageObjectFile2 (
  IN BORAX_STAGED_OBJECT_FILE  *Staged
  )
{
  EFI_STATUS                     Status;
  BORAX_STAGED_OBJECT_FILE_IMPL  *Impl = Staged->Impl;

  // Allocate the chunk data array
  {
    UINTN  SectionIndex;
    UINTN  ChunkCount;
    UINTN  ChunkDataSize;

    ChunkCount = 0;
    for (SectionIndex = 0;
         SectionIndex < Impl->Header.SectionHeaderCount;
         ++SectionIndex)
    {
      UINTN  PageCount = DIVIDE_ROUND_UP (
                           Impl->SectionHeaders[SectionIndex].Size,
                           BORAX_PAGE_SIZE
                           );

      switch (Impl->SectionHeaders[SectionIndex].Tag) {
        case BXO_SECTION_CONS:
          ChunkCount += PageCount;
          break;
        case BXO_SECTION_OBJECT:
          ChunkCount += DIVIDE_ROUND_UP (
                          PageCount,
                          Impl->SectionHeaders[SectionIndex].PagesPerChunk
                          );
          break;
        default:
          ChunkCount += 1;
          break;
      }
    }

    Status = SafeUintnMult (ChunkCount, sizeof (CHUNK_DATA), &ChunkDataSize);
    if (EFI_ERROR (Status)) {
      return Status;
    }

    Impl->ChunkData = AllocateZeroPool (ChunkDataSize);
    if (Impl->ChunkData == NULL) {
      return EFI_OUT_OF_RESOURCES;
    }

    Impl->ChunkCount = ChunkCount;
  }

  // Populate the chunk data array
  {
    UINTN  SectionIndex;
    UINTN  ChunkIndex    = 0;
    UINTN  NextPageIndex = 0;

    for (SectionIndex = 0;
         SectionIndex < Impl->Header.SectionHeaderCount;
         ++SectionIndex)
    {
      UINTN  PageCount = DIVIDE_ROUND_UP (
                           Impl->SectionHeaders[SectionIndex].Size,
                           BORAX_PAGE_SIZE
                           );
      UINTN  SectionChunkPages;
      UINTN  SectionChunkCount;
      UINTN  SectionChunkIndex;

      switch (Impl->SectionHeaders[SectionIndex].Tag) {
        case BXO_SECTION_CONS:
          SectionChunkPages = 1;
          SectionChunkCount = PageCount;
          break;
        case BXO_SECTION_OBJECT:
          SectionChunkPages = Impl->SectionHeaders[SectionIndex].PagesPerChunk;
          SectionChunkCount = DIVIDE_ROUND_UP (PageCount, SectionChunkPages);
          break;
        default:
          SectionChunkPages = PageCount;
          SectionChunkCount = 1;
          break;
      }

      for (SectionChunkIndex = 0;
           SectionChunkIndex < SectionChunkCount;
           ++SectionChunkIndex)
      {
        CHUNK_DATA  *ChunkData = &Impl->ChunkData[ChunkIndex];

        ChunkData->SectionIndex      = SectionIndex;
        ChunkData->SectionChunkIndex = SectionChunkIndex;
        ChunkData->FirstPageIndex    = NextPageIndex;
        ChunkData->PageCount         = SectionChunkPages;

        switch (Impl->SectionHeaders[SectionIndex].Tag) {
          case BXO_SECTION_CONS:
          case BXO_SECTION_OBJECT:
            ChunkData->Pages = Impl->Alloc->SysAlloc->AllocatePages (
                                                        Impl->Alloc->SysAlloc,
                                                        SectionChunkPages
                                                        );
            break;
          default:
            ChunkData->Pages = AllocatePages (
                                 Impl->Alloc->SysAlloc,
                                 SectionChunkPages
                                 );
            break;
        }

        if (ChunkData->Pages == NULL) {
          return EFI_OUT_OF_RESOURCES;
        }

        SetMem (ChunkData->Pages, BORAX_PAGE_SIZE * SectionChunkPages, -1);

        if (Impl->SectionHeaders[SectionIndex].Tag == BXO_SECTION_OBJECT) {
          UINTN  ObjectBitmapWords;

          Status = SafeUintnMult (
                     OBJECT_BITMAP_WORDS_PER_PAGE,
                     SectionChunkPages,
                     &ObjectBitmapWords
                     );
          if (EFI_ERROR (Status)) {
            return Status;
          }

          ChunkData->ObjectBitmap = AllocateZeroPool (ObjectBitmapWords);
          if (ChunkData->ObjectBitmap == NULL) {
            return EFI_OUT_OF_RESOURCES;
          }
        }

        NextPageIndex += SectionChunkPages;
        ++ChunkIndex;
      }
    }
  }

  // Chain to the section data I/O callback
  Impl->IOCallback = BoraxStageObjectFile3;
  Impl->IO.Status  = EFI_SUCCESS;
  gBS->SignalEvent (Impl->IO.Event);
  return EFI_SUCCESS;
}

STATIC EFI_STATUS
BoraxStageObjectFile3 (
  IN BORAX_STAGED_OBJECT_FILE  *Staged
  )
{
  EFI_STATUS                     Status;
  BORAX_STAGED_OBJECT_FILE_IMPL  *Impl = Staged->Impl;
  CHUNK_DATA                     *ChunkData;
  BXO_SECTION_HEADER             *SectionHeader;
  UINTN                          SectionChunkPages;
  UINTN                          SectionChunkBytes;
  UINTN                          ChunkOffset;
  UINTN                          ChunkEnd;
  UINTN                          ChunkSize;

  // Check for completion
  if (Impl->LoadChunkIndex >= Impl->ChunkCount) {
    BoraxStageObjectFileSuccess (Staged);
    return EFI_SUCCESS;
  }

  // Load next chunk
  ChunkData     = &Impl->ChunkData[Impl->LoadChunkIndex];
  SectionHeader = &Impl->SectionHeaders[ChunkData->SectionIndex];

  switch (Impl->SectionHeaders[SectionIndex].Tag) {
    case BXO_SECTION_CONS:
      SectionChunkPages = 1;
      break;
    case BXO_SECTION_OBJECT:
      SectionChunkPages = SectionHeader->PagesPerChunk;
      break;
    default:
      SectionChunkPages = PageCount;
      break;
  }

  Status = SafeUintnMult (
             BORAX_PAGE_SIZE,
             SectionChunkPages,
             &SectionChunkBytes
             );
  if (EFI_ERROR (Status)) {
    return Status;
  }

  Status = SafeUintnMult (
             SectionChunkBytes,
             ChunkData->SectionChunkIndex,
             &ChunkOffset
             );
  if (EFI_ERROR (Status)) {
    return Status;
  }

  Status = SafeUintnAdd (ChunkOffset, SectionChunkBytes, &ChunkEnd);
  if (EFI_ERROR (Status)) {
    return Status;
  }

  ChunkEnd = MIN (ChunkEnd, SectionHeader->Size);
  Status   = SafeUintnSub (ChunkEnd, ChunkOffset, &ChunkSize);
  if (EFI_ERROR (Status)) {
    return Status;
  }

  ++Impl->LoadChunkIndex;
  Impl->IOCallback = BoraxStageObjectFile3;
  return BoraxStageObjectFileRead (
           Staged,
           ChunkData->Pages,
           SectionHeader->Address + ChunkOffset,
           ChunkSize
           );
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
DecomposeAddress (
  IN BORAX_STAGED_OBJECT_FILE  *Staged,
  IN UINTN                     FileAddress,
  OUT UINTN                    *ChunkIndex,
  OUT UINTN                    *ChunkPageIndex,
  OUT UINTN                    *ByteIndex
  )
{
  BORAX_STAGED_OBJECT_FILE_IMPL  *Impl     = Staged->Impl;
  UINTN                          PageIndex = FileAddress >> 16;
  UINTN                          I;

  // Linear search for now
  for (I = 0; I < Impl->ChunkCount; ++I) {
    CHUNK_DATA  *ChunkData = &Impl->ChunkData[I];
    UINTN       Begin      = ChunkData->FirstPageIndex;
    UINTN       End        = Begin + ChunkData->PageCount;

    if ((Begin <= PageIndex) && (PageIndex < End)) {
      *ChunkIndex     = I;
      *ChunkPageIndex = PageIndex - Begin;
      *ByteIndex      = 0xfff & (FileAddress >> 4);
      return EFI_SUCCESS;
    }
  }

  // Generic overflow code
  return EFI_BUFFER_TOO_SMALL;
}
