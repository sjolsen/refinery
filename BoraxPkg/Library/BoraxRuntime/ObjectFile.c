#include <Library/BoraxObjectFile.h>

#include <Library/SafeIntLib.h>

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

typedef EFI_STATUS (*EFIAPI IO_CALLBACK)(IN BORAX_STAGED_OBJECT_FILE  *Staged);

#define OBJECTS_PER_PAGE              (BORAX_PAGE_SIZE / 8)
#define OBJECT_BITMAP_WORDS_PER_PAGE  (OBJECTS_PER_PAGE / BORAX_WORD_BITS)

struct _BORAX_STAGED_OBJECT_FILE_IMPL {
  BORAX_ALLOCATOR       *Alloc;
  EFI_FILE_PROTOCOL     *File;
  BOOLEAN               Cancelled;
  UINT64                FileSize;
  EFI_FILE_IO_TOKEN     IO;
  IO_CALLBACK           IOCallback;
  BXO_HEADER            Header;
  BORAX_CONS_CHUNK      *Cons;
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
      Impl->Alloc->SysAlloc->FreePages (
                               Impl->Alloc->SysAlloc,
                               Impl->Object,
                               UNSAFE_PAGE_COUNT_ROUNDING_UP (
                                 Impl->Header.Object.Size
                                 )
                               );
    }

    if (Impl->Cons != NULL) {
      // The page-count calculation must not have overflowed for us to make it
      // here
      Impl->Alloc->SysAlloc->FreePages (
                               Impl->Alloc->SysAlloc,
                               Impl->Cons,
                               UNSAFE_PAGE_COUNT_ROUNDING_UP (
                                 Impl->Header.Cons.Size
                                 )
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

  *Chunk = Staged->Impl->Alloc->SysAlloc->AllocatePages (
                                            Staged->Impl->Alloc->SysAlloc,
                                            PageCount
                                            );
  if (*Chunk == NULL) {
    return EFI_OUT_OF_RESOURCES;
  }

  SetMem (*Chunk, BORAX_PAGE_SIZE * PageCount, -1);
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
STATIC IO_CALLBACK  BoraxStageObjectFile4;

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
  Impl->IOCallback = BoraxStageObjectFile3;
  return BoraxStageObjectFileRead (
           Staged,
           Impl->Cons,
           Impl->Cons.Offset,
           Impl->Cons.Size
           );
}

STATIC EFI_STATUS
BoraxStageObjectFile3 (
  IN BORAX_STAGED_OBJECT_FILE  *Staged
  )
{
  BORAX_STAGED_OBJECT_FILE_IMPL  *Impl = Staged->Impl;

  // Read the object chunk
  Impl->IOCallback = BoraxStageObjectFile4;
  return BoraxStageObjectFileRead (
           Staged,
           Impl->Object,
           Impl->Object.Offset,
           Impl->Object.Size
           );
}

STATIC EFI_STATUS
BoraxStageObjectFile4 (
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

EFI_STATUS
EFIAPI
BoraxInjectObjectFile (
  IN BORAX_STAGED_OBJECT_FILE  *Staged,
  OUT BORAX_PIN                **Pin
  );
