#include <Library/BoraxObjectFile.h>

#include <Library/SafeIntLib.h>

typedef EFI_STATUS (*EFIAPI IO_CALLBACK)(IN BORAX_STAGED_OBJECT_FILE  *Staged);

struct _BORAX_STAGED_OBJECT_FILE_IMPL {
  BORAX_ALLOCATOR       *Alloc;
  EFI_FILE_PROTOCOL     *File;
  BOOLEAN               Cancelled;
  UINT64                FileSize;
  EFI_FILE_IO_TOKEN     IO;
  IO_CALLBACK           IOCallback;
  BXO_HEADER            Header;
  BXO_SECTION_HEADER    *SectionHeaders;
};

STATIC VOID
EFIAPI
BoraxStageObjectFileCleanup (
  IN BORAX_STAGED_OBJECT_FILE  *Staged
  )
{
  BORAX_STAGED_OBJECT_FILE_IMPL  *Impl = Staged->Impl;

  if (Impl == NULL) {
    return;
  }

  gBS->CloseEvent (Impl->IO.Event);
  FreePool (Impl->SectionHeaders);
  FreePool (Impl);
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

STATIC EFI_STATUS
EFIAPI
BoraxStageObjectFileIOStatus (
  IN BORAX_STAGED_OBJECT_FILE  *Staged
  )
{
  if (Staged->Impl->Cancelled) {
    return EFI_ABORTED;
  } else {
    return Staged->Impl->IO.Status;
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

  Status = Staged->Impl->IOCallback (Staged);
  if (EFI_ERROR (Status)) {
    BoraxStageObjectFileFailure (Staged, Status);
    return;
  }
}

STATIC IO_CALLBACK  BoraxStageObjectFile1;
STATIC IO_CALLBACK  BoraxStageObjectFile2;

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

  // Check for failure or cancellation
  Status = BoraxStageObjectFileIOStatus (Staged);
  if (EFI_ERROR (Status)) {
    return Status;
  }

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
