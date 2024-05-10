#include "MockFile.hpp"

#include <cstring>

MockFile::MockFile(
                   UINT64  Revision
                   )
{
  EFI_FILE_PROTOCOL  *Protocol = GetProtocol ();

  Protocol->Revision    = Revision;
  Protocol->Open        = Unsupported;
  Protocol->Close       = Unsupported;
  Protocol->Delete      = Unsupported;
  Protocol->Read        = WRAP_FN (Read);
  Protocol->Write       = Unsupported;
  Protocol->GetPosition = WRAP_FN (GetPosition);
  Protocol->SetPosition = WRAP_FN (SetPosition);
  Protocol->GetInfo     = Unsupported;
  Protocol->SetInfo     = Unsupported;
  Protocol->Flush       = Unsupported;
  Protocol->OpenEx      = Unsupported;
  Protocol->ReadEx      = Unsupported;
  Protocol->WriteEx     = Unsupported;
  Protocol->FlushEx     = Unsupported;
}

EFI_STATUS
MockFile::Read (
  IN OUT UINTN  *BufferSize,
  OUT VOID      *Buffer
  )
{
  MOCK_ERROR ("unsupported");
  return EFI_UNSUPPORTED;
}

EFI_STATUS
MockFile::GetPosition (
  OUT UINT64  *Position
  )
{
  MOCK_ERROR ("unsupported");
  return EFI_UNSUPPORTED;
}

EFI_STATUS
MockFile::SetPosition (
  IN UINT64  Position
  )
{
  MOCK_ERROR ("unsupported");
  return EFI_UNSUPPORTED;
}

BufferFile::BufferFile(
                       std::vector<unsigned char>  Data
                       )
  : MockFile (EFI_FILE_PROTOCOL_REVISION), // no ex methods
  FileData (std::move (Data)),
  FilePosition (0)
{
}

EFI_STATUS
BufferFile::Read (
  IN OUT UINTN  *BufferSize,
  OUT VOID      *Buffer
  )
{
  if (FilePosition >= FileData.size ()) {
    MOCK_ERROR ("file position beyond end of file");
    return EFI_DEVICE_ERROR;
  }

  UINTN  Remaining = FileData.size () - FilePosition;
  UINTN  ReadSize  = std::min (*BufferSize, Remaining);

  std::memcpy (Buffer, FileData.data () + FilePosition, ReadSize);
  *BufferSize   = ReadSize;
  FilePosition += ReadSize;
  return EFI_SUCCESS;
}

EFI_STATUS
BufferFile::GetPosition (
  OUT UINT64  *Position
  )
{
  *Position = FilePosition;
  return EFI_SUCCESS;
}

EFI_STATUS
BufferFile::SetPosition (
  IN UINT64  Position
  )
{
  if (Position == MAX_UINT64) {
    FilePosition = FileData.size ();
    return EFI_SUCCESS;
  } else {
    FilePosition = Position;
    return EFI_SUCCESS;
  }
}
