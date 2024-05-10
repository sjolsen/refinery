#include "MockFile.hpp"

MockFile::MockFile(
                   )
{
  EFI_FILE_PROTOCOL  *Protocol = GetProtocol ();

  Protocol->Revision    = EFI_FILE_PROTOCOL_REVISION2;
  Protocol->Open        = Unsupported;
  Protocol->Close       = Unsupported;
  Protocol->Delete      = Unsupported;
  Protocol->Read        = Unsupported;
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

BufferFile::BufferFile(
                       std::vector<unsigned char>  Data
                       )
  : FileData (std::move (Data)),
  FilePosition (0)
{
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
