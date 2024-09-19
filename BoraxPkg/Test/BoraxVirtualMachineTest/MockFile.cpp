#include "MockFile.hpp"

#include <cstring>
#include <sstream>

#include <errno.h>
#include <fcntl.h>
#include <unistd.h>

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

struct PosixFileError : std::exception {
  const char               *Operation;
  std::filesystem::path    Path;
  int                      Errno;
  std::string              WhatStr;

  PosixFileError (
                  const char             *Operation,
                  std::filesystem::path  Path,
                  int                    Errno
                  )
    : Operation (Operation),
    Path (std::move (Path)),
    Errno (Errno)
  {
    std::stringstream  ss;

    ss << Operation << "(" << this->Path << ") failed:  " << strerror (Errno);
    WhatStr = std::move (ss).str ();
  }

  const char *
  what (
    ) const noexcept override
  {
    return WhatStr.c_str ();
  }
};

PosixFile::PosixFile(
                     std::filesystem::path  Path
                     )
  : MockFile (EFI_FILE_PROTOCOL_REVISION), // no ex methods
  Path (std::move (Path))
{
  fd = open (this->Path.c_str (), O_RDONLY);
  if (fd < 0) {
    throw PosixFileError { "open", this->Path, errno };
  }
}

PosixFile::~PosixFile(
                      )
{
  close (fd);
}

EFI_STATUS
PosixFile::Read (
  IN OUT UINTN  *BufferSize,
  OUT VOID      *Buffer
  )
{
  ssize_t  rv = read (fd, Buffer, *BufferSize);

  if (rv < 0) {
    std::cerr << "read(" << Path << ", " << *BufferSize << ") failed: "
    << strerror (errno) << std::endl;
    return EFI_DEVICE_ERROR;
  }

  *BufferSize = static_cast<UINTN>(rv);
  return EFI_SUCCESS;
}

EFI_STATUS
PosixFile::GetPosition (
  OUT UINT64  *Position
  )
{
  off_t  rv = lseek (fd, 0, SEEK_CUR);

  if (rv < 0) {
    std::cerr << "lseek(" << Path << ", 0, SEEK_CUR) failed: "
    << strerror (errno) << std::endl;
    return EFI_DEVICE_ERROR;
  }

  *Position = static_cast<UINT64>(rv);
  return EFI_SUCCESS;
}

EFI_STATUS
PosixFile::SetPosition (
  IN UINT64  Position
  )
{
  if (Position == MAX_UINT64) {
    off_t  rv = lseek (fd, 0, SEEK_END);
    if (rv < 0) {
      std::cerr << "lseek(" << Path << ", 0, SEEK_END) failed: "
      << strerror (errno) << std::endl;
      return EFI_DEVICE_ERROR;
    }

    return EFI_SUCCESS;
  } else {
    off_t  rv = lseek (fd, Position, SEEK_SET);

    if (rv < 0) {
      std::cerr << "lseek(" << Path << ", " << Position
      << ", SEEK_SET) failed: "
      << strerror (errno) << std::endl;
      return EFI_DEVICE_ERROR;
    }

    return EFI_SUCCESS;
  }
}
