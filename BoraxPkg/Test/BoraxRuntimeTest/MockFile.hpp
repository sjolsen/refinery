#ifndef BORAX_MOCK_FILE_HPP
#define BORAX_MOCK_FILE_HPP

#include <vector>

extern "C" {
  #include <Uefi.h>
  #include <Protocol/SimpleFileSystem.h>
}

#include "ProtocolClass.hpp"

class MockFile : public ProtocolClass<MockFile, EFI_FILE_PROTOCOL> {
public:
  MockFile(
           UINT64  Revision = EFI_FILE_PROTOCOL_REVISION2
           );

  virtual
  EFI_STATUS
  Read (
    IN OUT UINTN  *BufferSize,
    OUT VOID      *Buffer
    );

  virtual
  EFI_STATUS
  GetPosition (
    OUT UINT64  *Position
    );

  virtual
  EFI_STATUS
  SetPosition (
    IN UINT64  Position
    );
};

class BufferFile : public MockFile {
private:
  std::vector<unsigned char> FileData;
  std::size_t FilePosition;

public:
  BufferFile(
             std::vector<unsigned char>  Data
             );

  EFI_STATUS
  Read (
    IN OUT UINTN  *BufferSize,
    OUT VOID      *Buffer
    ) override;

  EFI_STATUS
  GetPosition (
    OUT UINT64  *Position
    ) override;

  EFI_STATUS
  SetPosition (
    IN UINT64  Position
    ) override;
};

#endif // BORAX_MOCK_FILE_HPP
