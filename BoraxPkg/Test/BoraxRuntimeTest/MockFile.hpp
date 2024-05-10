#ifndef BORAX_MOCK_FILE_H
#define BORAX_MOCK_FILE_H

#include <vector>

extern "C" {
  #include <Uefi.h>
  #include <Protocol/SimpleFileSystem.h>
}

#include "WrapFn.hpp"

class MockFile : public ProtocolClass<MockFile, EFI_FILE_PROTOCOL> {
public:
  MockFile(
           );

  virtual
  EFI_STATUS
  GetPosition (
    OUT UINT64  *Position
    )
  {
    return EFI_UNSUPPORTED;
  }

  virtual
  EFI_STATUS
  SetPosition (
    IN UINT64  Position
    )
  {
    return EFI_UNSUPPORTED;
  }
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
  GetPosition (
    OUT UINT64  *Position
    ) override;

  EFI_STATUS
  SetPosition (
    IN UINT64  Position
    ) override;
};

#endif // BORAX_MOCK_FILE_H
