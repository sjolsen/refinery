#ifndef BORAX_MOCK_FILE_H
#define BORAX_MOCK_FILE_H

extern "C" {
  #include <Uefi.h>
  #include <Protocol/SimpleFileSystem.h>
}

#include "WrapFn.hpp"

class MockFile : public ProtocolClass<MockFile, EFI_FILE_PROTOCOL> {
public:
  MockFile(
           )
  {
    EFI_FILE_PROTOCOL  *Protocol = GetProtocol ();

    Protocol->Revision    = EFI_FILE_PROTOCOL_REVISION2;
    Protocol->Open        = Unsupported;
    Protocol->Close       = Unsupported;
    Protocol->Delete      = Unsupported;
    Protocol->Read        = Unsupported;
    Protocol->Write       = Unsupported;
    Protocol->GetPosition = Unsupported;
    Protocol->SetPosition = Unsupported;
    Protocol->GetInfo     = Unsupported;
    Protocol->SetInfo     = Unsupported;
    Protocol->Flush       = Unsupported;
    Protocol->OpenEx      = Unsupported;
    Protocol->ReadEx      = Unsupported;
    Protocol->WriteEx     = Unsupported;
    Protocol->FlushEx     = Unsupported;
  }
};

#endif // BORAX_MOCK_FILE_H
