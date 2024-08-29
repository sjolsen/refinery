#ifndef REFINERY_BUNDLED_RESOURCE_H
#define REFINERY_BUNDLED_RESOURCE_H

#include <Uefi.h>

EFI_STATUS
EFIAPI
BundledResourcePath (
  IN const CHAR16               *Path,
  OUT EFI_DEVICE_PATH_PROTOCOL  **Result
  );

#endif // REFINERY_BUNDLED_RESOURCE_H
