#ifndef REFINERY_BUNDLED_DRIVER_H
#define REFINERY_BUNDLED_DRIVER_H

#include <Uefi.h>

EFI_STATUS
EFIAPI
LoadBundledDriver (
  IN EFI_DEVICE_PATH_PROTOCOL  *DriverPath
  );

EFI_STATUS
ConnectAllEfi (
  VOID
  );

#endif // REFINERY_BUNDLED_DRIVER_H
