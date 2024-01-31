#ifndef REFINERY_BUNDLED_DRIVER_H
#define REFINERY_BUNDLED_DRIVER_H

#include <Uefi.h>

EFI_STATUS
EFIAPI
LoadBundledDriver (
  const CHAR16  *Basename
  );

EFI_STATUS
ConnectAllEfi (
  VOID
  );

#endif // REFINERY_BUNDLED_DRIVER_H
