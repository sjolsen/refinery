#include <Library/BundledDriver.h>
#include <Library/BundledResource.h>

#include <Library/DevicePathLib.h>
#include <Library/MemoryAllocationLib.h>
#include <Library/UefiBootServicesTableLib.h>
#include <Protocol/LoadedImage.h>

EFI_STATUS
EFIAPI
LoadBundledDriver (
  IN EFI_DEVICE_PATH_PROTOCOL  *DriverPath
  )
{
  EFI_STATUS                Status;
  EFI_DEVICE_PATH_PROTOCOL  *DriverPathRemainder;
  EFI_HANDLE                DriverHandle;

  // Check whether the driver is already running
  DriverPathRemainder = DriverPath;
  Status              = gBS->LocateDevicePath (
                               &gEfiLoadedImageProtocolGuid,
                               &DriverPathRemainder,
                               &DriverHandle
                               );
  if ((Status == EFI_SUCCESS) && IsDevicePathEndType (DriverPathRemainder)) {
    Status = EFI_SUCCESS;
    goto cleanup;
  }

  // Load and start the driver
  Status = gBS->LoadImage (
                  FALSE,
                  gImageHandle,
                  DriverPath,
                  NULL,
                  0,
                  &DriverHandle
                  );
  if (EFI_ERROR (Status)) {
    goto cleanup;
  }

  Status = gBS->StartImage (DriverHandle, NULL, NULL);

cleanup:
  return Status;
}

// Stolen from ShellPkg/.../Load.c
EFI_STATUS
ConnectAllEfi (
  VOID
  )
{
  EFI_STATUS  Status;
  UINTN       HandleCount;
  EFI_HANDLE  *HandleBuffer;
  UINTN       Index;

  Status = gBS->LocateHandleBuffer (
                  AllHandles,
                  NULL,
                  NULL,
                  &HandleCount,
                  &HandleBuffer
                  );
  if (EFI_ERROR (Status)) {
    return Status;
  }

  for (Index = 0; Index < HandleCount; Index++) {
    Status = gBS->ConnectController (HandleBuffer[Index], NULL, NULL, TRUE);
  }

  if (HandleBuffer != NULL) {
    FreePool (HandleBuffer);
  }

  return EFI_SUCCESS;
}
