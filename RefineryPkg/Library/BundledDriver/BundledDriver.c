#include <Library/BundledDriver.h>

#include <Library/DevicePathLib.h>
#include <Library/MemoryAllocationLib.h>
#include <Library/UefiBootServicesTableLib.h>
#include <Protocol/LoadedImage.h>

EFI_STATUS
EFIAPI
FindDevicePathNode (
  EFI_DEVICE_PATH_PROTOCOL  *Path,
  UINT8                     Type,
  UINT8                     Subtype,
  EFI_DEVICE_PATH_PROTOCOL  **Result
  )
{
  EFI_DEVICE_PATH_PROTOCOL  *Cursor = Path;

  while (!IsDevicePathEndType (Cursor)) {
    if (  (DevicePathType (Cursor) == Type)
       && (DevicePathSubType (Cursor) == Subtype))
    {
      *Result = Cursor;
      return EFI_SUCCESS;
    }

    Cursor = NextDevicePathNode (Cursor);
  }

  return EFI_NOT_FOUND;
}

EFI_STATUS
EFIAPI
GetDriverVolume (
  EFI_DEVICE_PATH_PROTOCOL  **Result
  )
{
  EFI_STATUS                Status;
  EFI_DEVICE_PATH_PROTOCOL  *AppPath;
  EFI_DEVICE_PATH_PROTOCOL  *AppDrive = NULL;
  EFI_DEVICE_PATH_PROTOCOL  *AppDriveEnd;

  // Get the path of the running application image
  Status = gBS->HandleProtocol (
                  gImageHandle,
                  &gEfiLoadedImageDevicePathProtocolGuid,
                  (VOID **)&AppPath
                  );
  if (EFI_ERROR (Status)) {
    goto cleanup;
  }

  // Truncate the path of the image down to the drive prefix
  AppDrive = DuplicateDevicePath (AppPath);
  if (EFI_ERROR (Status)) {
    goto cleanup;
  }

  Status = FindDevicePathNode (
             AppDrive,
             MEDIA_DEVICE_PATH,
             MEDIA_FILEPATH_DP,
             &AppDriveEnd
             );
  if (EFI_ERROR (Status)) {
    goto cleanup;
  }

  SetDevicePathEndNode (AppDriveEnd);

  // The caller owns the new device path
  *Result  = AppDrive;
  AppDrive = NULL;
  Status   = EFI_SUCCESS;

cleanup:
  FreePool (AppDrive);
  return Status;
}

EFI_STATUS
EFIAPI
AppendFilePath (
  EFI_DEVICE_PATH_PROTOCOL  *Base,
  const CHAR16              *File,
  EFI_DEVICE_PATH_PROTOCOL  **Result
  )
{
  EFI_STATUS                Status;
  EFI_DEVICE_PATH_PROTOCOL  *FilePath = NULL;
  EFI_DEVICE_PATH_PROTOCOL  *BaseFile = NULL;

  FilePath = FileDevicePath (NULL, File);
  if (FilePath == NULL) {
    Status = EFI_OUT_OF_RESOURCES;
    goto cleanup;
  }

  BaseFile = AppendDevicePath (Base, FilePath);
  if (BaseFile == NULL) {
    Status = EFI_OUT_OF_RESOURCES;
    goto cleanup;
  }

  *Result  = BaseFile;
  BaseFile = NULL;
  Status   = EFI_SUCCESS;

cleanup:
  FreePool (BaseFile);
  FreePool (FilePath);
  return Status;
}

EFI_STATUS
EFIAPI
GetDriverPath (
  const CHAR16              *Basename,
  EFI_DEVICE_PATH_PROTOCOL  **Result
  )
{
  EFI_STATUS                Status;
  EFI_DEVICE_PATH_PROTOCOL  *Volume = NULL;
  EFI_DEVICE_PATH_PROTOCOL  *VD     = NULL;
  EFI_DEVICE_PATH_PROTOCOL  *VDF    = NULL;

  Status = GetDriverVolume (&Volume);
  if (EFI_ERROR (Status)) {
    goto cleanup;
  }

  Status = AppendFilePath (Volume, L"EFI\\Drivers", &VD);
  if (EFI_ERROR (Status)) {
    goto cleanup;
  }

  Status = AppendFilePath (VD, Basename, &VDF);
  if (EFI_ERROR (Status)) {
    goto cleanup;
  }

  *Result = VDF;
  VDF     = NULL;
  Status  = EFI_SUCCESS;

cleanup:
  FreePool (VDF);
  FreePool (VD);
  FreePool (Volume);
  return Status;
}

EFI_STATUS
EFIAPI
LoadBundledDriver (
  const CHAR16  *Basename
  )
{
  EFI_STATUS                Status;
  EFI_DEVICE_PATH_PROTOCOL  *AppPath;
  EFI_DEVICE_PATH_PROTOCOL  *DriverPath = NULL;
  EFI_DEVICE_PATH_PROTOCOL  *DriverPathRemainder;
  EFI_HANDLE                DriverHandle;

  // Search the volume containing the running image
  Status = gBS->HandleProtocol (
                  gImageHandle,
                  &gEfiLoadedImageDevicePathProtocolGuid,
                  (VOID **)&AppPath
                  );
  if (EFI_ERROR (Status)) {
    goto cleanup;
  }

  Status = GetDriverPath (Basename, &DriverPath);
  if (EFI_ERROR (Status)) {
    goto cleanup;
  }

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
  FreePool (DriverPath);
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
