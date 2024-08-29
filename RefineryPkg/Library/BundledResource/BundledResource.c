#include <Library/BundledResource.h>

#include <Library/DevicePathLib.h>
#include <Library/MemoryAllocationLib.h>
#include <Library/UefiBootServicesTableLib.h>
#include <Protocol/LoadedImage.h>

STATIC EFI_STATUS
EFIAPI
FindDevicePathNode (
  IN EFI_DEVICE_PATH_PROTOCOL   *Path,
  IN UINT8                      Type,
  IN UINT8                      Subtype,
  OUT EFI_DEVICE_PATH_PROTOCOL  **Result
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

STATIC EFI_STATUS
EFIAPI
GetEfiVolume (
  OUT EFI_DEVICE_PATH_PROTOCOL  **Result
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

STATIC EFI_STATUS
EFIAPI
AppendFilePath (
  IN EFI_DEVICE_PATH_PROTOCOL   *Base,
  IN const CHAR16               *File,
  OUT EFI_DEVICE_PATH_PROTOCOL  **Result
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
BundledResourcePath (
  IN const CHAR16               *Path,
  OUT EFI_DEVICE_PATH_PROTOCOL  **Result
  )
{
  EFI_STATUS                Status;
  EFI_DEVICE_PATH_PROTOCOL  *Volume   = NULL;
  EFI_DEVICE_PATH_PROTOCOL  *Efi      = NULL;
  EFI_DEVICE_PATH_PROTOCOL  *Refinery = NULL;
  EFI_DEVICE_PATH_PROTOCOL  *File     = NULL;

  Status = GetEfiVolume (&Volume);
  if (EFI_ERROR (Status)) {
    goto cleanup;
  }

  Status = AppendFilePath (Volume, L"EFI", &Efi);
  if (EFI_ERROR (Status)) {
    goto cleanup;
  }

  Status = AppendFilePath (Efi, L"Refinery", &Refinery);
  if (EFI_ERROR (Status)) {
    goto cleanup;
  }

  Status = AppendFilePath (Refinery, Path, &File);
  if (EFI_ERROR (Status)) {
    goto cleanup;
  }

  *Result = File;
  File    = NULL;
  Status  = EFI_SUCCESS;

cleanup:
  FreePool (File);
  FreePool (Refinery);
  FreePool (Efi);
  FreePool (Volume);
  return Status;
}
