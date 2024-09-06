#include <Library/BundledDriver.h>
#include <Library/BundledResource.h>
#include <Library/MemoryAllocationLib.h>
#include <Library/UefiBootServicesTableLib.h>
#include <Library/UefiRuntimeServicesTableLib.h>

#include "Demo.h"
#include "Input.h"

DEMO         gDemo;
INPUT_STATE  gInput;

STATIC EFI_STATUS
EFIAPI
LoadDrivers (
  VOID
  )
{
  EFI_STATUS                Status;
  EFI_DEVICE_PATH_PROTOCOL  *DriverPath = NULL;

  Status = BundledResourcePath (L"Drivers\\UsbMouseDxe.efi", &DriverPath);
  if (EFI_ERROR (Status)) {
    goto cleanup;
  }

  Status = LoadBundledDriver (DriverPath);
  if (EFI_ERROR (Status)) {
    goto cleanup;
  }

  Status = ConnectAllEfi ();
  if (EFI_ERROR (Status)) {
    goto cleanup;
  }

cleanup:
  if (DriverPath != NULL) {
    FreePool (DriverPath);
  }

  return Status;
}

EFI_STATUS
EFIAPI
RefineryMain (
  IN EFI_HANDLE        ImageHandle,
  IN EFI_SYSTEM_TABLE  *SystemTable
  )
{
  EFI_STATUS                   Status;
  EFI_SIMPLE_POINTER_PROTOCOL  *Pointer;

  gBS->SetWatchdogTimer (0, 0, 0, NULL);

  Status = LoadDrivers ();
  if (EFI_ERROR (Status)) {
    goto exit;
  }

  Status = gBS->LocateProtocol (
                  &gEfiSimplePointerProtocolGuid,
                  NULL,
                  (VOID **)&Pointer
                  );
  if (EFI_ERROR (Status)) {
    goto exit;
  }

  Status = DemoInit (&gDemo, gST->ConOut);
  if (EFI_ERROR (Status)) {
    goto exit;
  }

  Status = InputInit (&gInput, &gDemo, gST->ConIn, Pointer);
  if (EFI_ERROR (Status)) {
    goto cleanup;
  }

  while (gDemo.State == DEMO_STATE_RUNNING) {
    if (gDemo.Redraw) {
      Status = DemoRedraw (&gDemo);
      if (EFI_ERROR (Status)) {
        goto cleanup;
      }
    }

    Status = InputDispatch (&gInput);
    if (EFI_ERROR (Status)) {
      goto cleanup;
    }
  }

  switch (gDemo.State) {
    case DEMO_STATE_EXIT:
      Status = EFI_SUCCESS;
      break;

    case DEMO_STATE_SHUTDOWN:
      // Should not return
      gRT->ResetSystem (EfiResetShutdown, EFI_SUCCESS, 0, NULL);
      Status = EFI_DEVICE_ERROR;
      break;

    default:
      Status = EFI_INVALID_PARAMETER;
      break;
  }

cleanup:
  DemoCleanup (&gDemo);
exit:
  return Status;
}
