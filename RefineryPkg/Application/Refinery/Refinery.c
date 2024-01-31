#include <Library/UefiBootServicesTableLib.h>
#include <Library/UefiRuntimeServicesTableLib.h>

#include "Demo.h"
#include "Input.h"

DEMO         gDemo;
INPUT_STATE  gInput;

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

  Status = gBS->LocateProtocol (
                  &gEfiSimplePointerProtocolGuid,
                  NULL,
                  (VOID **)&Pointer
                  );
  if (EFI_ERROR (Status)) {
    return Status;
  }

  Status = DemoInit (&gDemo, gST->ConOut);
  if (EFI_ERROR (Status)) {
    return Status;
  }

  Status = InputInit (&gInput, &gDemo, gST->ConIn, Pointer);
  if (EFI_ERROR (Status)) {
    return Status;
  }

  while (gDemo.State == DEMO_STATE_RUNNING) {
    if (gDemo.Redraw) {
      Status = DemoRedraw (&gDemo);
      if (EFI_ERROR (Status)) {
        return Status;
      }
    }

    Status = InputDispatch (&gInput);
    if (EFI_ERROR (Status)) {
      return Status;
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

  return Status;
}
