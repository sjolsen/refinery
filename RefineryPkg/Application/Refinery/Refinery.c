#include <Uefi.h>
#include <Library/UefiLib.h>

EFI_STATUS EFIAPI
RefineryMain (
  IN EFI_HANDLE        ImageHandle,
  IN EFI_SYSTEM_TABLE  *SystemTable
  )
{
  Print (L"RefineryMain\r\n");
  return EFI_SUCCESS;
}
