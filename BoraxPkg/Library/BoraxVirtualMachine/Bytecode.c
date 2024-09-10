#include <Library/BoraxBytecode.h>

STATIC EFI_STATUS
EFIAPI
BytecodeFunctionRun (
  IN BORAX_TASK  *Task,
  IN VOID        *Function
  )
{
  return EFI_UNSUPPORTED;
}

STATIC EFI_STATUS
EFIAPI
BytecodeFunctionName (
  IN VOID                  *Function,
  OUT BORAX_FUNCTION_NAME  *Name
  )
{
  return EFI_UNSUPPORTED;
}

STATIC EFI_STATUS
EFIAPI
BytecodeFunctionInfo (
  IN VOID                  *Function,
  OUT BORAX_FUNCTION_INFO  *Info
  )
{
  return EFI_UNSUPPORTED;
}

STATIC EFI_STATUS
EFIAPI
BytecodeFunctionShared (
  IN VOID    *Function,
  IN UINTN   Block,
  OUT UINTN  *Count
  )
{
  return EFI_UNSUPPORTED;
}

STATIC EFI_STATUS
EFIAPI
BytecodeFunctionConstant (
  IN VOID           *Function,
  IN UINTN          Index,
  OUT BORAX_OBJECT  *Constant
  )
{
  return EFI_UNSUPPORTED;
}

CONST BORAX_FUNCTION_OPS  gBytecodeFunctionOps = {
  .Run      = &BytecodeFunctionRun,
  .Name     = &BytecodeFunctionName,
  .Info     = &BytecodeFunctionInfo,
  .Shared   = &BytecodeFunctionShared,
  .Constant = &BytecodeFunctionConstant,
};
