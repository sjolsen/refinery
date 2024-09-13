#include <Library/BoraxBytecode.h>

#include <Library/BoraxPrimitive.h>

STATIC BORAX_OBJECT
EFIAPI
BytecodeFunctionRun (
  IN BORAX_TASK    *Task,
  IN BORAX_OBJECT  Function
  )
{
  return BoraxPrimitiveSimpleCondition (
           Task->Interp,
           BORAX_GLOBAL_CLASS_SIMPLE_ERROR,
           L"Not implemented: BytecodeFunctionRun",
           0,
           NULL
           );
}

STATIC BORAX_OBJECT
EFIAPI
BytecodeFunctionName (
  IN BORAX_INTERPRETER     *Interp,
  IN BORAX_OBJECT          Function,
  OUT BORAX_FUNCTION_NAME  *Name
  )
{
  return BoraxPrimitiveSimpleCondition (
           Interp,
           BORAX_GLOBAL_CLASS_SIMPLE_ERROR,
           L"Not implemented: BytecodeFunctionName",
           0,
           NULL
           );
}

STATIC BORAX_OBJECT
EFIAPI
BytecodeFunctionInfo (
  IN BORAX_INTERPRETER     *Interp,
  IN BORAX_OBJECT          Function,
  OUT BORAX_FUNCTION_INFO  *Info
  )
{
  return BoraxPrimitiveSimpleCondition (
           Interp,
           BORAX_GLOBAL_CLASS_SIMPLE_ERROR,
           L"Not implemented: BytecodeFunctionInfo",
           0,
           NULL
           );
}

STATIC BORAX_OBJECT
EFIAPI
BytecodeFunctionShared (
  IN BORAX_INTERPRETER  *Interp,
  IN BORAX_OBJECT       Function,
  IN UINTN              Block,
  OUT UINTN             *Count
  )
{
  return BoraxPrimitiveSimpleCondition (
           Interp,
           BORAX_GLOBAL_CLASS_SIMPLE_ERROR,
           L"Not implemented: BytecodeFunctionShared",
           0,
           NULL
           );
}

STATIC BORAX_OBJECT
EFIAPI
BytecodeFunctionConstant (
  IN BORAX_INTERPRETER  *Interp,
  IN BORAX_OBJECT       Function,
  IN UINTN              Index,
  OUT BORAX_OBJECT      *Constant
  )
{
  return BoraxPrimitiveSimpleCondition (
           Interp,
           BORAX_GLOBAL_CLASS_SIMPLE_ERROR,
           L"Not implemented: BytecodeFunctionConstant",
           0,
           NULL
           );
}

CONST BORAX_FUNCTION_OPS  gBytecodeFunctionOps = {
  .Run      = &BytecodeFunctionRun,
  .Name     = &BytecodeFunctionName,
  .Info     = &BytecodeFunctionInfo,
  .Shared   = &BytecodeFunctionShared,
  .Constant = &BytecodeFunctionConstant,
};
