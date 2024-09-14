#include <Library/BoraxBytecode.h>

#include <Library/BoraxPrimitive.h>

STATIC BORAX_OBJECT
EFIAPI
GetBytecodeFunction (
  IN BORAX_INTERPRETER         *Interp,
  IN BORAX_OBJECT              Object,
  OUT BORAX_BYTECODE_FUNCTION  **Function
  )
{
  EFI_STATUS               Status;
  BORAX_OBJECT             ClassBytecodeFunction = Interp->Globals[BORAX_GLOBAL_CLASS_BYTECODE_FUNCTION];
  BORAX_BYTECODE_FUNCTION  *F;

  Status = BORAX_GET_OBJECT_RECORD (Object, &F);
  if (EFI_ERROR (Status)) {
    goto type_error;
  }

  if (!BORAX_EQ (F->Record.Class, ClassBytecodeFunction)) {
    goto type_error;
  }

  *Function = F;
  return BORAX_NIL;

type_error:
  return BoraxPrimitiveTypeError (Interp, Object, ClassBytecodeFunction);
}

STATIC BORAX_OBJECT
EFIAPI
GetFixnum (
  IN BORAX_INTERPRETER  *Interp,
  IN BORAX_OBJECT       Object,
  OUT UINTN             *Value
  )
{
  BORAX_OBJECT  ClassFixnum = Interp->Globals[BORAX_GLOBAL_CLASS_FIXNUM];

  if (!BORAX_IS_FIXNUM (Object)) {
    return BoraxPrimitiveTypeError (Interp, Object, ClassFixnum);
  }

  *Value = BORAX_GET_FIXNUM (Object);
  return BORAX_NIL;
}

STATIC BORAX_OBJECT
EFIAPI
BytecodeFunctionRun (
  IN BORAX_TASK    *Task,
  IN BORAX_OBJECT  Function
  )
{
  BORAX_OBJECT             Condition;
  BORAX_BYTECODE_FUNCTION  *F;
  UINTN                    CodeLength;
  UINT8                    *CodeData;

  Condition = GetBytecodeFunction (Task->Interp, Function, &F);
  if (BORAX_BOOL (Condition)) {
    return Condition;
  }

  Condition = BoraxPrimitiveSimpleVectorU8Data (
                Task->Interp,
                F->Code,
                &CodeLength,
                &CodeData
                );
  if (BORAX_BOOL (Condition)) {
    return Condition;
  }

  return BoraxPrimitiveSimpleCondition (
           Task->Interp,
           Task->Interp->Globals[BORAX_GLOBAL_CLASS_SIMPLE_ERROR],
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
  BORAX_OBJECT             Condition;
  BORAX_BYTECODE_FUNCTION  *F;

  Condition = GetBytecodeFunction (Interp, Function, &F);
  if (BORAX_BOOL (Condition)) {
    return Condition;
  }

  Name->Tag    = BORAX_FUNCTION_NAME_OBJECT;
  Name->Object = F->Name;
  return BORAX_NIL;
}

STATIC BORAX_OBJECT
EFIAPI
BytecodeFunctionInfo (
  IN BORAX_INTERPRETER     *Interp,
  IN BORAX_OBJECT          Function,
  OUT BORAX_FUNCTION_INFO  *Info
  )
{
  BORAX_OBJECT             Condition;
  BORAX_BYTECODE_FUNCTION  *F;
  UINTN                    Entry, Locals, SharedLength;
  BORAX_OBJECT             *SharedData;

  Condition = GetBytecodeFunction (Interp, Function, &F);
  if (BORAX_BOOL (Condition)) {
    return Condition;
  }

  Condition = GetFixnum (Interp, F->Entry, &Entry);
  if (BORAX_BOOL (Condition)) {
    return Condition;
  }

  Condition = GetFixnum (Interp, F->Locals, &Locals);
  if (BORAX_BOOL (Condition)) {
    return Condition;
  }

  Condition = BoraxPrimitiveSimpleVectorData (
                Interp,
                F->Shared,
                &SharedLength,
                &SharedData
                );
  if (BORAX_BOOL (Condition)) {
    return Condition;
  }

  Info->Entry  = Entry;
  Info->Locals = Locals;
  Info->Shared = SharedLength;
  return BORAX_NIL;
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
           Interp->Globals[BORAX_GLOBAL_CLASS_SIMPLE_ERROR],
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
           Interp->Globals[BORAX_GLOBAL_CLASS_SIMPLE_ERROR],
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
