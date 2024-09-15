#include <Library/BoraxBytecode.h>

#include <Library/DebugLib.h>
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

typedef struct {
  BORAX_TASK    *Task;
  UINTN         CodeLength;
  UINT8         *CodeData;
  UINTN         Pos;
} PARSER_STATE;

STATIC BORAX_OBJECT
EFIAPI
ReadByte (
  IN PARSER_STATE  *State,
  OUT UINT8        *Byte
  )
{
  if (State->Pos >= State->CodeLength) {
    BORAX_OBJECT  Args[] = { BORAX_MAKE_FIXNUM (State->Pos) };
    // TODO: Dedicated error type
    return BoraxPrimitiveSimpleCondition (
             State->Task->Interp,
             State->Task->Interp->Globals[BORAX_GLOBAL_CLASS_SIMPLE_PROGRAM_ERROR],
             L"Code index out of bounds: ~S",
             ARRAY_SIZE (Args),
             Args
             );
  }

  *Byte = State->CodeData[State->Pos++];
  DEBUG ((DEBUG_ERROR, "Read byte %02x\n", *Byte));
  return BORAX_NIL;
}

STATIC BORAX_OBJECT
EFIAPI
DecodeField (
  IN PARSER_STATE  *State,
  IN OUT UINT8     *Field,
  IN UINTN         Bits
  )
{
  UINTN  Limit = (1 << Bits) - 1;

  if (*Field == Limit) {
    return ReadByte (State, Field);
  } else {
    return BORAX_NIL;
  }
}

STATIC BORAX_OBJECT
EFIAPI
ReadJumpTarget (
  IN PARSER_STATE  *State,
  OUT UINT16       *JumpTarget
  )
{
  BORAX_OBJECT  Condition;
  UINT8         Low, High;

  Condition = ReadByte (State, &Low);
  if (BORAX_BOOL (Condition)) {
    return Condition;
  }

  Condition = ReadByte (State, &High);
  if (BORAX_BOOL (Condition)) {
    return Condition;
  }

  *JumpTarget = Low | (High << 8);
  return BORAX_NIL;
}

typedef struct {
  UINT8    Mode;
  UINT8    Block;
  UINT8    Index;
} LOCATION;

STATIC BORAX_OBJECT
EFIAPI
ReadLocation (
  IN PARSER_STATE  *State,
  OUT LOCATION     *Location
  )
{
  BORAX_OBJECT  Condition;
  UINT8         Operand;
  UINT8         Mode, Block, Index;

  Condition = ReadByte (State, &Operand);
  if (BORAX_BOOL (Condition)) {
    return Condition;
  }

  Mode = Operand & 0xC0;

  switch (Mode) {
    case BORAX_MODE_CONSTANT:
    case BORAX_MODE_LOCAL:
      Block = 0;
      Index = Operand & 0x3F;

      Condition = DecodeField (State, &Index, 6);
      if (BORAX_BOOL (Condition)) {
        return Condition;
      }

      break;

    case BORAX_MODE_SHARED:
    case BORAX_MODE_CLOSURE:
      Block = (Operand >> 3) & 0x07;
      Index = (Operand >> 0) & 0x07;

      Condition = DecodeField (State, &Block, 3);
      if (BORAX_BOOL (Condition)) {
        return Condition;
      }

      Condition = DecodeField (State, &Index, 3);
      if (BORAX_BOOL (Condition)) {
        return Condition;
      }

      break;
  }

  Location->Mode  = Mode;
  Location->Block = Block;
  Location->Index = Index;
  return BORAX_NIL;
}

STATIC BORAX_OBJECT
EFIAPI
LoadLocation (
  IN PARSER_STATE    *State,
  IN CONST LOCATION  *Location,
  OUT BORAX_OBJECT   *Value
  )
{
  BORAX_OBJECT  Condition;

  switch (Location->Mode) {
    case BORAX_MODE_CONSTANT:
      return BoraxTaskReadConstant (State->Task, Location->Index, Value);

    case BORAX_MODE_LOCAL:
    {
      BORAX_OBJECT  *Local;

      Condition = BoraxTaskAccessLocal (State->Task, Location->Index, &Local);
      if (BORAX_BOOL (Condition)) {
        return Condition;
      }

      *Value = *Local;
      return BORAX_NIL;
    }

    case BORAX_MODE_SHARED:
    {
      BORAX_OBJECT  Args[] = {
        BORAX_MAKE_FIXNUM (Location->Block),
        BORAX_MAKE_FIXNUM (Location->Index),
      };

      return BoraxPrimitiveSimpleCondition (
               State->Task->Interp,
               State->Task->Interp->Globals[BORAX_GLOBAL_CLASS_SIMPLE_ERROR],
               L"Not implemented: access to location (SHARED ~S ~S)",
               ARRAY_SIZE (Args),
               Args
               );
    }

    case BORAX_MODE_CLOSURE:
    {
      BORAX_OBJECT  Args[] = {
        BORAX_MAKE_FIXNUM (Location->Block),
        BORAX_MAKE_FIXNUM (Location->Index),
      };

      return BoraxPrimitiveSimpleCondition (
               State->Task->Interp,
               State->Task->Interp->Globals[BORAX_GLOBAL_CLASS_SIMPLE_ERROR],
               L"Not implemented: access to location (CLOSURE ~S ~S)",
               ARRAY_SIZE (Args),
               Args
               );
    }

    default:
      UNREACHABLE ();
  }
}

STATIC BORAX_OBJECT
EFIAPI
StoreLocation (
  IN PARSER_STATE    *State,
  IN CONST LOCATION  *Location,
  IN BORAX_OBJECT    Value
  )
{
  BORAX_OBJECT  Condition;

  switch (Location->Mode) {
    case BORAX_MODE_CONSTANT:
    {
      BORAX_OBJECT  Args[] = { BORAX_MAKE_FIXNUM (Location->Index) };

      // TODO: Should this be a location-error?
      return BoraxPrimitiveSimpleCondition (
               State->Task->Interp,
               State->Task->Interp->Globals[BORAX_GLOBAL_CLASS_SIMPLE_PROGRAM_ERROR],
               L"Tried to BIND location (CONSTANT ~S)",
               ARRAY_SIZE (Args),
               Args
               );
    }

    case BORAX_MODE_LOCAL:
    {
      BORAX_OBJECT  *Local;

      Condition = BoraxTaskAccessLocal (State->Task, Location->Index, &Local);
      if (BORAX_BOOL (Condition)) {
        return Condition;
      }

      *Local = Value;
      return BORAX_NIL;
    }

    case BORAX_MODE_SHARED:
    {
      BORAX_OBJECT  Args[] = {
        BORAX_MAKE_FIXNUM (Location->Block),
        BORAX_MAKE_FIXNUM (Location->Index),
      };

      return BoraxPrimitiveSimpleCondition (
               State->Task->Interp,
               State->Task->Interp->Globals[BORAX_GLOBAL_CLASS_SIMPLE_ERROR],
               L"Not implemented: access to location (SHARED ~S ~S)",
               ARRAY_SIZE (Args),
               Args
               );
    }

    case BORAX_MODE_CLOSURE:
    {
      BORAX_OBJECT  Args[] = {
        BORAX_MAKE_FIXNUM (Location->Block),
        BORAX_MAKE_FIXNUM (Location->Index),
      };

      return BoraxPrimitiveSimpleCondition (
               State->Task->Interp,
               State->Task->Interp->Globals[BORAX_GLOBAL_CLASS_SIMPLE_ERROR],
               L"Not implemented: access to location (CLOSURE ~S ~S)",
               ARRAY_SIZE (Args),
               Args
               );
    }

    default:
      UNREACHABLE ();
  }
}

STATIC BORAX_OBJECT
EFIAPI
ProcessCondition (
  IN PARSER_STATE  *State,
  IN UINT8         CFlag,
  OUT BOOLEAN      *DoIt
  )
{
  BORAX_OBJECT  Condition;

  switch (CFlag) {
    case BORAX_CFLAG_UNCONDITIONAL:
      *DoIt = TRUE;
      return BORAX_NIL;

    case BORAX_CFLAG_BOOLEAN:
    {
      LOCATION      Location;
      BORAX_OBJECT  Value;

      Condition = ReadLocation (State, &Location);
      if (BORAX_BOOL (Condition)) {
        return Condition;
      }

      Condition = LoadLocation (State, &Location, &Value);
      if (BORAX_BOOL (Condition)) {
        return Condition;
      }

      *DoIt = BORAX_BOOL (Value);
      return BORAX_NIL;
    }

    case BORAX_CFLAG_NEGATED_BOOLEAN:
    {
      LOCATION      Location;
      BORAX_OBJECT  Value;

      Condition = ReadLocation (State, &Location);
      if (BORAX_BOOL (Condition)) {
        return Condition;
      }

      Condition = LoadLocation (State, &Location, &Value);
      if (BORAX_BOOL (Condition)) {
        return Condition;
      }

      *DoIt = !BORAX_BOOL (Value);
      return BORAX_NIL;
    }

    default:
    {
      BORAX_OBJECT  Args[] = { BORAX_MAKE_FIXNUM (CFlag) };
      return BoraxPrimitiveSimpleCondition (
               State->Task->Interp,
               State->Task->Interp->Globals[BORAX_GLOBAL_CLASS_SIMPLE_PROGRAM_ERROR],
               L"Illegal condition flag: ~S",
               ARRAY_SIZE (Args),
               Args
               );
    }
  }
}

STATIC BORAX_OBJECT
EFIAPI
BytecodeFunctionRun (
  IN BORAX_TASK    *Task,
  IN BORAX_OBJECT  Function
  )
{
  EFI_STATUS               Status;
  BORAX_OBJECT             Condition;
  BORAX_BYTECODE_FUNCTION  *F;
  PARSER_STATE             State;
  UINT8                    Opcode;
  BOOLEAN                  Fast, Tail;
  UINTN                    I;

  State.Task = Task;
  State.Pos  = Task->Registers.PC;

  Condition = GetBytecodeFunction (Task->Interp, Function, &F);
  if (BORAX_BOOL (Condition)) {
    return Condition;
  }

  Condition = BoraxPrimitiveSimpleVectorU8Data (
                Task->Interp,
                F->Code,
                &State.CodeLength,
                &State.CodeData
                );
  if (BORAX_BOOL (Condition)) {
    return Condition;
  }

  // Read opcode byte
  Condition = ReadByte (&State, &Opcode);
  if (BORAX_BOOL (Condition)) {
    return Condition;
  }

  // Handle fast/tail flags
  if ((Opcode & 0xC0) == BORAX_OPCODE_CALL) {
    Fast    = Opcode & BORAX_CALL_FLAG_FAST;
    Tail    = Opcode & BORAX_CALL_FLAG_TAIL;
    Opcode &= 0xCF;
  }

  // Dispatch on high nibble
  switch (Opcode & 0xF0) {
    case BORAX_OPCODE_JUMP:
    {
      UINT8    CFlag = Opcode & 0x0F;
      BOOLEAN  DoIt;
      UINT16   JumpTarget;

      Condition = ProcessCondition (&State, CFlag, &DoIt);
      if (BORAX_BOOL (Condition)) {
        return Condition;
      }

      Condition = ReadJumpTarget (&State, &JumpTarget);
      if (BORAX_BOOL (Condition)) {
        return Condition;
      }

      if (DoIt) {
        Task->Registers.PC = JumpTarget;
      } else {
        Task->Registers.PC = State.Pos;
      }

      return BORAX_NIL;
    }

    case BORAX_OPCODE_BIND:
    {
      UINTN  Length  = Task->Registers.VR->Length;
      UINT8  Opcount = Opcode & 0x0F;

      Condition = DecodeField (&State, &Opcount, 4);
      if (BORAX_BOOL (Condition)) {
        return Condition;
      }

      switch (Opcount) {
        case 0:
          // no-op
          break;

        case 1:
        {
          LOCATION  Location;

          Condition = ReadLocation (&State, &Location);
          if (BORAX_BOOL (Condition)) {
            return Condition;
          }

          // TODO: Either copy the VR or document the fact that binding clears
          // it. It's probably never useful to access the VR after binding it.
          Condition = StoreLocation (
                        &State,
                        &Location,
                        BORAX_MAKE_POINTER (Task->Registers.VR)
                        );
          if (BORAX_BOOL (Condition)) {
            return Condition;
          }

          // TODO: Maybe make BoraxMakeMultipleValues return a condition
          Status = BoraxMakeMultipleValues (
                     Task->Interp,
                     0,
                     &Task->Registers.VR
                     );
          if (EFI_ERROR (Status)) {
            return BoraxPrimitiveHeapExhausted (Task->Interp);
          }

          break;
        }

        default:
          Opcount -= 2;

          for (I = 0; I < Opcount; ++I) {
            BORAX_OBJECT  Value;
            LOCATION      Location;

            if (I < Length) {
              Value = Task->Registers.VR->Values[I];
            } else {
              Value = BORAX_NIL;
            }

            Condition = ReadLocation (&State, &Location);
            if (BORAX_BOOL (Condition)) {
              return Condition;
            }

            Condition = StoreLocation (&State, &Location, Value);
            if (BORAX_BOOL (Condition)) {
              return Condition;
            }
          }
      }

      Task->Registers.PC = State.Pos;
      return BORAX_NIL;
    }

    /* // TODO: Consider more carefully the consequence of encountering an error */
    /* // after invalidating the VR */
    /* // TODO: Also maybe make BoraxResizeMultipleValues return a condition */
    /* Status = BoraxResizeMultipleValues ( */
    /*   Task->Interp, */
    /*   &Task->Registers.VR, */
    /*   Opcount */
    /*   ); */
    /* if (EFI_ERROR (Status)) { */
    /*   return BoraxPrimitiveHeapExhausted (Task->Interp); */
    /* } */

    case BORAX_OPCODE_MOVE:
    {
      UINT8     CFlag = Opcode & 0x0F;
      BOOLEAN   DoIt;
      LOCATION  Dst, Src;

      Condition = ProcessCondition (&State, CFlag, &DoIt);
      if (BORAX_BOOL (Condition)) {
        return Condition;
      }

      Condition = ReadLocation (&State, &Dst);
      if (BORAX_BOOL (Condition)) {
        return Condition;
      }

      Condition = ReadLocation (&State, &Src);
      if (BORAX_BOOL (Condition)) {
        return Condition;
      }

      if (DoIt) {
        BORAX_OBJECT  Value;

        Condition = LoadLocation (&State, &Src, &Value);
        if (BORAX_BOOL (Condition)) {
          return Condition;
        }

        Condition = StoreLocation (&State, &Dst, Value);
        if (BORAX_BOOL (Condition)) {
          return Condition;
        }
      }

      Task->Registers.PC = State.Pos;
      return BORAX_NIL;
    }

    default:
    {
      BORAX_OBJECT  Args[] = { BORAX_MAKE_FIXNUM (Opcode) };
      (VOID)Fast;
      (VOID)Tail;
      return BoraxPrimitiveSimpleCondition (
               Task->Interp,
               Task->Interp->Globals[BORAX_GLOBAL_CLASS_SIMPLE_ERROR],
               L"Not implemented: instruction ~2,'0X",
               ARRAY_SIZE (Args),
               Args
               );
    }
  }
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
  BORAX_OBJECT             Condition;
  BORAX_BYTECODE_FUNCTION  *F;
  UINTN                    Length;
  BORAX_OBJECT             *Data;

  Condition = GetBytecodeFunction (Interp, Function, &F);
  if (BORAX_BOOL (Condition)) {
    return Condition;
  }

  Condition = BoraxPrimitiveSimpleVectorData (
                Interp,
                F->Constants,
                &Length,
                &Data
                );
  if (BORAX_BOOL (Condition)) {
    return Condition;
  }

  if (Index >= Length) {
    return BoraxPrimitiveConstantLocationError (Interp, Index);
  }

  *Constant = Data[Index];
  return BORAX_NIL;
}

CONST BORAX_FUNCTION_OPS  gBytecodeFunctionOps = {
  .Run      = &BytecodeFunctionRun,
  .Name     = &BytecodeFunctionName,
  .Info     = &BytecodeFunctionInfo,
  .Shared   = &BytecodeFunctionShared,
  .Constant = &BytecodeFunctionConstant,
};
