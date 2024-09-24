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
             BoraxCString (L"Code index out of bounds: ~S"),
             ARRAY_SIZE (Args),
             Args
             );
  }

  *Byte = State->CodeData[State->Pos++];
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
               BoraxCString (L"Not implemented: access to location (SHARED ~S ~S)"),
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
               BoraxCString (L"Not implemented: access to location (CLOSURE ~S ~S)"),
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
               BoraxCString (L"Tried to BIND location (CONSTANT ~S)"),
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
               BoraxCString (L"Not implemented: access to location (SHARED ~S ~S)"),
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
               BoraxCString (L"Not implemented: access to location (CLOSURE ~S ~S)"),
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
  BOOLEAN       Negated = !(CFlag & 1);

  switch (CFlag) {
    case BORAX_CFLAG_UNCONDITIONAL:
      *DoIt = TRUE;
      return BORAX_NIL;

    case BORAX_CFLAG_BOOLEAN:
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

      *DoIt = Negated ^ BORAX_BOOL (Value);
      return BORAX_NIL;
    }

    case BORAX_CFLAG_EQ:
    case BORAX_CFLAG_NEGATED_EQ:
    {
      LOCATION      LocationA, LocationB;
      BORAX_OBJECT  A, B;

      Condition = ReadLocation (State, &LocationA);
      if (BORAX_BOOL (Condition)) {
        return Condition;
      }

      Condition = ReadLocation (State, &LocationB);
      if (BORAX_BOOL (Condition)) {
        return Condition;
      }

      Condition = LoadLocation (State, &LocationA, &A);
      if (BORAX_BOOL (Condition)) {
        return Condition;
      }

      Condition = LoadLocation (State, &LocationB, &B);
      if (BORAX_BOOL (Condition)) {
        return Condition;
      }

      *DoIt = Negated ^ BORAX_EQ (A, B);
      return BORAX_NIL;
    }

    case BORAX_CFLAG_TYPEP:
    case BORAX_CFLAG_NEGATED_TYPEP:
    {
      LOCATION      LocationValue, LocationType;
      BORAX_OBJECT  Value, Type;
      BOOLEAN       Match;

      Condition = ReadLocation (State, &LocationValue);
      if (BORAX_BOOL (Condition)) {
        return Condition;
      }

      Condition = ReadLocation (State, &LocationType);
      if (BORAX_BOOL (Condition)) {
        return Condition;
      }

      Condition = LoadLocation (State, &LocationValue, &Value);
      if (BORAX_BOOL (Condition)) {
        return Condition;
      }

      Condition = LoadLocation (State, &LocationType, &Type);
      if (BORAX_BOOL (Condition)) {
        return Condition;
      }

      Condition = BoraxPrimitiveClassTypep (
                    State->Task->Interp,
                    Value,
                    Type,
                    &Match
                    );
      if (BORAX_BOOL (Condition)) {
        return Condition;
      }

      *DoIt = Negated ^ Match;
      return BORAX_NIL;
    }

    default:
    {
      BORAX_OBJECT  Args[] = { BORAX_MAKE_FIXNUM (CFlag) };
      return BoraxPrimitiveSimpleCondition (
               State->Task->Interp,
               State->Task->Interp->Globals[BORAX_GLOBAL_CLASS_SIMPLE_PROGRAM_ERROR],
               BoraxCString (L"Illegal condition flag: ~S"),
               ARRAY_SIZE (Args),
               Args
               );
    }
  }
}

STATIC BORAX_OBJECT
EFIAPI
ConditionalCoBind (
  IN PARSER_STATE  *State,
  IN BOOLEAN       DoIt,
  IN UINT8         Opcount
  )
{
  EFI_STATUS    Status;
  BORAX_OBJECT  Condition;
  BORAX_TASK    *Task = State->Task;
  UINTN         I;

  if (DoIt) {
    // Populate the VR
    switch (Opcount) {
      case 0:
        // no-op
        break;

      case 1:
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

        if (BORAX_DISCRIMINATE (Value) != BORAX_DISCRIM_MULTIPLE_VALUES) {
          return BoraxPrimitiveTypeError (
                   Task->Interp,
                   Value,
                   Task->Interp->Globals[BORAX_GLOBAL_CLASS_MULTIPLE_VALUES]
                   );
        }

        // Prevent Lisp code from mucking with the VR through a shared
        // reference
        Status = BoraxCopyMultipleValues (
                   Task->Interp,
                   (BORAX_MULTIPLE_VALUES *)BORAX_GET_POINTER (Value),
                   &Task->Registers.VR
                   );
        if (EFI_ERROR (Status)) {
          return BoraxPrimitiveHeapExhausted (Task->Interp);
        }

        break;
      }

      default:
        Opcount -= 2;

        Status = BoraxResizeMultipleValues (
                   Task->Interp,
                   Opcount,
                   &Task->Registers.VR
                   );

        for (I = 0; I < Opcount; ++I) {
          LOCATION      Location;
          BORAX_OBJECT  *Value = &Task->Registers.VR->Values[I];

          Condition = ReadLocation (State, &Location);
          if (BORAX_BOOL (Condition)) {
            return Condition;
          }

          Condition = LoadLocation (State, &Location, Value);
          if (BORAX_BOOL (Condition)) {
            return Condition;
          }
        }

        break;
    }
  } else {
    switch (Opcount) {
      case 0:
      case 1:
        // no-op
        break;

      default:
        Opcount -= 2;
        break;
    }

    // Consume the rest of the instruction bytes but don't do anything with
    // them
    for (I = 0; I < Opcount; ++I) {
      LOCATION  Location;

      Condition = ReadLocation (State, &Location);
      if (BORAX_BOOL (Condition)) {
        return Condition;
      }
    }
  }

  return BORAX_NIL;
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
  UINT8                    Opcode, Discrim;
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
  Discrim = Opcode;
  if ((Discrim & 0xC0) == BORAX_OPCODE_CALL) {
    Fast     = Opcode & BORAX_CALL_FLAG_FAST;
    Tail     = Opcode & BORAX_CALL_FLAG_TAIL;
    Discrim &= 0xCF;
  }

  // Handle push instructions
  if ((Discrim & 0xF0) != 0x80) {
    Discrim &= 0xF0;
  }

  // Dispatch on a variable-length subset of opcode bits
  switch (Discrim) {
    case BORAX_OPCODE_CALL:
    {
      UINT8         CFlag   = (Opcode >> 3) & 0x01;
      UINT8         Opcount = (Opcode >> 0) & 0x07;
      BOOLEAN       DoIt;
      LOCATION      Location;
      BORAX_OBJECT  Function;

      Condition = DecodeField (&State, &CFlag, 1);
      if (BORAX_BOOL (Condition)) {
        return Condition;
      }

      Condition = ProcessCondition (&State, CFlag, &DoIt);
      if (BORAX_BOOL (Condition)) {
        return Condition;
      }

      Condition = DecodeField (&State, &Opcount, 3);
      if (BORAX_BOOL (Condition)) {
        return Condition;
      }

      Condition = ReadLocation (&State, &Location);
      if (BORAX_BOOL (Condition)) {
        return Condition;
      }

      Condition = LoadLocation (&State, &Location, &Function);
      if (BORAX_BOOL (Condition)) {
        return Condition;
      }

      Condition = ConditionalCoBind (&State, DoIt, Opcount);
      if (BORAX_BOOL (Condition)) {
        return Condition;
      }

      if (DoIt) {
        // TODO: Handle the fast flag
        if (Tail) {
          return BoraxTaskEnterFunctionTail (Task, Function);
        } else {
          return BoraxTaskEnterFunction (Task, Function, State.Pos);
        }
      } else {
        Task->Registers.PC = State.Pos;
        return BORAX_NIL;
      }
    }

    case BORAX_OPCODE_JUMP:
    {
      UINT8    CFlag = Opcode & 0x0F;
      BOOLEAN  DoIt;
      UINT16   JumpTarget;

      Condition = DecodeField (&State, &CFlag, 4);
      if (BORAX_BOOL (Condition)) {
        return Condition;
      }

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

    case BORAX_OPCODE_RETURN:
    {
      UINT8    CFlag   = (Opcode >> 3) & 0x01;
      UINT8    Opcount = (Opcode >> 0) & 0x07;
      BOOLEAN  DoIt;

      Condition = DecodeField (&State, &CFlag, 1);
      if (BORAX_BOOL (Condition)) {
        return Condition;
      }

      Condition = ProcessCondition (&State, CFlag, &DoIt);
      if (BORAX_BOOL (Condition)) {
        return Condition;
      }

      Condition = DecodeField (&State, &Opcount, 3);
      if (BORAX_BOOL (Condition)) {
        return Condition;
      }

      Condition = ConditionalCoBind (&State, DoIt, Opcount);
      if (BORAX_BOOL (Condition)) {
        return Condition;
      }

      if (DoIt) {
        return BoraxTaskExitFunction (Task);
      } else {
        Task->Registers.PC = State.Pos;
        return BORAX_NIL;
      }
    }

    case BORAX_OPCODE_EXIT:
    {
      UINT8         CFlag   = (Opcode >> 3) & 0x01;
      UINT8         Opcount = (Opcode >> 0) & 0x07;
      BOOLEAN       DoIt;
      LOCATION      Location;
      BORAX_OBJECT  Exit;

      Condition = DecodeField (&State, &CFlag, 1);
      if (BORAX_BOOL (Condition)) {
        return Condition;
      }

      Condition = ProcessCondition (&State, CFlag, &DoIt);
      if (BORAX_BOOL (Condition)) {
        return Condition;
      }

      Condition = DecodeField (&State, &Opcount, 3);
      if (BORAX_BOOL (Condition)) {
        return Condition;
      }

      Condition = ReadLocation (&State, &Location);
      if (BORAX_BOOL (Condition)) {
        return Condition;
      }

      Condition = ConditionalCoBind (&State, DoIt, Opcount);
      if (BORAX_BOOL (Condition)) {
        return Condition;
      }

      if (DoIt) {
        Condition = LoadLocation (&State, &Location, &Exit);
        if (BORAX_BOOL (Condition)) {
          return Condition;
        }

        return BoraxTaskTakeExit (Task, Exit);
      } else {
        Task->Registers.PC = State.Pos;
        return BORAX_NIL;
      }
    }

    case BORAX_OPCODE_PUSH_EXIT:
    {
      LOCATION    Location;
      UINT16      JumpTarget;
      BORAX_EXIT  *Exit;

      Condition = ReadLocation (&State, &Location);
      if (BORAX_BOOL (Condition)) {
        return Condition;
      }

      Condition = ReadJumpTarget (&State, &JumpTarget);
      if (BORAX_BOOL (Condition)) {
        return Condition;
      }

      Condition = BoraxTaskPushExit (Task, JumpTarget, &Exit);
      if (BORAX_BOOL (Condition)) {
        return Condition;
      }

      Condition = StoreLocation (&State, &Location, BORAX_MAKE_POINTER (Exit));
      if (BORAX_BOOL (Condition)) {
        return Condition;
      }

      Task->Registers.PC = State.Pos;
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
          LOCATION               Location;
          BORAX_MULTIPLE_VALUES  *Values;

          Condition = ReadLocation (&State, &Location);
          if (BORAX_BOOL (Condition)) {
            return Condition;
          }

          Status = BoraxCopyMultipleValues (
                     Task->Interp,
                     Task->Registers.VR,
                     &Values
                     );
          if (EFI_ERROR (Status)) {
            return BoraxPrimitiveHeapExhausted (Task->Interp);
          }

          Condition = StoreLocation (
                        &State,
                        &Location,
                        BORAX_MAKE_POINTER (Values)
                        );
          if (BORAX_BOOL (Condition)) {
            return Condition;
          }

          // TODO: Maybe make BoraxMakeMultipleValues et al. return a condition
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

          break;
      }

      Task->Registers.PC = State.Pos;
      return BORAX_NIL;
    }

    case BORAX_OPCODE_MOVE:
    {
      UINT8     CFlag = Opcode & 0x0F;
      BOOLEAN   DoIt;
      LOCATION  Dst, Src;

      Condition = DecodeField (&State, &CFlag, 4);
      if (BORAX_BOOL (Condition)) {
        return Condition;
      }

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
               BoraxCString (L"Not implemented: instruction ~2,'0X"),
               ARRAY_SIZE (Args),
               Args
               );
    }
  }
}

STATIC BORAX_OBJECT
EFIAPI
BytecodeFunctionName (
  IN BORAX_INTERPRETER  *Interp,
  IN BORAX_OBJECT       Function,
  OUT BORAX_OBJECT      *Name
  )
{
  BORAX_OBJECT             Condition;
  BORAX_BYTECODE_FUNCTION  *F;

  Condition = GetBytecodeFunction (Interp, Function, &F);
  if (BORAX_BOOL (Condition)) {
    return Condition;
  }

  *Name = F->Name;
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
           BoraxCString (L"Not implemented: BytecodeFunctionShared"),
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
