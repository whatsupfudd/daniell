module RunTime.Interpreter.OpCodes
where

import Data.Int (Int32, Int64)

import RunTime.Interpreter.Memory


type VarID = Int32
type FieldID = Int32
type RegID = Int32


data OpCode =
  NOP
  -- Variable access:
  | SET_VAR VarID
  | GET_VAR VarID
  | GET_FIELD FieldID
  | SET_FIELD FieldID
  -- Register access:
  | SET_REG_BOOL RegID BoolM
  | SET_REG_CHAR RegID CharM
  | SET_REG_INT RegID IntM
  | SET_REG_FLOAT RegID FloatM
  | SET_REG_DOUBLE RegID DoubleM
  -- Comparisons:
  | CMP_BOOL RegID RegID
  | CMP_CHAR RegID RegID
  | CMP_INT RegID RegID
  | CMP_FLOAT RegID RegID
  | CMP_DOUBLE RegID RegID
  -- PC ops:
  | JUMP_ABS PcPtrM
  | JUMP_REL Int32
  | JUMP_INDEX RegID
  -- Stack Access:
  | PUSH_BOOL RegID
  | PUSH_BOOL_IMM BoolM
  | PUSH_CHAR RegID
  | PUSH_CHAR_IMM CharM
  | PUSH_INT RegID
  | PUSH_INT_IMM IntM
  | PUSH_FLOAT RegID
  | PUSH_FLOAT_IMM FloatM
  | PUSH_DOUBLE RegID
  | PUSH_DOUBLE_IMM DoubleM
  | POP_BOOL RegID
  | POP_CHAR RegID
  | POP_INT RegID
  | POP_FLOAT RegID
  | POP_DOUBLE RegID
  | POP_BOOL_VOID
  | POP_CHAR_VOID
  | POP_INT_VOID
  | POP_FLOAT_VOID
  | POP_DOUBLE_VOID
  -- Invoke:
  | EVAL
  -- Heap management (save new heap ID to register):
  | ALLOC_ABS IntM RegID
  | ALLOC_REL RegID RegID
  deriving Show

  

  