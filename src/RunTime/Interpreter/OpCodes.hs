{-# LANGUAGE InstanceSigs #-}
{-# OPTIONS_GHC -Wno-overlapping-patterns #-}
module RunTime.Interpreter.OpCodes
where

import Data.Int (Int32, Int64)

import RunTime.Interpreter.Memory


type VarID = Int32
type FieldID = Int32
type RegID = Int32


data OpCode =
  NOP
  -- Heap access:
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
  | SET_REG_CONST RegID Int32
  -- Comparisons:
  | CMP_BOOL RegID RegID
  | CMP_CHAR RegID RegID
  | CMP_INT RegID RegID
  | CMP_FLOAT RegID RegID
  | CMP_DOUBLE RegID RegID
  | CMP_BOOL_IMM
  | CMP_CHAR_IMM
  | CMP_INT_IMM
  | CMP_FLOAT_IMM
  | CMP_DOUBLE_IMM
  -- PC ops:
  | JUMP_ABS PcPtrM
  | JUMP_REL Int32
  | JUMP_INDEX RegID
  | TEST_TRUE PcPtrM
  | TEST_FALSE PcPtrM
  -- Stack Access:
  | PUSH_BOOL RegID
  | PUSH_CHAR RegID
  | PUSH_FLOAT RegID
  | PUSH_DOUBLE RegID
  | PUSH_CONST RegID
  | PUSH_BOOL_IMM BoolM
  | PUSH_CHAR_IMM CharM
  | PUSH_INT_IMM IntM
  | PUSH_FLOAT_IMM FloatM
  | PUSH_DOUBLE_IMM DoubleM
  | PUSH_CONST_IMM Int32
  | POP_BOOL RegID
  | POP_CHAR RegID
  | POP_INT RegID
  | POP_FLOAT RegID
  | POP_DOUBLE RegID
  | POP_CONST RegID
  | POP_BOOL_VOID
  | POP_CHAR_VOID
  | POP_INT_VOID
  | POP_FLOAT_VOID
  | POP_DOUBLE_VOID
  | POP_CONST_VOID
  -- Invoke:
  | REDUCE Int32 Int32    -- function ID + number of arguments pushed on stack.
  -- Heap management (save new heap ID to register):
  | ALLOC_ABS IntM RegID
  | ALLOC_REL RegID RegID
  -- Integer math:
  | IADD
  | ISUB
  | IMUL
  | IDIV
  | IMOD
  | ISHL
  | ISHR
  | INEGATE
  -- Float math:
  | FADD
  | FSUB
  | FMUL
  | FDIV
  | FNEGATE
  -- Double math:
  | DADD
  | DSUB
  | DMUL
  | DDIV
  | DNEGATE
  -- Bool arth:
  | BAND
  | BOR
  | BXOR
  | BNOT
  -- Array ops:
  | ARR_CONCAT
  | ARR_ADD
  -- Control flow:
  | RETURN
  | HALT
  deriving (Show)

instance Enum OpCode where
  fromEnum :: OpCode -> Int
  fromEnum NOP = 0
  fromEnum (SET_VAR _) = 1
  fromEnum (GET_VAR _) = 2
  fromEnum (GET_FIELD _) = 3
  fromEnum (SET_FIELD _) = 4
  fromEnum (SET_REG_BOOL _ _) = 5
  fromEnum (SET_REG_CHAR _ _) = 6
  fromEnum (SET_REG_INT _ _) = 7
  fromEnum (SET_REG_FLOAT _ _) = 8
  fromEnum (SET_REG_DOUBLE _ _) = 9
  fromEnum (SET_REG_CONST _ _) = 10
  fromEnum (CMP_BOOL _ _) = 11
  fromEnum (CMP_CHAR _ _) = 12
  fromEnum (CMP_INT _ _) = 13
  fromEnum (CMP_FLOAT _ _) = 14
  fromEnum (CMP_DOUBLE _ _) = 15
  fromEnum CMP_BOOL_IMM = 16
  fromEnum CMP_CHAR_IMM = 17
  fromEnum CMP_INT_IMM = 18
  fromEnum CMP_FLOAT_IMM = 19
  fromEnum CMP_DOUBLE_IMM = 20
  fromEnum (JUMP_ABS _) = 21
  fromEnum (JUMP_REL _) = 22
  fromEnum (JUMP_INDEX _) = 23
  fromEnum (TEST_TRUE _) = 24
  fromEnum (TEST_FALSE _) = 25
  fromEnum (PUSH_BOOL _) = 26
  fromEnum (PUSH_CHAR _) = 27
  fromEnum (PUSH_FLOAT _) = 28
  fromEnum (PUSH_DOUBLE _) = 29
  fromEnum (PUSH_CONST _) = 30
  fromEnum (PUSH_BOOL_IMM _) = 31
  fromEnum (PUSH_CHAR_IMM _) = 32
  fromEnum (PUSH_INT_IMM _) = 33
  fromEnum (PUSH_FLOAT_IMM _) = 34
  fromEnum (PUSH_DOUBLE_IMM _) = 35
  fromEnum (PUSH_CONST_IMM _) = 36
  fromEnum (POP_BOOL _) = 37
  fromEnum (POP_CHAR _) = 38
  fromEnum (POP_INT _) = 39
  fromEnum (POP_FLOAT _) = 40
  fromEnum (POP_DOUBLE _) = 41
  fromEnum (POP_CONST _) = 42
  fromEnum POP_BOOL_VOID = 43
  fromEnum POP_CHAR_VOID = 44
  fromEnum POP_INT_VOID = 45
  fromEnum POP_FLOAT_VOID = 46
  fromEnum POP_DOUBLE_VOID = 47
  fromEnum POP_CONST_VOID = 48
  fromEnum (REDUCE _ _) = 49
  fromEnum (ALLOC_ABS _ _) = 50
  fromEnum (ALLOC_REL _ _) = 51
  fromEnum IADD = 52
  fromEnum ISUB = 53
  fromEnum IMUL = 54
  fromEnum IDIV = 55
  fromEnum IMOD = 56
  fromEnum ISHL = 57
  fromEnum ISHR = 58
  fromEnum INEGATE = 59
  fromEnum FADD = 60
  fromEnum FSUB = 61
  fromEnum FMUL = 62
  fromEnum FDIV = 63
  fromEnum FNEGATE = 64
  fromEnum DADD = 65
  fromEnum DSUB = 66
  fromEnum DMUL = 67
  fromEnum DDIV = 68
  fromEnum DNEGATE = 69
  fromEnum BAND = 70
  fromEnum BOR = 71
  fromEnum BXOR = 72
  fromEnum BNOT = 73
  fromEnum ARR_CONCAT = 74
  fromEnum ARR_ADD = 75
  fromEnum RETURN = 76
  fromEnum HALT = 77
  fromEnum a = error $ "fromEnum: bad argument" <> show a
  
  toEnum :: Int -> OpCode
  toEnum 0 = NOP
  toEnum 1 = SET_VAR undefined
  toEnum 2 = GET_VAR undefined
  toEnum 3 = GET_FIELD undefined
  toEnum 4 = SET_FIELD undefined
  toEnum 5 = SET_REG_BOOL undefined undefined
  toEnum 6 = SET_REG_CHAR undefined undefined
  toEnum 7 = SET_REG_INT undefined undefined
  toEnum 8 = SET_REG_FLOAT undefined undefined
  toEnum 9 = SET_REG_DOUBLE undefined undefined
  toEnum 10 = CMP_BOOL undefined undefined
  toEnum 11 = CMP_CHAR undefined undefined
  toEnum 12 = CMP_INT undefined undefined
  toEnum 13 = CMP_FLOAT undefined undefined
  toEnum 14 = CMP_DOUBLE undefined undefined
  toEnum 15 = JUMP_ABS undefined
  toEnum 16 = JUMP_REL undefined
  toEnum 17 = JUMP_INDEX undefined
  toEnum 18 = PUSH_BOOL undefined
  toEnum 19 = PUSH_BOOL_IMM undefined
  toEnum 20 = PUSH_CHAR undefined
  toEnum 21 = PUSH_CHAR_IMM undefined
  toEnum 22 = PUSH_INT_IMM undefined
  toEnum 23 = PUSH_FLOAT undefined
  toEnum 24 = PUSH_FLOAT_IMM undefined
  toEnum 25 = PUSH_DOUBLE undefined
  toEnum 26 = PUSH_DOUBLE_IMM undefined
  toEnum 27 = POP_BOOL undefined
  toEnum 28 = POP_CHAR undefined
  toEnum 29 = POP_INT undefined
  toEnum 30 = POP_FLOAT undefined
  toEnum 31 = POP_DOUBLE undefined
  toEnum 32 = POP_BOOL_VOID
  toEnum 33 = POP_CHAR_VOID
  toEnum 34 = POP_INT_VOID
  toEnum 35 = POP_FLOAT_VOID
  toEnum 36 = POP_DOUBLE_VOID
  toEnum 37 = REDUCE undefined undefined
  toEnum 38 = ALLOC_ABS undefined undefined
  toEnum 39 = ALLOC_REL undefined undefined
  toEnum 40 = PUSH_CONST undefined
  toEnum _ = error "toEnum: bad argument"




  

  