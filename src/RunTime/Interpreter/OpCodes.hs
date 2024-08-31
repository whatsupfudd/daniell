{-# LANGUAGE InstanceSigs #-}
{-# OPTIONS_GHC -Wno-overlapping-patterns #-}
module RunTime.Interpreter.OpCodes
where

import Data.Char (ord)
import Data.Int (Int32, Int64)
import Data.Text (Text)

import RunTime.Interpreter.Memory
import Template.Golang.Parser (callExpr)


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
  | JUMP_TRUE PcPtrM
  | JUMP_FALSE PcPtrM
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
  fromEnum (JUMP_TRUE _) = 24
  fromEnum (JUMP_FALSE _) = 25
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
  toEnum 10 = SET_REG_CONST undefined undefined
  toEnum 11 = CMP_BOOL undefined undefined
  toEnum 12 = CMP_CHAR undefined undefined
  toEnum 13 = CMP_INT undefined undefined
  toEnum 14 = CMP_FLOAT undefined undefined
  toEnum 15 = CMP_DOUBLE undefined undefined
  toEnum 16 = CMP_BOOL_IMM
  toEnum 17 = CMP_CHAR_IMM
  toEnum 18 = CMP_INT_IMM
  toEnum 19 = CMP_FLOAT_IMM
  toEnum 20 = CMP_DOUBLE_IMM
  toEnum 21 = JUMP_ABS undefined
  toEnum 22 = JUMP_REL undefined
  toEnum 23 = JUMP_INDEX undefined
  toEnum 24 = JUMP_TRUE undefined
  toEnum 25 = JUMP_FALSE undefined
  toEnum 26 = PUSH_BOOL undefined
  toEnum 27 = PUSH_CHAR undefined
  toEnum 28 = PUSH_FLOAT undefined
  toEnum 29 = PUSH_DOUBLE undefined
  toEnum 30 = PUSH_CONST undefined
  toEnum 31 = PUSH_BOOL_IMM undefined
  toEnum 32 = PUSH_CHAR_IMM undefined
  toEnum 33 = PUSH_INT_IMM undefined
  toEnum 34 = PUSH_FLOAT_IMM undefined
  toEnum 35 = PUSH_DOUBLE_IMM undefined
  toEnum 36 = PUSH_CONST_IMM undefined
  toEnum 37 = POP_BOOL undefined
  toEnum 38 = POP_CHAR undefined
  toEnum 39 = POP_INT undefined
  toEnum 40 = POP_FLOAT undefined
  toEnum 41 = POP_DOUBLE undefined
  toEnum 42 = POP_CONST undefined
  toEnum 43 = POP_BOOL_VOID
  toEnum 44 = POP_CHAR_VOID
  toEnum 45 = POP_INT_VOID
  toEnum 46 = POP_FLOAT_VOID
  toEnum 47 = POP_DOUBLE_VOID
  toEnum 48 = POP_CONST_VOID
  toEnum 49 = REDUCE undefined undefined
  toEnum 50 = ALLOC_ABS undefined undefined
  toEnum 51 = ALLOC_REL undefined undefined
  toEnum 52 = IADD
  toEnum 53 = ISUB
  toEnum 54 = IMUL
  toEnum 55 = IDIV
  toEnum 56 = IMOD
  toEnum 57 = ISHL
  toEnum 58 = ISHR
  toEnum 59 = INEGATE
  toEnum 60 = FADD
  toEnum 61 = FSUB
  toEnum 62 = FMUL
  toEnum 63 = FDIV
  toEnum 64 = FNEGATE
  toEnum 65 = DADD
  toEnum 66 = DSUB
  toEnum 67 = DMUL
  toEnum 68 = DDIV
  toEnum 69 = DNEGATE
  toEnum 70 = BAND
  toEnum 71 = BOR
  toEnum 72 = BXOR
  toEnum 73 = BNOT
  toEnum 74 = ARR_CONCAT
  toEnum 75 = ARR_ADD
  toEnum 76 = RETURN
  toEnum 77 = HALT
  toEnum _ = error "toEnum: bad argument"

opParCount :: OpCode -> Int
opParCount NOP = 0
opParCount (SET_VAR _) = 1
opParCount (GET_VAR _) = 1
opParCount (GET_FIELD _) = 1
opParCount (SET_FIELD _) = 1
opParCount (SET_REG_BOOL _ _) = 2
opParCount (SET_REG_CHAR _ _) = 2
opParCount (SET_REG_INT _ _) = 2
opParCount (SET_REG_FLOAT _ _) = 2
opParCount (SET_REG_DOUBLE _ _) = 2
opParCount (SET_REG_CONST _ _) = 2
opParCount (CMP_BOOL _ _) = 2
opParCount (CMP_CHAR _ _) = 2
opParCount (CMP_INT _ _) = 2
opParCount (CMP_FLOAT _ _) = 2
opParCount (CMP_DOUBLE _ _) = 2
opParCount CMP_BOOL_IMM = 0
opParCount CMP_CHAR_IMM = 0
opParCount CMP_INT_IMM = 0
opParCount CMP_FLOAT_IMM = 0
opParCount CMP_DOUBLE_IMM = 0
opParCount (JUMP_ABS _) = 1
opParCount (JUMP_REL _) = 1
opParCount (JUMP_INDEX _) = 1
opParCount (JUMP_TRUE _) = 1
opParCount (JUMP_FALSE _) = 1
opParCount (PUSH_BOOL _) = 1
opParCount (PUSH_CHAR _) = 1
opParCount (PUSH_FLOAT _) = 1
opParCount (PUSH_DOUBLE _) = 1
opParCount (PUSH_CONST _) = 1
opParCount (PUSH_BOOL_IMM _) = 1
opParCount (PUSH_CHAR_IMM _) = 1
opParCount (PUSH_INT_IMM _) = 1
opParCount (PUSH_FLOAT_IMM _) = 1
opParCount (PUSH_DOUBLE_IMM _) = 1
opParCount (PUSH_CONST_IMM _) = 1
opParCount (POP_BOOL _) = 1
opParCount (POP_CHAR _) = 1
opParCount (POP_INT _) = 1
opParCount (POP_FLOAT _) = 1
opParCount (POP_DOUBLE _) = 1
opParCount (POP_CONST _) = 1
opParCount POP_BOOL_VOID = 0
opParCount POP_CHAR_VOID = 0
opParCount POP_INT_VOID = 0
opParCount POP_FLOAT_VOID = 0
opParCount POP_DOUBLE_VOID = 0
opParCount POP_CONST_VOID = 0
opParCount (REDUCE _ _) = 2
opParCount (ALLOC_ABS _ _) = 2
opParCount (ALLOC_REL _ _) = 2
opParCount IADD = 0
opParCount ISUB = 0
opParCount IMUL = 0
opParCount IDIV = 0
opParCount IMOD = 0
opParCount ISHL = 0
opParCount ISHR = 0
opParCount INEGATE = 0
opParCount FADD = 0
opParCount FSUB = 0
opParCount FMUL = 0
opParCount FDIV = 0
opParCount FNEGATE = 0
opParCount DADD = 0
opParCount DSUB = 0
opParCount DMUL = 0
opParCount DDIV = 0
opParCount DNEGATE = 0
opParCount BAND = 0
opParCount BOR = 0
opParCount BXOR = 0
opParCount BNOT = 0
opParCount ARR_CONCAT = 0
opParCount ARR_ADD = 0
opParCount RETURN = 0
opParCount HALT = 0

toInstr :: OpCode -> [Int32]
toInstr NOP = [0]
toInstr (SET_VAR a1) = [1, a1]
toInstr (GET_VAR a1) = [2, a1]
toInstr (GET_FIELD a1) = [3, a1]
toInstr (SET_FIELD a1) = [4, a1]
toInstr (SET_REG_BOOL a1 a2) = [5, a1, if a2 then 1 else 0]
toInstr (SET_REG_CHAR a1 a2) = [6, a1, fromIntegral . ord $ a2]
toInstr (SET_REG_INT a1 a2) = [7, a1, a2]
toInstr (SET_REG_FLOAT a1 a2) = [8, a1, round a2]
toInstr (SET_REG_DOUBLE a1 a2) = [9, a1, round a2]
toInstr (SET_REG_CONST a1 a2) = [10, a1, a2]
toInstr (CMP_BOOL a1 a2) = [11, a1, a2]
toInstr (CMP_CHAR a1 a2) = [12, a1, a2]
toInstr (CMP_INT a1 a2) = [13, a1, a2]
toInstr (CMP_FLOAT a1 a2) = [14, a1, a2]
toInstr (CMP_DOUBLE a1 a2) = [15, a1, a2]
toInstr CMP_BOOL_IMM = [16]
toInstr CMP_CHAR_IMM = [17]
toInstr CMP_INT_IMM = [18]
toInstr CMP_FLOAT_IMM = [19]
toInstr CMP_DOUBLE_IMM = [20]
toInstr (JUMP_ABS a1) = [21, a1]
toInstr (JUMP_REL a1) = [22, a1]
toInstr (JUMP_INDEX a1) = [23, a1]
toInstr (JUMP_TRUE a1) = [24, a1]
toInstr (JUMP_FALSE a1) = [25, a1]
toInstr (PUSH_BOOL a1) = [26, a1]
toInstr (PUSH_CHAR a1) = [27, a1]
toInstr (PUSH_FLOAT a1) = [28, a1]
toInstr (PUSH_DOUBLE a1) = [29, a1]
toInstr (PUSH_CONST a1) = [30, a1]
toInstr (PUSH_BOOL_IMM a1) = [31, if a1 then 1 else 0]
toInstr (PUSH_CHAR_IMM a1) = [32, fromIntegral . ord $ a1]
toInstr (PUSH_INT_IMM a1) = [33, a1]
toInstr (PUSH_FLOAT_IMM a1) = [34, round a1]
toInstr (PUSH_DOUBLE_IMM a1) = [35, round a1]
toInstr (PUSH_CONST_IMM a1) = [36, a1]
toInstr (POP_BOOL a1) = [37, a1]
toInstr (POP_CHAR a1) = [38, a1]
toInstr (POP_INT a1) = [39, a1]
toInstr (POP_FLOAT a1) = [40, a1]
toInstr (POP_DOUBLE a1) = [41, a1]
toInstr (POP_CONST a1) = [42, a1]
toInstr POP_BOOL_VOID = [43]
toInstr POP_CHAR_VOID = [44]
toInstr POP_INT_VOID = [45]
toInstr POP_FLOAT_VOID = [46]
toInstr POP_DOUBLE_VOID = [47]
toInstr POP_CONST_VOID = [48]
toInstr (REDUCE a1 a2) = [49, a1, a2]
toInstr (ALLOC_ABS a1 a2) = [50, a1, a2]
toInstr (ALLOC_REL a1 a2) = [51, a1, a2]
toInstr IADD = [52]
toInstr ISUB = [53]
toInstr IMUL = [54]
toInstr IDIV = [55]
toInstr IMOD = [56]
toInstr ISHL = [57]
toInstr ISHR = [58]
toInstr INEGATE = [59]
toInstr FADD = [60]
toInstr FSUB = [61]
toInstr FMUL = [62]
toInstr FDIV = [63]
toInstr FNEGATE = [64]
toInstr DADD = [65]
toInstr DSUB = [66]
toInstr DMUL = [67]
toInstr DDIV = [68]
toInstr DNEGATE = [69]
toInstr BAND = [70]
toInstr BOR = [71]
toInstr BXOR = [72]
toInstr BNOT = [73]
toInstr ARR_CONCAT = [74]
toInstr ARR_ADD = [75]
toInstr RETURN = [76]
toInstr HALT = [77]
toInstr a = error $ "fromEnum: bad argument" <> show a
