module RunTime.Interpreter.Memory

where

import Data.Int (Int32, Int64)
import qualified Data.Map as Mp


type BoolM = Bool
type CharM = Char
type IntM = Int64
type FloatM = Float
type DoubleM = Double
type PcPtrM = Int32


data MemValue =
  BoolV BoolM
  | CharV CharM
  | IntV IntM
  | FloatV FloatM
  | DoubleV DoubleM
  | PcPtrV PcPtrM


type Stack = [ MemValue ]


data Variable =
  Simple MemValue
  | Complex (Mp.Map String Variable)
