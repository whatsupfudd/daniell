module RunTime.Interpreter.Memory

where

import Data.ByteString (ByteString)
import Data.Int (Int32, Int64)
import Data.Vector (Vector)


type BoolM = Bool
type CharM = Char
type IntM = Int32
type LongM = Int64
type FloatM = Float
type DoubleM = Double
type PcPtrM = Int32


data MemType =
  BoolV
  | CharV
  | IntV
  | FloatV
  | DoubleV
  | LongV
  | StringV
  | ConstantRefV
  -- the heap is a stack, the current is 0, its parent is 1, etc.
  | HeapRefV Int32


type Stack = [ (MemType, Int32) ]

type Heap = Vector (MemType, HeapValue)

data HeapValue =
  ArrayHV (Vector HeapValue)
  | TupleHV Int32 (Vector HeapValue)
  | StructHV [ByteString] (Vector HeapValue)
  | StringHV ByteString
  | FloatHV Float
  | DoubleHV Double
  | IntHV Int32
  | LongHV Int64


