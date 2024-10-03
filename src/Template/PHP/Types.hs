module Template.PHP.Types where

import Data.Data (Data (..))
import Data.List (intercalate)
import Control.DeepSeq (NFData (rnf))


import TreeSitter.Node ( Node(..), TSPoint(TSPoint, pointRow, pointColumn) )
import qualified Data.Vector as V
import Template.Fuddle.Ast (BinaryOp(EqOP))


data NodeEntry = NodeEntry {
  name :: String
  , start :: TSPoint
  , end :: TSPoint
  , children :: [NodeEntry]
  }

instance Show NodeEntry where
  show ne = "ne " <> ne.name <> " (" <> showRange ne.start ne.end
          <> case ne.children of
            [] -> ""
            rest -> ", c = [" <> intercalate ", " (map (.name) (take 5 ne.children)) <> " }"

showRange :: TSPoint -> TSPoint -> String
showRange start end = "(" <> show start.pointRow <> "," <> show start.pointColumn <> ") - (" <> show end.pointRow <> "," <> show end.pointColumn <> ")"


instance Eq NodeEntry where
  NodeEntry a b c d == NodeEntry a' b' c' d' = a == a' && b == b' && c == c' && d == d'

instance Ord NodeEntry where
  compare n1 n2 =
       compare n1.name n2.name
    <> compare n1.start n2.start
    <> compare n1.end n2.end

instance Data NodeEntry where
  toConstr _ = error "toConstr: NodeEntry"
  gunfold _ _ = error "gunfold: NodeEntry"
  dataTypeOf _ = error "dataTypeOf: NodeEntry"

instance NFData NodeEntry where
  rnf NodeEntry { name = a, start = b, end = c, children = d } =
    rnf a `seq` rnf b `seq` rnf c `seq` rnf d

instance NFData TSPoint where
  rnf (TSPoint a b) = rnf a `seq` rnf b


instance Ord TSPoint where
  compare (TSPoint a b) (TSPoint a' b') =
    case compare a a' of
      EQ -> compare b b'
      rez -> rez

instance Data TSPoint where
  toConstr _ = error "toConstr: TSPoint"
  gunfold _ _ = error "gunfold: TSPoint"
  dataTypeOf _ = error "dataTypeOf: TSPoint"

