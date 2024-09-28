module Template.PHP.Types where

import Data.Data (Data (..))
import Control.DeepSeq (NFData (rnf))


import TreeSitter.Node ( Node(..), TSPoint(TSPoint, pointRow, pointColumn) )
import qualified Data.Vector as V


data NodeEntry = NodeEntry {
  name :: String
  , start :: TSPoint
  , end :: TSPoint
  , children :: [NodeEntry]
  }
  deriving Show

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


type SegmentPos = (TSPoint, TSPoint)


data PhpAction =
  Verbatim Int
  | Statement PhpStatement
  | Expression PhpExpression
  | Block [PhpAction]
  | CommentA Int
  | MiscST String SegmentPos
  deriving Show

data PhpStatement =
  IfST PhpExpression PhpAction (Maybe PhpAction)
  | ClassST Int [ PhpStatement ]
  deriving Show


data PhpExpression =
  Literal Int
  | Variable Int
  | BinaryOp Int
  | UnaryOp UnaryOps PhpExpression
  | TernaryOp PhpExpression PhpExpression PhpExpression
  | FunctionCall String [PhpExpression]
  | ArrayAccess PhpExpression PhpExpression
  | ArrayLiteral [PhpExpression]
  | Parenthesized [PhpExpression]
  | CommentX Int
  | MiscExpr String (TSPoint, TSPoint)
  deriving Show


data UnaryOps =
  NotOp
  | NegOp
  deriving Show


data PhpContext = PhpContext {
  logic :: V.Vector PhpAction
  , contentDemands :: V.Vector SegmentPos
  }
  deriving Show


initPhpContext :: PhpContext
initPhpContext = PhpContext {
    logic = V.empty
    , contentDemands = V.empty
  }
