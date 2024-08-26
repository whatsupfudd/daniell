module Template.Fuddle.BAst where


import Data.Text (Text)
import Data.Sequence (Seq)

data StatementFd =
  SeqST [ StatementFd ]
  | ElseIfShortST Bool Expression
  | BlockEndST
  | IfElseNilSS Expression
  | ImportST Bool QualifiedIdent (Maybe QualifiedIdent)
  | BindOneST IdentWithParam Expression        -- identWithParam = <expression>
  | LetST [ (IdentWithParam, Expression) ] Expression  -- let [ identWithParam = <expression> ] in <expression>
  | ExpressionST Expression
  deriving Show


data Expression =
  LiteralExpr LiteralValue
  | ParenExpr Expression
  | ArrayExpr [ Expression ]
  | UnaryExpr UnaryOp Expression
  | BinOpExpr BinOp Expression Expression
  | ReductionExpr QualifiedIdent [ Expression ]
  deriving Show


data LiteralValue =
  ArithValue Int
  | BoolValue Bool
  | CharValue Char
  | StringValue Text
  | ArrayValue [ LiteralValue ]
  deriving Show


data UnaryOp =
  NegateOP
  | NotOP
  | BitNotOP
  deriving Show


data BinOp =
  -- Arithmetic
  AddOP
  | SubstractOP
  | MultiplyOP
  | DivideOP
  | ModuloOP
  | BitXorOP
  | BitOrOP
  | BitShiftLeftOP
  | BitShiftRightOP
  -- Logical
  | OrOP
  | AndOP
  -- Comparison
  | EqOP
  | NeOP
  | LtOP
  | LeOP
  | GeOP
  | GtOP
  -- Array
  | ConcatOP
  | CarAddOP
  deriving Show


type QualifiedIdent = (Seq Text)
type IdentWithParam = (QualifiedIdent, [ QualifiedIdent ])
