module Template.Fuddle.BAst where

import Data.ByteString (ByteString)
import Data.Text (Text)
import Data.Sequence (Seq)


data StatementFd =
  SeqST [ StatementFd ]
  | ElseIfShortST Bool Expression (Maybe [Text])
  | BlockEndST
  | IfElseNilSS Expression (Maybe [Text])     -- @? <bool expr> @] (args)
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
  | BinOpExpr BinaryOp Expression Expression
  | ReductionExpr QualifiedIdent [ Expression ]
  deriving Show


data LiteralValue =
  NumeralValue Int
  | BoolValue Bool
  | CharValue Char
  | StringValue Text
  | TupleValue [ LiteralValue ]
  | ArrayValue [ LiteralValue ]
  deriving Show


data UnaryOp =
  NegateOP
  | NotOP
  | BitNotOP
  deriving Show


data BinaryOp =
  -- Arithmetic
  AddOP | SubstractOP | MultiplyOP | DivideOP | ModuloOP
  -- Bitwise
  | BitXorOP | BitOrOP | BitShiftLeftOP | BitShiftRightOP
  -- Logical
  | OrOP | AndOP
  -- Comparison
  | EqOP | NeOP | LtOP | LeOP | GeOP | GtOP
  -- Array
  | ConcatOP | CarAddOP
  deriving Show


type QualifiedIdent = (Seq Text)
type IdentWithParam = (QualifiedIdent, [ QualifiedIdent ])

data BlockAst = 
  VerbatimBlock ByteString
  | LogicBlock StatementFd
  deriving Show

data NodeAst =
  CloneText ByteString
  | AstLogic StmtAst
  deriving Show

data StmtAst = StmtAst {
      statement :: StatementFd
      , children :: [NodeAst]
    }
  deriving Show
