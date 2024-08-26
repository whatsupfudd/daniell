module Template.Fuddle.AAst where

import Data.Text (Text)


data StatementFd =
  IfShortST ExprFd
  | ElseIfShortST ExprFd
  | ElseShortST
  | IfThenElseST ExprFd [ StatementFd ] [ StatementFd ]
  | ValueST ExprFd
  | BlockStartST
  | BlockEndST
  | SpecialST SpecialSymbolFd
  | ImportST VariableFd
  deriving (Show)

data SpecialSymbolFd =
  IfElseNilSS ExprFd
  deriving (Show)


data ExprFd =
  LiteralEX LiteralFd
  | ApplicationEX Text [ ExprFd ]
  | VariableEX VariableFd
  | BinaryOperEX BinaryOperatorFd ExprFd ExprFd
  | UnaryOperEX UnaryOperatorFd ExprFd
  | ParenthesisEX ExprFd
  deriving (Show)


data VariableFd =
  LocVR LocalVarFd
  | QualVar QualifiedVarFd
  deriving (Show)


data BinaryOperatorFd =
  AndOP
  | OrOP
  | PlusOP
  | MinusOP
  | MultiplyOP
  | DivideOP
  | ModuloOP
  | ExponentialOP
  | PipeOP
  | EqOP
  | NeOP
  | LtOP
  | LeOP
  | GtOP
  | GeOP
  | ConcatOP
  | CarAddOP
  deriving (Show)


data UnaryOperatorFd =
  NotOP
  | NegOP
  deriving (Show)


data LiteralFd =
  IntLV Int
  | StringLV Text
  | CharLV Char
  | BoolLV Bool
  deriving (Show)


data QualifiedVarFd = QualifiedVar Bool [ Text ]
  deriving (Show)


newtype LocalVarFd = LocalVar (Maybe Text)
  deriving (Show)


type Identifier = Text
