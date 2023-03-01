module Template.TemplateText.Ast where

import Data.Text (Text)


data GoStmt =
  IfST PipelineExpr
  | WithST QualifiedVar
  | RangeST PipelineExpr
  | ElseIfST PipelineExpr
  | ElseST
  | EndST
  | BreakST
  | ContinueST
  -- TODO: check if the template context is a variable.
  | PartialST Text (Maybe QualifiedVar)
  | TemplateST Text (Maybe QualifiedVar)
  -- Binds a local name to a value, possibly (Bool) adding the name.
  | VarBindingST Bool LocalVar PipelineExpr
  | ValueST PipelineExpr
  deriving (Show)

data PipelineExpr =
  LiteralEX LiteralValue
  | CallEX Identifier [ PipelineExpr ]
  | VariableEX Variable
  | BinaryOperExpr BinaryOperator PipelineExpr PipelineExpr
  | ParenthesisExpr PipelineExpr
  -- TODO: define a real RangeEX type
  | RangeEX Variable
  deriving (Show)


data Variable =
  LocVR LocalVar
  | QualVar QualifiedVar
  deriving (Show)


data BinaryOperator =
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
  deriving (Show)


data LiteralValue =
  IntLV Int
  | StringLV Text
  | CharLV Char
  | BoolLV Bool
  deriving (Show)


data QualifiedVar = QualifiedVar Bool [ Text ]
  deriving (Show)


newtype LocalVar = LocalVar (Maybe Text)
  deriving (Show)


type Identifier = Text
