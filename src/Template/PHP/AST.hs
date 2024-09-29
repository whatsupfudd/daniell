module Template.PHP.AST where

import TreeSitter.Node (TSPoint(..))

type SegmentPos = (TSPoint, TSPoint)


data PhpAction =
  Verbatim Int
  | Statement PhpStatement
  | CommentA Int
  | MiscST String SegmentPos
  | Interpolation [PhpAction]
  | NoOpAct
  deriving Show

data PhpStatement =
  IfST PhpExpression PhpAction (Maybe PhpAction)
  | ClassST Int [ PhpStatement ]
  | EchoST [PhpExpression]
  | ExitST (Maybe PhpExpression)
  | ForEachST PhpExpression VariableSpec (Maybe VariableSpec) PhpAction
  | BlockST [PhpAction]
  | ExpressionST PhpExpression
  deriving Show


data PhpExpression =
  Literal LiteralValue
  | Variable VariableSpec
  | Symbol Int
  | BinaryOp BinaryOps PhpExpression PhpExpression
  | UnaryOp UnaryOps PhpExpression
  | TernaryOp PhpExpression PhpExpression PhpExpression
  | FunctionCall Int [PhpExpression]
  | ArrayAccess PhpExpression PhpExpression
  | ArrayLiteral [PhpExpression]
  | Parenthesized [PhpExpression]
  | AssignLocal Int PhpExpression
  | Require Bool PhpExpression
  | CommentX Int
  | MiscExpr String (TSPoint, TSPoint)
  | Subscript VariableSpec PhpExpression -- var  ()'{' | '[') expr (']' | '}')
  | MemberAccess VariableSpec Int        -- var->expr
  | Conditional PhpExpression PhpExpression PhpExpression
  | Casting Int PhpExpression
  deriving Show

data VariableSpec =
  SimpleVS Int
  | Dynamic Int Int
  | ComplexVS PhpExpression
  deriving Show


data UnaryOps =
  NotOp
  | NegOp
  deriving Show


data BinaryOps =
  DotOp
  | AddOp
  | SubOp
  | MulOp
  | DivOp
  | ModOp
  | PowOp
  | BitAndOp
  | BitOrOp
  | BitXorOp
  | EqOp
  | EqlOp
  | NeqOp
  | GtOp
  | LtOp
  | GteOp
  | LteOp
  | AndOp
  | OrOp
  deriving Show

data LiteralValue =
  BoolLiteral Int
  | IntLiteral Int
  | StringLiteral StringDetails 
  deriving Show

data StringDetails =
  SimpleString (Maybe Int)
  | EncapsedString (Maybe (Int, Int))
  deriving Show

