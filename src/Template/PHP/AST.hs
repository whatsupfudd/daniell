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
  -- Empty:
  NoOpST
  -- Compound:
  | BlockST [PhpAction]
  -- named label:
  | NamedLabelST
  -- Expression:
  | ExpressionST PhpExpression
  -- If:
  | IfST PhpExpression PhpAction (Maybe PhpAction)
  -- Switch:
  | SwitchST
  -- While:
  | WhileST
  -- Do:
  | DoST
  -- For:
  | ForST
  -- Foreach:
  | ForEachST PhpExpression (Bool, VariableSpec) (Maybe VariableSpec) PhpAction
  -- Goto:
  | GotoST
  -- Continue:
  | ContinueST
  -- Break:
  | BreakST
  -- Return:
  | ReturnST (Maybe PhpExpression)
  -- Try:
  | TryST
  -- Declare:
  | DeclareST
  -- Echo:
  | EchoST [PhpExpression]
  -- Exit:
  | ExitST (Maybe PhpExpression)
  -- Unset:
  | UnsetST
  -- Const:
  | ConstDeclST
  -- Function definition:
  | FunctionDefST
  -- Class: attributes, modifiers, name, extends, implements, members
  | ClassDefST (Maybe AttributeList) [MemberModifier] Int (Maybe Int) (Maybe [Int]) [ClassMemberDecl]
  -- Interface:
  | InterfaceDefST
  -- Trait:
  | TraitDefST
  -- Enum:
  | EnumDefST
  -- Namespace:
  | NamespaceDefST
  -- Namespace use:
  | NamespaceUseST
  -- Global:
  | GlobalDeclST
  -- Static:
  | FunctionStaticST
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
  | AssignVar PhpExpression PhpExpression
  | CommentX Int
  | MiscExpr String (TSPoint, TSPoint)
  | Subscript PhpExpression (Maybe PhpExpression) -- var  ()'{' | '[') expr (']' | '}')
  | MemberAccess PhpExpression MemberAccessMode        -- var->expr
  | Conditional PhpExpression PhpExpression PhpExpression
  | Casting Int PhpExpression
  | ObjectCreation Int [PhpExpression]
  | Include IncludeMode PhpExpression
  | AugmentedAssign PhpExpression BinaryOps PhpExpression
  | ScopeCall [ScopeMode] [PhpExpression]
  | ScopedPropertyAccess ScopeMode PhpExpression
  deriving Show


data MemberAccessMode =
  NameMT Int
  | ParentMT
  | VarExprMT PhpExpression
  deriving Show


data ScopeMode =
  RelativeSelfSM
  | RelativeStaticSM
  | NamedSM Int
  deriving Show


data IncludeMode =
  IncludeOnceIM
  | RequireOnceIM
  | RequireIM
  | IncludeIM
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
  | NeqlOp
  | GtOp
  | LtOp
  | GteOp
  | LteOp
  | AndOp
  | OrOp
  | InstanceOfOp
  deriving Show

data LiteralValue =
  BoolLiteral Int
  | IntLiteral Int
  -- string [bB] "<content>" | [bB] '<content>'
  | StringLiteral Bool StringDetails 
  | NullLiteral
  deriving Show

data StringDetails =
  SimpleString (Maybe Int)
  | EncapsedString [EncapsedMode]
  deriving Show


data EncapsedMode =
  ContentEM Int
  | EscapeEM Int
  -- the PhpExpression is a (Variable varSpec)
  | VariableEM PhpExpression
  | CurlyEM Bool
  deriving Show


data MemberModifier =
  AbstractCM
  | FinalCM
  | ReadonlyCM
  | PublicCM
  | ProtectedCM
  | PrivateCM
  | StaticCM
  | ReferenceCM
  | NoOpCM
  deriving Show


newtype Attribute = LabelAT Int
  deriving Show

newtype AttributeGroup = AttributeGroup [Attribute]
  deriving Show

newtype AttributeList = AttributeList [AttributeGroup]
  deriving Show


data ClassMemberDecl =
    CommentCDecl Int
  | ConstantCDecl [MemberModifier] [(Int, PhpExpression)]
  | PropertyCDecl (Maybe AttributeList) [MemberModifier] VariableSpec (Maybe PhpExpression)
  | MethodCDecl (Maybe AttributeList) [MemberModifier] MemberAccessMode [VariableSpec] MethodImplementation
  -- Constructor / Destructor are just __construct / __destruct methods, they are taken by MethodCDecl.
  | ConstructorCDecl
  | DestructorCDecl
  | TraitUseCDecl [Int] (Maybe UseList)
  deriving Show

newtype UseList = UseList [Int]
  deriving Show

data MethodImplementation =
    MethodImplementation PhpAction
  | ReturnType TypeDecl
  deriving Show


data TypeDecl =
    VoidTD
  | ArrayTD
  | CallableTD
  | IterableTD
  | BoolTD
  | FloatTD
  | IntTD
  | StringTD
  | NullTD
  deriving Show
