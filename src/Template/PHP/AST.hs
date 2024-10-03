module Template.PHP.AST where

import qualified Data.Vector as V

import TreeSitter.Node (TSPoint(..))
import Template.PHP.Types (showRange)
import Options (EnvOptions(danHome))

type SegmentPos = (TSPoint, TSPoint)

showPoint :: TSPoint -> String
showPoint pt = "(" <> show pt.pointRow <> ", " <> show pt.pointColumn <> ")"

showSegmentRange :: SegmentPos -> String
showSegmentRange (start, end) = showRange start end


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


data PhpAction =
  Verbatim Int
  | Statement PhpStatement
  | CommentA Int
  | MiscST String SegmentPos
  | Interpolation [PhpAction]
  | NoOpAct

instance Show PhpAction where
  show action = case action of
    Verbatim uid -> "Verbatim " <> show uid
    Statement stmt -> "Statement " <> show stmt
    CommentA uid -> "CommentA " <> show uid
    MiscST msg pos -> "MiscST " <> msg <> " " <> showSegmentRange pos
    Interpolation acts -> "Interpolation " <> show (map show acts)
    NoOpAct -> "noop"

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
  | ForEachST PhpExpression (Bool, VariableSpec) (Maybe (Bool, VariableSpec)) PhpAction
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
  | FunctionDefST QualifiedName PhpAction
  -- Class: attributes, modifiers, name, extends, implements, members
  | ClassDefST (Maybe AttributeList) [MemberModifier] Int (Maybe QualifiedName) (Maybe [Int]) [ClassMemberDecl]
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
  -- A holder for dangling statements (else, else-if, end-if, ...)
  | DanglingST DanglingClosure
  deriving Show


data DanglingClosure =
  StatementDC PhpAction
  | EndDeclareDC
  | EndForDC
  | EndForEachDC
  | EndIfDC
  | EndSwitchDC
  | EndWhileDC
  deriving Show


data PhpExpression =
  Literal LiteralValue
  | Variable VariableSpec
  | Symbol Int
  | BinaryOp BinaryOps PhpExpression PhpExpression
  | UnaryOp UnaryOps PhpExpression
  | TernaryOp PhpExpression PhpExpression PhpExpression
  | FunctionCall CallerSpec [PhpExpression]
  | ArrayAccess PhpExpression PhpExpression
  | ArrayLiteral [PhpExpression]
  | Parenthesized [PhpExpression]
  | AssignVar Bool PhpExpression PhpExpression
  | CommentX Int
  | MiscExpr String (TSPoint, TSPoint)
  | Subscript PhpExpression (Maybe PhpExpression) -- var  ()'{' | '[') expr (']' | '}')
  | MemberAccess PhpExpression MemberAccessMode        -- var->expr
  | MemberCall PhpExpression MemberAccessMode [PhpExpression]        -- var->expr
  | Conditional PhpExpression PhpExpression PhpExpression
  | Casting Int PhpExpression
  | ObjectCreation MemberAccessMode [PhpExpression]
  | Include IncludeMode PhpExpression
  | AugmentedAssign PhpExpression BinaryOps PhpExpression
  | ScopeCall [ScopeMode] [PhpExpression]
  | ScopedPropertyAccess ScopeMode PhpExpression
  | ErrorSuppression PhpExpression
  | ListLiteral [PhpExpression]
  | HereDoc Int Int Int
  | ClassConstantAccess ScopeMode Int
  | ShellCommand PhpExpression
  | ThrowExpr PhpExpression
  -- UpdateExpr posFlag (true: prefix, false: postfix) selectedOp var
  | UpdateExpr Bool UpdateOp PhpExpression
  | CloneExpr PhpExpression
  deriving Show


data CallerSpec =
  QualNameCS QualifiedName
  | VariableCS VariableSpec
  | SubscriptCS PhpExpression
  deriving Show


data MemberAccessMode =
  NameMT Int
  | ParentMT
  | SelfMT
  | VarExprMT PhpExpression
  | StringMT PhpExpression
  deriving Show


data ScopeMode =
  RelativeSelfSM
  | RelativeStaticSM
  | RelativeParentSM
  | NamedSM Int
  | VariableSM VariableSpec
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
  | BitShiftLeftOp
  | BitShiftRightOp
  deriving Show


data UpdateOp =
  IncOp
  | DecOp
  deriving Show


data LiteralValue =
  BoolLiteral Int
  | IntLiteral Int
  | FloatLiteral Int
  -- string [bB] "<content>" | [bB] '<content>'
  | StringLiteral Bool StringDetails 
  | NullLiteral
  deriving Show

data StringDetails =
  SimpleString [EncapsedMode]
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
  deriving (Show, Eq)


newtype Attribute = LabelAT Int
  deriving Show

newtype AttributeGroup = AttributeGroup [Attribute]
  deriving Show

newtype AttributeList = AttributeList [AttributeGroup]
  deriving Show


data QualifiedName =
    SimpleNameQN Int
  | QualifiedNameQN [Int]
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
    MethodImplementationMI PhpAction
  | ReturnTypeMI TypeDecl
  | AbstractMI
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
