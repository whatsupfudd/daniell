module Template.PHP.Parser.Expressions where

import Control.Applicative.Combinators (optional)
import Control.Applicative (asum, many, some, (<|>))

import Data.Maybe (isJust)

import TreeSitter.Node ( TSPoint(..) )

import Template.PHP.Parser.Support (debugOpt)
import qualified Template.PHP.Scanner as B
import qualified Template.PHP.Class as B
import qualified Template.PHP.State as B
import Template.PHP.AST
import Template.PHP.Parser.Types


expressionS :: ScannerB PhpExpression
expressionS = do
  asum [
      parenExprS
      , unaryOpS
      , functionCallS
      , debugOpt "assignS" assignS
      , requireOnceS
      , requireS
      , includeS
      , includeOnceS
      , binaryOpS
      , symbolAccessS
      , literalS
      , localVarAccessS
      , subscriptS
      , memberAccessS
      , conditionalS
      , castingS
      , memberCallS
      , arrayCreationS
      , objectCreationS
      , scopeCallS
      , augmentedAssignS
      , scopedPropertyAccessS
      , errorSupressionS
      , hereDocS

      -- update_expression: [++, --] variable_name [ ++, -- ]
      -- anonymous_function_creation_expression
      -- unset_expression
    ]




-- argument: variadic_unpacking ... variable_name
-- array_element_initializer
-- anonymous_function_use_clause

-- ** Expression parsing: ** --
parenExprS :: ScannerB PhpExpression
parenExprS = do
  debugOpt "parenE-start" $ B.singleP "parenthesized_expression"
  debugOpt "parenE-paren" $ B.single "("
  expr <- debugOpt "parenE-exprS" expressionS
  debugOpt "parenE-end" $ B.single ")"
  pure expr


unaryOpS :: ScannerB PhpExpression
unaryOpS = do
  debugOpt "unaryOpS-start" $ B.singleP "unary_op_expression"
  op <- asum [ NotOp <$ B.single "!"
          , NegOp <$ B.single "-"
    ]
  UnaryOp op <$> debugOpt "unaryOpS-expr" expressionS


functionCallS :: ScannerB PhpExpression
functionCallS = do
  debugOpt "funcCallS-start" $ B.singleP "function_call_expression"
  nameID <- debugOpt "funcCallS-name" $ B.symbol "name"
  -- TODO: use singleP and parse content of arguments.
  debugOpt "funcCallS-args" $ B.single "arguments"
  pure $ FunctionCall nameID []


assignS :: ScannerB PhpExpression
assignS = do
  debugOpt "assignS-start" $ B.singleP "assignment_expression"
  assignee <- localVarAccessS <|> memberAccessS <|> subscriptS <|> scopedPropertyAccessS <|> listLiteralS
  debugOpt "assignS-assign" $ B.single "="
  AssignVar assignee <$> debugOpt "assignS-expr" expressionS


requireOnceS :: ScannerB PhpExpression
requireOnceS = do
  debugOpt "requireOnceS-start" $ B.singleP "require_once_expression"
  debugOpt "requireOnceS-require" $ B.single "require_once"
  expr <- debugOpt "requireOnceS-expr" expressionS
  pure $ Include RequireOnceIM expr

requireS :: ScannerB PhpExpression
requireS = do
  debugOpt "requireOnceS-start" $ B.singleP "require_expression"
  debugOpt "requireOnceS-require" $ B.single "require"
  expr <- debugOpt "requireOnceS-expr" expressionS
  pure $ Include RequireIM expr

binaryOpS :: ScannerB PhpExpression
binaryOpS = do
  debugOpt "binOpS-start" $ B.singleP "binary_expression"
  aExpr <- debugOpt "binOpS-aExpr" expressionS
  -- TODO: figure out how to attach comments to the right expression.
  comment1 <- optional $ CommentX <$> B.symbol "comment"
  op <- binOperatorS
  comment2 <- optional $ CommentX <$> B.symbol "comment"
  BinaryOp op aExpr <$> debugOpt "binOpS-bExpr" expressionS

binOperatorS :: ScannerB BinaryOps
binOperatorS = asum [
    DotOp <$ debugOpt "binOpS-dot" (B.single ".")
    , AddOp <$ debugOpt "binOpS-add" (B.single "+")
    , SubOp <$ debugOpt "binOpS-sub" (B.single "-")
    , MulOp <$ debugOpt "binOpS-mul" (B.single "*")
    , DivOp <$ debugOpt "binOpS-div" (B.single "/")
    , ModOp <$ debugOpt "binOpS-mod" (B.single "%")
    , PowOp <$ debugOpt "binOpS-pow" (B.single "^")
    , EqOp <$ debugOpt "binOpS-eq" (B.single "==")
    , EqlOp <$ debugOpt "binOpS-eql" (B.single "===")
    , NeqOp <$ debugOpt "binOpS-neq" (B.single "!=")
    , NeqlOp <$ debugOpt "binOpS-neq" (B.single "!==")
    , GtOp <$ debugOpt "binOpS-gt" (B.single ">")
    , LtOp <$ debugOpt "binOpS-lt" (B.single "<")
    , GteOp <$ debugOpt "binOpS-gte" (B.single ">=")
    , LteOp <$ debugOpt "binOpS-lte" (B.single "<=")
    , AndOp <$ debugOpt "binOpS-and" (B.single "&&")
    , OrOp <$ debugOpt "binOpS-or" (B.single "||")
    , InstanceOfOp <$ debugOpt "binOpS-instanceOf" (B.single "instanceof")
    ]


symbolAccessS :: ScannerB PhpExpression
symbolAccessS = do
  nameID <- debugOpt "symbolAccessS-name" $ B.symbol "name"
  pure $ Symbol nameID


localVarAccessS :: ScannerB PhpExpression
localVarAccessS = do
  debugOpt "localVarS-var" $ B.singleP "variable_name"
  Variable <$> varAccessorS


varAccessorS :: ScannerB VariableSpec
varAccessorS = do
  debugOpt "varAccess-dollar" $ B.single "$"
  -- TODO: implement the other variable access types.
  SimpleVS <$> debugOpt "varAccess-name" (B.symbol "name")


subscriptS :: ScannerB PhpExpression
subscriptS = do
  debugOpt "subscriptS-start" $ B.singleP "subscript_expression"
  var <- localVarAccessS <|> memberAccessS <|> functionCallS <|> subscriptS <|> scopedPropertyAccessS
  B.single "[" <|> B.single "{"
  mbIndex <- optional expressionS
  B.single "]" <|> B.single "}"
  pure $ Subscript var mbIndex


memberAccessS :: ScannerB PhpExpression
memberAccessS = do
  debugOpt "memberAcc-start" $ B.singleP "member_access_expression"
  baseVar <- localVarAccessS <|> memberCallS <|> memberAccessS <|> subscriptS
  debugOpt "memberAcc-arrow" $ B.single "->"
  fieldVar <- asum [
      nameS
      , VarExprMT <$> debugOpt "memberAcc-uid" localVarAccessS
    ]
  pure $ MemberAccess baseVar fieldVar


nameS :: ScannerB MemberAccessMode
nameS = do
  eiVal <- debugOpt "memberAcc-uid" (B.symbolPT "name")
  case eiVal of
    Left anInt -> pure $ NameMT anInt
    Right _ ->
      ParentMT <$ B.single "parent"


conditionalS :: ScannerB PhpExpression
conditionalS = do
  debugOpt "conditionalS-start" $ B.singleP "conditional_expression"
  cond <- debugOpt "conditionalS-cond" expressionS
  B.single "?"
  thenExpr <- debugOpt "conditionalS-then" expressionS
  B.single ":"
  elseExpr <- debugOpt "conditionalS-else" expressionS
  pure $ Conditional cond thenExpr elseExpr


castingS :: ScannerB PhpExpression
castingS = do
  debugOpt "castingS-start" $ B.singleP "cast_expression"
  B.single "("
  castType <- debugOpt "castingS-castType" $ B.symbol "cast_type"
  B.single ")"
  expr <- debugOpt "castingS-expr" expressionS
  pure $ Casting castType expr


memberCallS :: ScannerB PhpExpression
memberCallS = do
  debugOpt "memberCallS-start" $ B.singleP "member_call_expression"
  var <- localVarAccessS <|> functionCallS <|> memberAccessS <|> subscriptS <|> scopedPropertyAccessS <|> scopeCallS <|> memberCallS
  debugOpt "memberCallS-arrow" $ B.single "->"
  fieldVar <- asum [
      nameS
      , VarExprMT <$> debugOpt "memberAcc-uid" localVarAccessS
    ]
  -- TODO: push the arguments node and parse the children.
  debugOpt "memberCallS-args" $ B.single "arguments"
  pure $ MemberCall var fieldVar []


arrayCreationS :: ScannerB PhpExpression
arrayCreationS = do
  -- TODO: push the children and parse the array.
  debugOpt "arrayCreationS-start" $ B.single "array_creation_expression"
  pure $ ArrayLiteral []


objectCreationS :: ScannerB PhpExpression
objectCreationS = do
  debugOpt "objectCreationS-start" $ B.singleP "object_creation_expression"
  B.single "new"
  name <- asum [
    NameMT <$> debugOpt "objectCreationS-name" (B.symbol "name")
    , VarExprMT <$> localVarAccessS
    ]
  -- TODO: push the arguments and parse them.
  B.single "arguments"
  pure $ ObjectCreation name []


includeS :: ScannerB PhpExpression
includeS = do
  debugOpt "includeS-start" $ B.singleP "include_expression"
  debugOpt "includeS-include" $ B.single "include"
  expr <- debugOpt "includeS-expr" expressionS
  pure $ Include IncludeIM expr


includeOnceS :: ScannerB PhpExpression
includeOnceS = do
  debugOpt "includeOnceS-start" $ B.singleP "include_once_expression"
  debugOpt "includeOnceS-include" $ B.single "include_once"
  expr <- debugOpt "includeOnceS-expr" expressionS
  pure $ Include IncludeOnceIM expr


scopeCallS :: ScannerB PhpExpression
scopeCallS = do
  debugOpt "scopeCallS-start" $ B.singleP "scoped_call_expression"
  names <- debugOpt "scopeCallS-name" $ scopeNameS `B.sepBy` B.single "::"
  debugOpt "funcCallS-args" $ B.single "arguments"
  pure $ ScopeCall names []


augmentedAssignS :: ScannerB PhpExpression
augmentedAssignS = do
  debugOpt "augmentedAssignS-start" $ B.singleP "augmented_assignment_expression"
  var <- localVarAccessS <|> memberAccessS <|> subscriptS <|> scopedPropertyAccessS
  op <- augmentedAssignOperatorS
  expr <- debugOpt "augmentedAssignS-expr" expressionS
  pure $ AugmentedAssign var op expr

augmentedAssignOperatorS :: ScannerB BinaryOps
augmentedAssignOperatorS = asum [
    DotOp <$ debugOpt "augAssgn-dot" (B.single ".=")
    , AddOp <$ debugOpt "augAssgn-add" (B.single "+=")
    , SubOp <$ debugOpt "augAssgn-sub" (B.single "-=")
    , MulOp <$ debugOpt "augAssgn-mul" (B.single "*=")
    , DivOp <$ debugOpt "augAssgn-div" (B.single "/=")
    , ModOp <$ debugOpt "augAssgn-mod" (B.single "%=")
    ]


scopedPropertyAccessS :: ScannerB PhpExpression
scopedPropertyAccessS = do
  debugOpt "scopedProp-start" $ B.singleP "scoped_property_access_expression"
  -- TODO: find out what else to parse beside relative-scope/self.
  baseName <- scopeNameS
  B.single "::"
  expr <- debugOpt "scopedProp-expr" expressionS
  pure $ ScopedPropertyAccess baseName expr


scopeNameS :: ScannerB ScopeMode
scopeNameS =  asum [
      NamedSM <$> debugOpt "scopedProp-name" (B.symbol "name")
    , do
      B.singleP "relative_scope"
      RelativeSelfSM <$ B.single "self"
    , do
      B.singleP "relative_scope"
      RelativeStaticSM <$ B.single "static"
  ]

errorSupressionS :: ScannerB PhpExpression
errorSupressionS = do
  debugOpt "errorSupressionS-start" $ B.singleP "error_suppression_expression"
  B.single "@"
  ErrorSuppression <$> debugOpt "errorSupressionS-expr" expressionS

hereDocS :: ScannerB PhpExpression
hereDocS = do
  debugOpt "hereDocS-start" $ B.singleP "heredoc"
  B.single "<<<"
  docKind <- B.symbol "heredoc_start"
  content <- B.symbol "heredoc_body"
  endKind <- B.symbol "heredoc_end"
  pure $ HereDoc docKind content endKind

listLiteralS :: ScannerB PhpExpression
listLiteralS = do
  debugOpt "listLiteralS-start" $ B.singleP "list_literal"
  B.single "list"
  B.single "("
  -- TODO: implement list-intrisic clause.
  exprs <- debugOpt "listLiteralS-exprs" $ asum [
      localVarAccessS
      , do
        encapsedStrLiteralS
        B.single "=>"
        localVarAccessS
      ] `B.sepBy1` B.single ","
  B.single ")"
  pure $ ListLiteral exprs


{-
list-intrinsic:
   list   (   list-expression-list   )

list-expression-list:
   unkeyed-list-expression-list
   keyed-list-expression-list   ,opt

unkeyed-list-expression-list:
   list-or-variable
   ,
   unkeyed-list-expression-list   ,   list-or-variableopt

keyed-list-expression-list:
   expression   =>   list-or-variable
   keyed-list-expression-list   ,   expression   =>   list-or-variable

list-or-variable:
   list-intrinsic
   &opt   variable
-}


-- *** Literal parsing: *** --
literalS :: ScannerB PhpExpression
literalS =
  asum [
    boolLiteralS
    , intLiteralS
    , stringLiteralS
    , debugOpt "lit-encap" encapsedStrLiteralS
    , nullLiteralS
  ]


boolLiteralS :: ScannerB PhpExpression
boolLiteralS = do
  boolID <- B.symbol "boolean"
  pure . Literal $ BoolLiteral boolID


intLiteralS :: ScannerB PhpExpression
intLiteralS = do
  intID <- B.symbol "integer"
  pure . Literal $ IntLiteral intID

stringLiteralS :: ScannerB PhpExpression
stringLiteralS = do
  B.singleP "string"
  -- TODO: find out what happens to the [bB]" parsing in tree-sitter.
  binFlag <- optional $ B.single "'"
  content <- many $ asum [
        ContentEM <$> B.symbol "string_content"
        , EscapeEM <$> B.symbol "escape_sequence"
     ]
  B.single "'"
  pure . Literal $ StringLiteral (isJust binFlag) (SimpleString content)

encapsedStrLiteralS :: ScannerB PhpExpression
encapsedStrLiteralS = do
  B.singleP "encapsed_string"
  -- TODO: find out what happens to the [bB]" parsing in tree-sitter.
  binFlag <- optional $ B.single "\""
  stringParts <- many encapsedContentS
  B.single "\""
  pure . Literal $ StringLiteral (isJust binFlag) (EncapsedString stringParts)
  where
  encapsedContentS :: ScannerB EncapsedMode
  encapsedContentS = asum [
      ContentEM <$> B.symbol "string_content"
      , EscapeEM <$> B.symbol "escape_sequence"
      , VariableEM <$> asum [ localVarAccessS , memberAccessS , subscriptS, scopedPropertyAccessS ]
      , CurlyEM True <$ B.single "{"
      , CurlyEM False <$ B.single "}"
    ]

nullLiteralS :: ScannerB PhpExpression
nullLiteralS = do
  Literal NullLiteral <$ B.single "null"
