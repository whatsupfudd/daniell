module Template.PHP.NeParser where

import Control.Monad.Cont (foldM)
import Control.Applicative (asum, many, some, (<|>))
import Control.Applicative.Combinators (optional)

import Control.Lens (Identity)
import Control.Monad.Identity (Identity(..))

import Data.Data (Data (..))
import qualified Data.Vector as V

import TreeSitter.Node ( TSPoint(..) )

import Template.PHP.Print (printNode, printPhpContext)
import qualified Template.PHP.Scanner as B
import qualified Template.PHP.Class as B
import qualified Template.PHP.State as B
import qualified Template.PHP.Error as E

import Template.PHP.Debug (debug)
import Template.PHP.Types

import Template.PHP.AST

-- **** Combinatorial Monadic approach to parsing, derived from Megaparsec. **** --

newtype TError = TError String
  deriving (Show, Eq, Ord)
instance Data TError where
  toConstr _ = error "toConstr"
  gunfold _ _ = error "gunfold"
  dataTypeOf _ = error "dataTypeOf"

instance (Show te, Ord te, Data te) => E.ShowErrorComponent (E.ScanError te) where
  showErrorComponent = show


type ScannerB = B.ScannerT (E.ScanError TError) Identity

debugOpt label parser =
  let
    isOn = False
  in
  if isOn then debug label parser else parser


testScannerB :: [NodeEntry] -> Either TError PhpContext
testScannerB nodes =
  let
    mainScanner = debugOpt "main" testS <* B.pEof
    result = B.doScan mainScanner nodes
  in do
  -- putStrLn $ "@[testScannerB] endState: " <> show endState
  case result of
    Left err -> Left $ TError $ show err
    Right (logic, demands) -> Right $ PhpContext (V.fromList logic) demands


testScannerC :: [NodeEntry] -> IO ()
testScannerC nodes = do
  let
    mainScanner = debugOpt "main" $ testS <* B.pEof
  B.testScan mainScanner nodes



testS :: ScannerB [PhpAction]
testS = do
  many $ debugOpt "testS" statementS


statementS :: ScannerB PhpAction
statementS = asum [
  textS
  , phpTagS
  , commentS
  , ifS
  , exprStmtS
  , exitS
  , echoS
  , interpolationS
  , forEachS
  ]


expressionS :: ScannerB PhpExpression
expressionS = do
  asum [
      parenExprS
      , unaryOpS
      , functionCallS
      , assignS
      , requireOnceS
      , requireS
      , binaryOpS
      , symbolAccessS
      , literalS
      , localVarAccessS
      , subscriptS
      , memberAccessS
      , conditionalS
      , castingS
      , memberCallS
    ]


literalS :: ScannerB PhpExpression
literalS =
  asum [
    boolLiteralS
    , intLiteralS
    , stringLiteralS
    , debugOpt "lit-encap" encapsedStrLiteralS
  ]

-- ** Statement parsing: ** --
commentS :: ScannerB PhpAction
commentS = do
  commentID <- B.symbol "comment"
  pure $ CommentA commentID


ifS :: ScannerB PhpAction
ifS = do
  debugOpt "ifS-stmt" $ B.singleP "if_statement"
  debugOpt "ifS-if" $ B.single "if"
  expr <- debugOpt "ifS-parenE" parenExprS
  thenClause <- debugOpt "ifS-then" compoundStmtS
  mbElseClause <- optional $ debugOpt "ifS-else" elseClauseS
  pure $ Statement (IfST expr thenClause mbElseClause)


textS :: ScannerB PhpAction
textS = do
  textID <- B.symbol "text"
  pure $ Verbatim textID


phpTagS :: ScannerB PhpAction
phpTagS = do
  rez <- B.single "php_tag"
  pure $ MiscST "php_tag" (TSPoint 3 4, TSPoint 3 4)


compoundStmtS :: ScannerB PhpAction
compoundStmtS = do
  debugOpt "compoundStmtS-start" $ B.singleP "compound_statement"
  debugOpt "compoundStmtS-lbrace" $ B.single "{"
  stmts <- many $ debugOpt "compoundStmtS-stmt" statementS
  debugOpt "compoundStmtS-rbrace" $ B.single "}"
  pure $ if length stmts == 1 then head stmts else Statement $ BlockST stmts


elseClauseS :: ScannerB PhpAction
elseClauseS = do
  debugOpt "elseClauseS-start" $ B.singleP "else_clause"
  debugOpt "elseClauseS-else" $ B.single "else"
  debugOpt "elseClauseS-stmt" compoundStmtS

exprStmtS :: ScannerB PhpAction
exprStmtS = do
  debugOpt "exprStmtS-start" $ B.singleP "expression_statement"
  Statement . ExpressionST <$> expressionS <* B.single ";"

exitS :: ScannerB PhpAction
exitS = do
  debugOpt "exitS-start" $ B.singleP "exit_statement"
  debugOpt "exitS-exit" $ B.single "exit"
  mbExpr <- optional $ do
    B.single "("
    expr <- debugOpt "exitS-expr" expressionS
    B.single ")"
    pure expr
  B.single ";"
  pure . Statement $ ExitST mbExpr


echoS :: ScannerB PhpAction
echoS = do
  debugOpt "echoS-start" $ B.singleP "echo_statement"
  debugOpt "echoS-echo" $ B.single "echo"
  exprs <- many $ debugOpt "echoS-expr" expressionS
  B.single ";"
  pure . Statement $ EchoST exprs


interpolationS :: ScannerB PhpAction
interpolationS = do
  debugOpt "interpolationS-start" $ B.singleP "text_interpolation"
  content <- many $ asum [
      textS
      , NoOpAct <$ B.single "php_tag"
      , NoOpAct <$ B.single "?>"
    ]
  pure $ Interpolation content

forEachS :: ScannerB PhpAction
forEachS = do
  debugOpt "forEachS-start" $ B.singleP "foreach_statement"
  debugOpt "forEachS-foreach" $ B.single "foreach"
  B.single "("
  cond <- debugOpt "forEachS-iter" expressionS
  debugOpt "forEachS-as" $ B.single "as"
  asVar <- debugOpt "forEachS-asVar" localVarAccessS
  mbWithKey <- optional $ do
    debugOpt "forEachS-withKey" $ B.single "=>"
    debugOpt "forEachS-keyVar" localVarAccessS
  B.single ")"
  iterStmt <- forEachColonBlock "foreach"
            <|> debugOpt "forEachS-iterStmt" statementS
  B.single ";"
  case asVar of
    Variable varSpec ->
      case mbWithKey of
        Just (Variable keySpec) -> pure $ Statement $ ForEachST cond varSpec (Just keySpec) iterStmt
        Nothing -> pure $ Statement $ ForEachST cond varSpec Nothing iterStmt
        Just _ -> B.failure "foreach statement must use a simple variable for key." Nothing mempty
    _ -> B.failure "foreach statement must use a simple variable for value." Nothing mempty
  where
  forEachColonBlock :: String -> ScannerB PhpAction
  forEachColonBlock blockName = do
    debugOpt "forEachColonBlock-start" $ B.singleP "colon_block"
    debugOpt "forEachColonBlock-colon" $ B.single ":"
    stmts <- many $ debugOpt "forEachColonBlock-stmt" statementS
    debugOpt "forEachColonBlock-rbrace" $ B.single ("end" <> blockName)
    pure $ if length stmts == 1 then head stmts else Statement $ BlockST stmts


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
  debugOpt "assignS-var" $ B.singleP "variable_name"
  debugOpt "assignS-dollar" $ B.single "$"
  nameID <- debugOpt "assignS-name" $ B.symbol "name"
  debugOpt "assignS-assign" $ B.single "="
  expr <- debugOpt "assignS-expr" expressionS
  pure $ AssignLocal nameID expr

requireOnceS :: ScannerB PhpExpression
requireOnceS = do
  debugOpt "requireOnceS-start" $ B.singleP "require_once_expression"
  debugOpt "requireOnceS-require" $ B.single "require_once"
  expr <- debugOpt "requireOnceS-expr" expressionS
  pure $ Require True expr

requireS :: ScannerB PhpExpression
requireS = do
  debugOpt "requireOnceS-start" $ B.singleP "require_expression"
  debugOpt "requireOnceS-require" $ B.single "require"
  expr <- debugOpt "requireOnceS-expr" expressionS
  pure $ Require False expr

binaryOpS :: ScannerB PhpExpression
binaryOpS = do
  debugOpt "binOpS-start" $ B.singleP "binary_expression"
  aExpr <- debugOpt "binOpS-aExpr" expressionS
  op <- binOperatorS
  BinaryOp op aExpr <$> debugOpt "binOpS-bExpr"expressionS

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
    , NeqOp <$ debugOpt "binOpS-neq" (B.single "!==")
    , GtOp <$ debugOpt "binOpS-gt" (B.single ">")
    , LtOp <$ debugOpt "binOpS-lt" (B.single "<")
    , GteOp <$ debugOpt "binOpS-gte" (B.single ">=")
    , LteOp <$ debugOpt "binOpS-lte" (B.single "<=")
    , AndOp <$ debugOpt "binOpS-and" (B.single "&&")
    , OrOp <$ debugOpt "binOpS-or" (B.single "||")
    ]


symbolAccessS :: ScannerB PhpExpression
symbolAccessS = do
  nameID <- debugOpt "symbolAccessS-name" $ B.symbol "name"
  pure $ Symbol nameID


localVarAccessS :: ScannerB PhpExpression
localVarAccessS = do
  debugOpt "assignS-var" $ B.singleP "variable_name"
  Variable <$> varAccessorS


varAccessorS :: ScannerB VariableSpec
varAccessorS = do
  debugOpt "assignS-dollar" $ B.single "$"
  -- TODO: implement the other variable access types.
  SimpleVS <$> debugOpt "assignS-name" (B.symbol "name")


subscriptS :: ScannerB PhpExpression
subscriptS = do
  debugOpt "subscriptS-start" $ B.singleP "subscript_expression"
  var <- localVarAccessS
  B.single "[" <|> B.single "{"
  index <- expressionS
  B.single "]" <|> B.single "}"
  case var of
    Variable varSpec -> pure $ Subscript varSpec index
    _ -> B.failure "unknown expression on a localVarAccessS parser." Nothing mempty


memberAccessS :: ScannerB PhpExpression
memberAccessS = do
  debugOpt "memberAccessS-start" $ B.singleP "member_access_expression"
  var <- localVarAccessS
  debugOpt "memberAccessS-arrow" $ B.single "->"
  uid <- debugOpt "memberAccessS-uid" $ B.symbol "name"
  case var of
    Variable varSpec -> pure $ MemberAccess varSpec uid
    _ -> B.failure "unknown expression on a localVarAccessS parser." Nothing mempty


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
  var <- localVarAccessS
  debugOpt "memberCallS-arrow" $ B.single "->"
  uid <- debugOpt "memberCallS-uid" $ B.symbol "name"
  debugOpt "memberCallS-args" $ B.single "arguments"
  case var of
    Variable varSpec -> pure $ MemberAccess varSpec uid
    _ -> B.failure "unknown expression on a localVarAccessS parser." Nothing mempty

-- *** Literal parsing: *** --
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
  B.single "'"
  mbStringID <- optional $ B.symbol "string_content"
  B.single "'"
  pure . Literal $ StringLiteral (SimpleString mbStringID)

encapsedStrLiteralS :: ScannerB PhpExpression
encapsedStrLiteralS = do
  B.singleP "encapsed_string"
  B.single "\""
  mbStringID <- optional encapsedContentS
  B.single "\""
  pure . Literal $ StringLiteral (EncapsedString mbStringID)
  where
  encapsedContentS :: ScannerB (Int, Int)
  encapsedContentS = do
    contentID <-B.symbol "string_content"
    sequenceID <-B.symbol "escape_sequence"
    pure (contentID, sequenceID)

