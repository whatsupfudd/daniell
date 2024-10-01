{-# LANGUAGE TupleSections #-}
module Template.PHP.NeParser where

import Control.Monad.Cont (foldM)
import Control.Applicative (asum, many, some, (<|>))
import Control.Applicative.Combinators (optional)

import Control.Lens (Identity)
import Control.Monad.Identity (Identity(..))

import Data.Data (Data (..))
import Data.Maybe (isJust, maybeToList)
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
import FileSystem.Types (DirNode(dirPath))
import Data.Functor (($>))

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
    Left err -> Left $ TError $ E.showScanErrorBundle err
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
  , interpolationS
  , compoundStmtS
  , namedLabelS
  , exprStmtS
  , ifS
  , switchS
  , whileS
  , doS
  , forS
  , forEachS
  , gotoS
  , continueS
  , breakS
  , returnS
  , tryS
  , declareS
  , echoS
  , exitS
  , unsetS
  , constDeclS
  , functionDefS
  , debugOpt "class-stmt" classDefS
  , interfaceDefS
  , traitDeclS
  , enumDeclS
  , namespaceDefS
  , namespaceUseS
  , globalDeclS
  , functionStaticS
  ]


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
    ]


literalS :: ScannerB PhpExpression
literalS =
  asum [
    boolLiteralS
    , intLiteralS
    , stringLiteralS
    , debugOpt "lit-encap" encapsedStrLiteralS
    , nullLiteralS
  ]

-- ** Statement parsing: ** --
commentS :: ScannerB PhpAction
commentS = do
  commentID <- B.symbol "comment"
  pure $ CommentA commentID


textS :: ScannerB PhpAction
textS = do
  textID <- B.symbol "text"
  pure $ Verbatim textID


phpTagS :: ScannerB PhpAction
phpTagS = do
  rez <- B.single "php_tag"
  pure $ MiscST "php_tag" (TSPoint 3 4, TSPoint 3 4)


interpolationS :: ScannerB PhpAction
interpolationS = do
  debugOpt "interpolationS-start" $ B.singleP "text_interpolation"
  content <- many $ asum [
      textS
      , NoOpAct <$ B.single "php_tag"
      , NoOpAct <$ B.single "?>"
    ]
  pure $ Interpolation content


compoundStmtS :: ScannerB PhpAction
compoundStmtS = do
  debugOpt "compoundStmtS-start" $ B.singleP "compound_statement"
  debugOpt "compoundStmtS-lbrace" $ B.single "{"
  stmts <- many $ debugOpt "compoundStmtS-stmt" statementS
  debugOpt "compoundStmtS-rbrace" $ B.single "}"
  pure $ if length stmts == 1 then head stmts else Statement $ BlockST stmts


namedLabelS :: ScannerB PhpAction
namedLabelS = do
  debugOpt "namedLabelS-start" $ B.single "named_label_statement"
  pure $ Statement NamedLabelST


exprStmtS :: ScannerB PhpAction
exprStmtS = do
  debugOpt "exprStmtS-start" $ B.singleP "expression_statement"
  Statement . ExpressionST <$> expressionS <* B.single ";"


ifS :: ScannerB PhpAction
ifS = do
  debugOpt "ifS-stmt" $ B.singleP "if_statement"
  debugOpt "ifS-if" $ B.single "if"
  expr <- debugOpt "ifS-parenE" parenExprS
  thenClause <- debugOpt "ifS-then" compoundStmtS
  elseIfClauses <- debugOpt "ifS-elseIf" $ many elseIfClauseS
  mbElseClause <- optional $ debugOpt "ifS-else" elseClauseS
  let
    restClauses = case elseIfClauses of
      [] -> mbElseClause
      _ ->
        let
          revClauses = reverse elseIfClauses
          (exprGlob, thenGlob, elseGlob) =
            case head revClauses of
              Statement (IfST expr thenClause elseClause) -> (expr, thenClause, mbElseClause)
              _ -> (Literal (BoolLiteral 0), Statement NoOpST, Nothing)
        in
        Just $ foldl (\accum prevIf ->
            case prevIf of
              Statement (IfST expr thenClause elseClause) -> Statement $ IfST expr thenClause (Just accum)
              _ -> Statement NoOpST
          )
          (Statement $ IfST exprGlob thenGlob elseGlob) (tail elseIfClauses)
  pure $ Statement (IfST expr thenClause restClauses)


elseIfClauseS :: ScannerB PhpAction
elseIfClauseS = do
  debugOpt "elIfS-start" $ B.singleP "else_if_clause"
  debugOpt "elIfS-elseif" $ B.single "elseif"
  expr <- debugOpt "elIfS-parenE" parenExprS
  thenClause <- debugOpt "elIfS-then" compoundStmtS
  pure $ Statement (IfST expr thenClause Nothing)

elseClauseS :: ScannerB PhpAction
elseClauseS = do
  debugOpt "elseS-start" $ B.singleP "else_clause"
  debugOpt "elseS-else" $ B.single "else"
  debugOpt "elseS-stmt" compoundStmtS


switchS :: ScannerB PhpAction
switchS = do
  debugOpt "switchS-start" $ B.single "switch_statement"
  pure $ Statement SwitchST

whileS :: ScannerB PhpAction
whileS = do
  debugOpt "whileS-start" $ B.single "while_statement"
  pure $ Statement WhileST

doS :: ScannerB PhpAction
doS = do
  debugOpt "doS-start" $ B.single "do_statement"
  pure $ Statement DoST

forS :: ScannerB PhpAction
forS = do
  debugOpt "forS-start" $ B.single "for_statement"
  pure $ Statement ForST


forEachS :: ScannerB PhpAction
forEachS = do
  debugOpt "forEachS-start" $ B.singleP "foreach_statement"
  debugOpt "forEachS-foreach" $ B.single "foreach"
  debugOpt "forEachS-lparen" $ B.single "("
  cond <- debugOpt "forEachS-cond" expressionS
  debugOpt "forEachS-as" $ B.single "as"
  (isRef, asVar, mbWithKey) <- asum [
      do
        debugOpt "forEach-pair" $ B.singleP "pair"
        (isRef, asVar) <- mbRefVarAccessS
        debugOpt "pair-arrow" $ B.single "=>"
        withKey <- debugOpt "pair-localVar" localVarAccessS
        pure (isRef, asVar, Just withKey)
    , do
      (isRef, asVar) <- mbRefVarAccessS
      pure (isRef, asVar, Nothing)
    ]
  debugOpt "forEachS-rparen" $ B.single ")"
  iterStmt <- colonBlockS "foreach"
            <|> debugOpt "forEachS-iterStmt" statementS
  case asVar of
    Variable varSpec ->
      case mbWithKey of
        Just (Variable keySpec) -> pure $ Statement $ ForEachST cond (isRef, varSpec) (Just keySpec) iterStmt
        Nothing -> pure $ Statement $ ForEachST cond (isRef, varSpec) Nothing iterStmt
        Just _ -> B.failure "foreach statement must use a simple variable for key." Nothing mempty
    _ -> B.failure "foreach statement must use a simple variable for value." Nothing mempty


mbRefVarAccessS :: ScannerB (Bool, PhpExpression)
mbRefVarAccessS = asum [
      do
      debugOpt "pair-byRef" $ B.singleP "by_ref"
      debugOpt "pair-amps" $ B.single "&"
      asVar <- debugOpt "pair-localVar" localVarAccessS
      pure (True, asVar)
    , do
      asVar <- debugOpt "pair-localVar" localVarAccessS
      pure (False, asVar)
  ]


colonBlockS :: String -> ScannerB PhpAction
colonBlockS blockName = do
  debugOpt "forEachColonBlock-start" $ B.singleP "colon_block"
  debugOpt "forEachColonBlock-colon" $ B.single ":"
  stmts <- many $ debugOpt "forEachColonBlock-stmt" statementS
  debugOpt "forEachColonBlock-rbrace" $ B.single ("end" <> blockName)
  B.single ";"
  pure $ if length stmts == 1 then head stmts else Statement $ BlockST stmts


gotoS :: ScannerB PhpAction
gotoS = do
  debugOpt "gotoS-start" $ B.single "goto_statement"
  pure $ Statement GotoST

continueS :: ScannerB PhpAction
continueS = do
  debugOpt "continueS-start" $ B.single "continue_statement"
  pure $ Statement ContinueST

breakS :: ScannerB PhpAction
breakS = do
  debugOpt "breakS-start" $ B.single "break_statement"
  pure $ Statement BreakST

returnS :: ScannerB PhpAction
returnS = do
  debugOpt "returnS-start" $ B.singleP "return_statement"
  B.single "return"
  mbExpr <- optional $ debugOpt "returnS-expr" expressionS
  B.single ";"
  pure $ Statement $ ReturnST mbExpr

tryS :: ScannerB PhpAction
tryS = do
  debugOpt "tryS-start" $ B.single "try_statement"
  pure $ Statement TryST

declareS :: ScannerB PhpAction
declareS = do
  debugOpt "declareS-start" $ B.single "declare_statement"
  pure $ Statement DeclareST


echoS :: ScannerB PhpAction
echoS = do
  debugOpt "echoS-start" $ B.singleP "echo_statement"
  debugOpt "echoS-echo" $ B.single "echo"
  exprs <- many $ debugOpt "echoS-expr" expressionS
  B.single ";"
  pure . Statement $ EchoST exprs


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


unsetS :: ScannerB PhpAction
unsetS = do
  debugOpt "unsetS-start" $ B.single "unset_statement"
  pure $ Statement UnsetST

constDeclS :: ScannerB PhpAction
constDeclS = do
  debugOpt "constDeclS-start" $ B.single "const_declaration"
  pure $ Statement ConstDeclST

functionDefS :: ScannerB PhpAction
functionDefS = do
  debugOpt "functionDefS-start" $ B.single "function_definition"
  pure $ Statement FunctionDefST

classDefS :: ScannerB PhpAction
classDefS = do
  debugOpt "classDefS-start" $ B.singleP "class_declaration"
  mbAttributes <- optional attributeListS
  modifiers <- many memberModifierS
  B.single "class"
  nameID <- debugOpt "classDefS-name" $ B.symbol "name"
  mbBaseID <- optional $ do
    B.singleP "base_clause"
    B.single "extends"
    debugOpt "classDefS-base" $ B.symbol "name"
  interfDefs <- optional $ do
    B.singleP "class_interface_clause"
    B.single "implements"
    debugOpt "classDefS-interf" $ B.symbol "name" `B.sepBy` B.single ","
  B.singleP "declaration_list"
  B.single "{"
  classMembers <- many $ debugOpt "classDefS-member" classMemberDeclS
  B.single "}"
  pure . Statement $ ClassDefST mbAttributes modifiers nameID mbBaseID interfDefs classMembers


classMemberDeclS :: ScannerB ClassMemberDecl
classMemberDeclS = asum [
    CommentCDecl <$> B.symbol "comment"
    , constantCDeclS
    , debugOpt "clmbr-property" propertyCDeclS
    , debugOpt "clmbr-method" methodCDeclS
    , constructorCDeclS
    , destructorCDeclS
    , traitUseCDeclS
  ]

constantCDeclS :: ScannerB ClassMemberDecl
constantCDeclS = do
  debugOpt "const-start" $ B.singleP "const_declaration"
  modifiers <- many $ debugOpt "const-modifiers" memberModifierS
  B.single "const"
  elements <- constantMemberS `B.sepBy1` B.single ","
  B.single ";"
  pure $ ConstantCDecl modifiers elements


constantMemberS :: ScannerB (Int, PhpExpression)
constantMemberS = do
  B.singleP "const_element"
  nameID <- debugOpt "constantMemberS-name" $ B.symbol "name"
  B.single "="
  expr <- debugOpt "constantMemberS-expr" expressionS
  pure (nameID, expr)


memberModifierS :: ScannerB MemberModifier
memberModifierS = asum [
  visibilityModifierS
  , referenceModifierS
  , classModifierS
  ]


visibilityModifierS :: ScannerB MemberModifier
visibilityModifierS = do
  B.singleP "visibility_modifier"
  asum [
    PublicCM <$ B.single "public"
    , ProtectedCM <$ B.single "protected"
    , PrivateCM <$ B.single "private"
    ]


classModifierS :: ScannerB MemberModifier
classModifierS = debugOpt "cl-modifier" $ asum [
      AbstractCM <$ do
          B.singleP "abstract_modifier"
          B.single "abstract"
      , FinalCM <$ do
          B.singleP "final_modifier"
          B.single "final"
      , ReadonlyCM <$ do
          B.singleP "readonly_modifier"
          B.single "readonly"
      , StaticCM <$ do
          B.singleP "static_modifier"
          B.single "static"
    ]


referenceModifierS :: ScannerB MemberModifier
referenceModifierS = do
  B.singleP "reference_modifier"
  B.single "&"
  pure ReferenceCM


propertyCDeclS :: ScannerB ClassMemberDecl
propertyCDeclS = do
  debugOpt "propertyCDeclS-start" $ B.singleP "property_declaration"
  mbAttributes <- optional attributeListS
  modifiers <- many $
    NoOpCM <$ B.single "var"
    <|>
    debugOpt "propertyCDeclS-modifiers" memberModifierS
  debugOpt "propertyCDeclS-dollar" $ B.singleP "property_element"
  varEntry <- localVarAccessS
  mbInitializer <- optional $ do
    B.singleP "property_initializer"
    B.single "="
    debugOpt "propertyCDeclS-initializer" expressionS
  B.single ";"
  case varEntry of
    Variable varSpec -> pure $ PropertyCDecl mbAttributes modifiers varSpec mbInitializer
    _ -> B.failure "unknown expression on a localVarAccessS parser." Nothing mempty


methodCDeclS :: ScannerB ClassMemberDecl
methodCDeclS = do
  debugOpt "methodCDeclS-start" $ B.singleP "method_declaration"
  mbAttributes <- optional attributeListS
  modifiers <- many $
    NoOpCM <$ B.single "var"
    <|>
    debugOpt "methodCDeclS-modifiers" memberModifierS
  B.single "function"
  mbRefModifier <- optional referenceModifierS
  name <- debugOpt "propertyCDeclS-name" $ nameS
  B.single "formal_parameters"
  -- ()
  -- TODO: handle a "return_type" ";" instead of a compoundStmt.
  methodImpl <-
    MethodImplementation <$> compoundStmtS
    <|> ReturnType <$> functionReturnTypeS
  pure $ MethodCDecl mbAttributes (modifiers <> maybeToList mbRefModifier) name [] methodImpl

functionReturnTypeS :: ScannerB TypeDecl
functionReturnTypeS = do
  debugOpt "functionReturnTypeS-start" $ B.singleP "return_type"
  B.single ":"
  debugOpt "functionReturnTypeS-typeDecl" typeDeclS


-- WIP:
typeDeclS :: ScannerB TypeDecl
typeDeclS = do
  debugOpt "typeDeclS-start" $ B.singleP "type_declaration"
  mbSafe <- optional $ B.single "?"
  asum [
      B.single "void" $> VoidTD
      , B.single "array" $> ArrayTD
      , B.single "callable" $> CallableTD
      , B.single "iterable" $> IterableTD
      , B.single "bool" $> BoolTD
      , B.single "float" $> FloatTD
      , B.single "int" $> IntTD
      , B.single "string" $> StringTD
    ]


constructorCDeclS :: ScannerB ClassMemberDecl
constructorCDeclS = do
  debugOpt "constructorCDeclS-start" $ B.single "constructor_declaration"
  pure $ ConstructorCDecl

destructorCDeclS :: ScannerB ClassMemberDecl
destructorCDeclS = do
  debugOpt "destructorCDeclS-start" $ B.single "destructor_declaration"
  pure $ DestructorCDecl

traitUseCDeclS :: ScannerB ClassMemberDecl
traitUseCDeclS = do
  debugOpt "traitUseCDeclS-start" $ B.singleP "use_declaration"
  debugOpt "traitUseCDeclS-use" $ B.single "use"
  nameIDs <- debugOpt "traitUseCDeclS-name" $ B.symbol "name" `B.sepBy1` B.single ","
  mbUseList <- asum [
      do
        -- TODO: push the children and parse the use list.
        B.single "use_list"
        pure . Just $ UseList []
      , Nothing <$ B.single ";"
    ]
  pure $ TraitUseCDecl nameIDs mbUseList

-- Attribute list: #[ AttrGroup, AttrGroup, ... ] ; AttrGroup = [ Attribute, ... ] ; Attribute = <Name>
attributeListS :: ScannerB AttributeList
attributeListS = do
  debugOpt "attributeListS-start" $ B.singleP "attribute_list"
  groups <- some $ do
    debugOpt "attributeListS-group" $ B.singleP "attribute_group"
    B.single "#["
    attribs <- many $ do
      debugOpt "attributeListS-attrib" $ B.singleP "attribute"
      LabelAT <$> debugOpt "attributeListS-name" (B.symbol "name")
    B.single "]"
    pure $ AttributeGroup attribs
  pure $ AttributeList groups


interfaceDefS :: ScannerB PhpAction
interfaceDefS = do
  debugOpt "interfaceDefS-start" $ B.single "interface_declaration"
  pure $ Statement InterfaceDefST

traitDeclS :: ScannerB PhpAction
traitDeclS = do
  debugOpt "traitDeclS-start" $ B.single "trait_declaration"
  pure $ Statement TraitDefST

enumDeclS :: ScannerB PhpAction
enumDeclS = do
  debugOpt "enumDeclS-start" $ B.single "enum_declaration"
  pure $ Statement EnumDefST

namespaceDefS :: ScannerB PhpAction
namespaceDefS = do
  debugOpt "namespaceDefS-start" $ B.single "namespace_definition"
  pure $ Statement NamespaceDefST

namespaceUseS :: ScannerB PhpAction
namespaceUseS = do
  debugOpt "namespaceUseS-start" $ B.single "namespace_use_declaration"
  pure $ Statement NamespaceUseST

globalDeclS :: ScannerB PhpAction
globalDeclS = do
  debugOpt "globalDeclS-start" $ B.single "global_declaration"
  pure $ Statement GlobalDeclST

functionStaticS :: ScannerB PhpAction
functionStaticS = do
  debugOpt "functionStaticS-start" $ B.single "function_static_declaration"
  pure $ Statement FunctionStaticST



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
  assignee <- localVarAccessS <|> memberAccessS <|> subscriptS <|> scopedPropertyAccessS
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
  comment1 <- optional commentS
  op <- binOperatorS
  comment2 <- optional commentS
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
  baseVar <- localVarAccessS <|> memberCallS
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
  var <- localVarAccessS <|> functionCallS <|> memberAccessS <|> subscriptS <|> scopedPropertyAccessS <|> memberCallS
  debugOpt "memberCallS-arrow" $ B.single "->"
  fieldVar <- asum [
      nameS
      , VarExprMT <$> debugOpt "memberAcc-uid" localVarAccessS
    ]
  debugOpt "memberCallS-args" $ B.single "arguments"
  pure $ MemberAccess var fieldVar


arrayCreationS :: ScannerB PhpExpression
arrayCreationS = do
  -- TODO: push the children and parse the array.
  debugOpt "arrayCreationS-start" $ B.single "array_creation_expression"
  pure $ ArrayLiteral []


objectCreationS :: ScannerB PhpExpression
objectCreationS = do
  debugOpt "objectCreationS-start" $ B.singleP "object_creation_expression"
  B.single "new"
  nameID <- debugOpt "objectCreationS-name" $ B.symbol "name"
  -- TODO: push the arguments and parse them.
  B.single "arguments"
  pure $ ObjectCreation nameID []


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
  -- TODO: find out what happens to the [bB]" parsing in tree-sitter.
  binFlag <- optional $ B.single "'"
  mbStringID <- optional $ B.symbol "string_content"
  B.single "'"
  pure . Literal $ StringLiteral (isJust binFlag) (SimpleString mbStringID)

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
