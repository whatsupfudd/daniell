module Template.PHP.Parser.Statements where

import Control.Applicative (asum, many, some, (<|>))
import Control.Applicative.Combinators (optional)

import Data.Functor (($>))
import Data.Maybe (maybeToList)

import TreeSitter.Node ( TSPoint(..) )

import Template.PHP.Parser.Support (debugOpt)
import qualified Template.PHP.Scanner as B
import qualified Template.PHP.Class as B
import qualified Template.PHP.State as B
import Template.PHP.AST
import Template.PHP.Parser.Types
import Template.PHP.Parser.Expressions

-- ** Statement parsing: ** --

statementS :: ScannerB PhpAction
statementS = asum [
  textInterpolationS
  , commentS
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
  , textS
  , phpTagS
  -- The dangling statements are cut by interpolation-text, and they aren't put as children of the if-statement but as next in list.
  , danglingElseS
  , danglingElseIfS
  ]


commentS :: ScannerB PhpAction
commentS = do
  CommentA <$> B.symbol "comment"


textS :: ScannerB PhpAction
textS = do
  textID <- B.symbol "text"
  pure $ Verbatim textID


phpTagS :: ScannerB PhpAction
phpTagS = do
  rez <- B.single "php_tag"
  pure $ MiscST "php_tag" (TSPoint 3 4, TSPoint 3 4)


textInterpolationS :: ScannerB PhpAction
textInterpolationS = do
  debugOpt "textInter-start" $ B.singleP "text_interpolation"
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
  asum [
        do
          (thenClause, mbElseClause) <- colonBlockS "if" True
          pure $ Statement (IfST expr thenClause mbElseClause)
      , do
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
    ]


elseIfClauseS :: ScannerB PhpAction
elseIfClauseS = do
  debugOpt "elIfS-start" $ B.singleP "else_if_clause"
  debugOpt "elIfS-elseif" $ B.single "elseif"
  expr <- debugOpt "elIfS-parenE" parenExprS
  asum [
    do
      (thenClause, mbElseClause) <- colonBlockS "if" True
      pure $ Statement (IfST expr thenClause mbElseClause)
    , do
      thenClause <- debugOpt "elIfS-then" compoundStmtS
      pure $ Statement (IfST expr thenClause Nothing)
    ]


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
        (refVar, asVar) <- mbRefVarAccessS
        debugOpt "pair-arrow" $ B.single "=>"
        (refKey, withKey) <- debugOpt "pair-localVar" mbRefVarAccessS 
        pure (refVar, asVar, Just (refKey, withKey))
    , do
      (isRef, asVar) <- mbRefVarAccessS
      pure (isRef, asVar, Nothing)
    ]
  debugOpt "forEachS-rparen" $ B.single ")"
  (iterStmt, _) <- colonBlockS "foreach" False
            <|> (,) <$> debugOpt "forEachS-iterStmt" statementS <*> pure Nothing
  case asVar of
    Variable varSpec ->
      case mbWithKey of
        Just (keyFlag, Variable keySpec) -> pure $ Statement $ ForEachST cond (isRef, varSpec) (Just (keyFlag, keySpec)) iterStmt
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


colonBlockS :: String -> Bool -> ScannerB (PhpAction, Maybe PhpAction)
colonBlockS blockName hasElse = do
  debugOpt "colonBlock-start" $ B.singleP "colon_block"
  debugOpt "colonBlock-colon" $ B.single ":"
  stmts <- many $ debugOpt "colonBlock-then-stmt" statementS
  thenVerbatim <- optional textInterpolationS
  mbElseBlock <- if hasElse then 
    optional $ do
      debugOpt "colonBlock-else-clause" $ B.singleP "else_clause"
      B.single "else"
      debugOpt "colonBlock-start" $ B.singleP "colon_block"
      debugOpt "colonBlock-colon" $ B.single ":"
      many $ debugOpt "colonBlock-else-stmt" statementS
  else
    pure Nothing
  elseVerbatim <- optional $ debugOpt "colonBlock-else-verbatim" textInterpolationS
  -- TODO: figure out if the end-token should be considered outside of this block
  -- or not (it's not a child of colon-block).
  debugOpt "colonBlock-end" $ B.single ("end" <> blockName)
  B.single ";"
  let
    mbElseStmt = case mbElseBlock of
      Nothing -> Nothing
      Just elseBlock -> case elseBlock of
        [] -> Nothing
        [ aStmt ] -> Just aStmt
        stmts -> Just (Statement $ BlockST elseBlock)
    thenStmt = case stmts of
      [] -> Statement NoOpST
      [ aStmt ] -> aStmt
      stmts -> Statement $ BlockST stmts
  pure $ (thenStmt, mbElseStmt)


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
  exprs <- asum [
      (:[]) <$> debugOpt "echoS-expr" expressionS
    , do
      B.single "sequence_expression"
      debugOpt "echoS-exprs-seq" $ expressionS `B.sepBy` B.single ","
    ]
  B.single ";"
  pure . Statement $ EchoST exprs


exitS :: ScannerB PhpAction
exitS = do
  debugOpt "exitS-start" $ B.singleP "exit_statement"
  debugOpt "exitS-exit" $ B.single "exit"
  mbExpr <- optional $ do
    B.single "("
    expr <- optional $ debugOpt "exitS-expr" expressionS
    B.single ")"
    pure expr
  B.single ";"
  case mbExpr of
    Nothing -> pure $ Statement $ ExitST Nothing
    Just subExpr -> case subExpr of
      Nothing -> pure $ Statement $ ExitST Nothing
      Just expr -> pure $ Statement $ ExitST (Just expr)


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
  debugOpt "fctDefS-start" $ B.singleP "function_definition"
  B.single "function"
  name <- debugOpt "fctDefS-name" qualifiedNameS
  -- TODO: push the formal parameters and handle them.
  B.single "formal_parameters"
  Statement . FunctionDefST name <$> debugOpt "fctDefS-body" compoundStmtS


classDefS :: ScannerB PhpAction
classDefS = do
  debugOpt "classDefS-start" $ B.singleP "class_declaration"
  mbAttributes <- optional attributeListS
  modifiers <- many memberModifierS
  B.single "class"
  nameID <- debugOpt "classDefS-name" $ B.symbol "name"
  mbBaseName <- optional $ do
    B.singleP "base_clause"
    B.single "extends"
    qualifiedNameS
  interfDefs <- optional $ do
    B.singleP "class_interface_clause"
    B.single "implements"
    debugOpt "classDefS-interf" $ B.symbol "name" `B.sepBy` B.single ","
  B.singleP "declaration_list"
  B.single "{"
  classMembers <- many $ debugOpt "classDefS-member" classMemberDeclS
  B.single "}"
  pure . Statement $ ClassDefST mbAttributes modifiers nameID mbBaseName interfDefs classMembers


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
  name <- debugOpt "methodCDeclS-name" nameS
  -- TODO: push the formal parameters and handle them.
  B.single "formal_parameters"
  -- B.single "("
  -- params <- debug "methodCDeclS-param" formalParameterS
  -- B.single ")"
  -- TODO: handle a "return_type" ";" instead of a compoundStmt.
  methodImpl <-
    if AbstractCM `elem` modifiers then
      AbstractMI <$ B.single ";"
    else
      MethodImplementationMI <$> compoundStmtS
      <|> ReturnTypeMI <$> functionReturnTypeS
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

-- Dangling statements are cut by interpolation-text from their theoretical parent, and thus aren't put as children of the if-statement but as next in list.
danglingElseS :: ScannerB PhpAction
danglingElseS =
  Statement . DanglingST . StatementDC <$> elseClauseS

danglingElseIfS :: ScannerB PhpAction
danglingElseIfS = 
  Statement . DanglingST . StatementDC <$> elseIfClauseS

danglingEndIfS :: ScannerB PhpAction
danglingEndIfS =
  Statement . DanglingST <$> asum [
      EndDeclareDC <$ B.single "enddeclare"
    , EndForDC <$ B.single "endfor"
    , EndForEachDC <$ B.single "endforeach"
    , EndIfDC <$ B.single "endif"
    , EndSwitchDC <$ B.single "endswitch"
    , EndWhileDC <$ B.single "endwhile"
  ]

