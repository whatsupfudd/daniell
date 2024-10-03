module Template.PHP.Print where


import qualified Data.ByteString as Bs
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Data.Maybe (maybe)
import qualified Data.Vector as V
import TreeSitter.Node ( Node(..), TSPoint(TSPoint, pointRow, pointColumn) )

import Template.PHP.Types
import Template.PHP.AST
import Template.PHP.Parser.Types (TError)


printNode :: Int -> NodeEntry -> IO ()
printNode level node = do
  let
    prefix = if level == 0 then "" else replicate ((level - 1) * 2) ' ' <> "| "
  putStrLn $ prefix <> name node <> " " <> showNodePos node
  if null node.children then
    pure ()
  else do
    mapM_ (printNode (succ level)) node.children


showNode :: Int -> NodeEntry -> String
showNode level node =
  let
    prefix = if level == 0 then "" else replicate ((level - 1) * 2) ' ' <> "| "
    mainPart = prefix <> name node <> " " <> showRange node.start node.end
  in
  if null node.children then
    mainPart
  else
    mainPart <> " > " <> concatMap (showNode (succ level)) node.children


showNodePos :: NodeEntry -> String
showNodePos aNode =
  let
    startS = "(" ++ show aNode.start.pointRow ++ "," ++ show aNode.start.pointColumn ++ ")"
    endS = "(" ++ show aNode.end.pointRow ++ "," ++ show aNode.end.pointColumn ++ ")"
  in
  startS <> "-" <> endS


showNodeCap :: Int -> Int -> [NodeEntry] -> (String, Int)
showNodeCap level capCount nodes =
  case nodes of
    [] -> ("", capCount)
    hNode : rest ->
      if capCount > 3 then
        ("...", succ capCount)
      else
        let
            prefix = if level == 0 then "" else "\n" <> replicate ((level - 1) * 2) ' ' <> "| "
            mainPart = prefix <> show hNode
          in
          if null hNode.children then
            (mainPart, succ capCount)
          else
            let
              (nextPart, newCap) = showNodeCap level (succ capCount) hNode.children
              (restPart2, newCap2) = if newCap > 3 then ("...", newCap) else showNodeCap level (succ newCap) rest
            in
            (mainPart <> " > " <> nextPart <> " | " <> restPart2, newCap2)


printPhpContext :: Bs.ByteString -> PhpContext -> IO ()
printPhpContext content ctxt =
  let
    cLines = V.fromList $ Bs.split 10 content
    demandLines = V.map (fetchContent cLines) $ V.zip ctxt.contentDemands (V.fromList [0..])
  in do
  putStrLn $ "@[printPhpContext] logic: " <> showLogic 0 (V.toList ctxt.logic)
  putStrLn $ "@[printPhpContext] contentDemands: "
  V.mapM_ putStrLn demandLines
  where
  fetchContent :: V.Vector Bs.ByteString -> (SegmentPos, Int) -> String
  fetchContent cLines ((start, end), lineNum) =
    let
      startLine = fromIntegral start.pointRow
      startCol = fromIntegral start.pointColumn
      endLine = fromIntegral end.pointRow
      endCol = fromIntegral end.pointColumn
      mainText
        | startLine == endLine = Bs.take (endCol - startCol) $ Bs.drop startCol (cLines V.! startLine)
        | endCol == 0 = let
                          prefix = Bs.drop startCol (cLines V.! startLine)
                          middle = if endLine == succ startLine then
                              ""
                            else
                              V.foldl (\acc x -> acc <> "\n" <> x) "" (V.slice startLine (endLine - startLine - 1) cLines)
                        in
                        prefix <> middle
        | otherwise = let
                        prefix = Bs.drop startCol (cLines V.! startLine)
                        middle = V.foldl (\acc x -> acc <> "\n" <> x) "" (V.slice (succ startLine) (endLine - startLine) cLines)
                        postfix = Bs.drop endCol (cLines V.! endLine)
                      in
                      prefix <> middle <> postfix
    in
    show lineNum <> ": " <> (T.unpack . T.decodeUtf8) mainText <> "\n"

  showLogic :: Int -> [PhpAction] -> String
  showLogic level actions =
    let
      indent = replicate (level * 2) ' '
    in
    foldl (\acc action -> acc <> "\n" <> indent <> showAction (level + 1) action) "" actions

  showAction :: Int -> PhpAction -> String
  showAction level action =
    let
      indent = replicate (level * 2) ' '
    in
    case action of
      Verbatim uid -> indent <> "Verbatim " <> show uid
      Statement stmt -> indent <> "Statement " <> showStatement (level + 1) stmt
      CommentA uid -> indent <> "CommentA " <> show uid
      MiscST name pos -> indent <> "MiscST " <> show name
      Interpolation content -> indent <> "Interpolation " <> concatMap (showAction level) content
      NoOpAct -> ""

  showStatement :: Int -> PhpStatement -> String
  showStatement level stmt =
    let
      indent = replicate (level * 2) ' '
    in
    case stmt of
      BlockST actions -> indent <> "Block " <> showLogic (level + 1) actions
      NamedLabelST -> indent <> "NamedLabelST "
      ExpressionST expr -> indent <> "Expression " <> showExpr (level + 1) expr
      IfST cond thenBlock elseBlock ->
        let
          elsePart = case elseBlock of
            Nothing -> ""
            Just aBlock -> showAction level aBlock
        in
        indent <> "IfST \n" <> indent <> showExpr level cond <> "\n" <> showAction level thenBlock <> "\n" <> elsePart
      SwitchST -> indent <> "SwitchST "
      WhileST -> indent <> "WhileST "
      DoST -> indent <> "DoST "
      ForST -> indent <> "ForST "
      ForEachST cond (isRef, asVar) mbWithKey iterStmt ->
        indent <> "ForEachST " <> showExpr level cond
        <> "\n" <> if isRef then " Ref " else "" <> showVarSpec level asVar
        <> "\n" <> maybe "" (\(refFlag, varSpec) -> if refFlag then "(refed) " else "" <> showVarSpec level varSpec) mbWithKey
        <> "\n" <> showAction level iterStmt
      GotoST -> indent <> "GotoST "
      ContinueST -> indent <> "ContinueST "
      BreakST -> indent <> "BreakST "
      ReturnST expr -> indent <> "ReturnST " <> maybe "" (showExpr level) expr
      TryST -> indent <> "TryST "
      DeclareST -> indent <> "DeclareST "
      EchoST exprs -> indent <> "EchoST " <> concatMap (showExpr level) exprs
      ExitST mbExpr -> indent <> "ExitST " <> maybe "" (showExpr (level + 1)) mbExpr
      UnsetST -> indent <> "UnsetST "
      ConstDeclST -> indent <> "ConstDeclST "
      FunctionDefST qualName stmt -> indent <> "FunctionDefST " <> show qualName <> "\n" <> showAction level stmt
      ClassDefST attribs modifiers nameID mbBaseID interfDefs classMembers ->
          indent <> "ClassDefST " <> show nameID <> maybe "" (\bid -> " extends " <> show bid) mbBaseID
          <> "\n" <> indent <> "attribs: " <> show attribs <> "; modifiers: " <> show modifiers
          <> "\n" <> indent <> "interfaces: " <> show interfDefs <> "; classMembers:\n"
          <> unlines (map (showMemberDecl (succ level)) classMembers)
      InterfaceDefST -> indent <> "InterfaceDefST "
      TraitDefST -> indent <> "TraitDefST "
      EnumDefST -> indent <> "EnumDefST "
      NamespaceDefST -> indent <> "NamespaceDefST "
      NamespaceUseST -> indent <> "NamespaceUseST "
      GlobalDeclST -> indent <> "GlobalDeclST "
      FunctionStaticST -> indent <> "FunctionStaticST "
      DanglingST dangling -> indent <> "DanglingST " <> show dangling
      _ -> "unprettyfied stmt: " <> show stmt
      

  showMemberDecl :: Int -> ClassMemberDecl -> String
  showMemberDecl level memberDecl =
    let
      indent = replicate (level * 2) ' '
    in
    case memberDecl of
      CommentCDecl uid -> indent <> "comment " <> show uid
      ConstantCDecl modifiers consts -> indent <> "ConstantCDecl " <> show modifiers <> " " <> show consts
      PropertyCDecl attribs modifiers varSpec mbExpr -> indent <> "PropertyCDecl " <> show attribs <> " (" <> show modifiers <> ") " <> show varSpec <> maybe "" (\expr -> ", init: " <> showExpr level expr) mbExpr
      MethodCDecl attribs modifiers varID argSpecs methodImpl ->
          indent <> "MethodCDecl " <> show attribs <> " (" <> show modifiers <> ") " <> show varID
          <> "\n" <> indent <> unlines (map (showVarSpec level) argSpecs)
          <> "\n" <> case methodImpl of
            AbstractMI -> indent <> "abstract method"
            MethodImplementationMI action -> showAction level action
            ReturnTypeMI typeDecl -> indent <> "returns " <> show typeDecl
      ConstructorCDecl -> indent <> "ConstructorCDecl "
      DestructorCDecl -> indent <> "DestructorCDecl "
      TraitUseCDecl nameIDs mbUseList -> indent <> "TraitUseCDecl " <> show nameIDs <> maybe "" (\useList -> " " <> show useList) mbUseList

  showVarSpec :: Int -> VariableSpec -> String
  showVarSpec level varSpec =
    let
      indent = replicate (level * 2) ' '
    in
    case varSpec of
      SimpleVS varID -> indent <> "SimpleVS " <> show varID
      Dynamic varName index -> indent <> "Dynamic " <> show varName <> " " <> show index
      ComplexVS expr -> indent <> "ComplexVS " <> showExpr level expr


  showExpr :: Int -> PhpExpression -> String
  showExpr level expr =
    let
      indent = ""
    in
    case expr of
      Literal uid -> indent <> "Literal " <> show uid
      Variable uid -> indent <> "Variable " <> show uid
      Symbol uid -> indent <> "Symbol " <> show uid
      BinaryOp op lArg rArg -> indent <> "BinaryOp " <> show op <> " " <> showExpr 0 lArg <> " " <> showExpr 0 rArg
      UnaryOp op arg -> indent <> "UnaryOp " <> show op <> " " <> showExpr 0 arg
      TernaryOp cond thenExpr elseExpr ->
        indent <> "TernaryOp \n" <> showExpr (level + 1) cond <> "\n" <> showExpr (level + 1) thenExpr <> "\n" <> showExpr 0 elseExpr
      FunctionCall uid args ->
        indent <> "FunctionCall " <> show uid <> " " <> concatMap (showExpr (level + 1)) args
      ArrayAccess array index ->
        indent <> "ArrayAccess " <> showExpr (level + 1) array <> " " <> showExpr 0 index
      ArrayLiteral exprs ->
        indent <> "ArrayLiteral " <> concatMap (showExpr level) exprs
      Parenthesized exprs ->
        indent <> "Parenthesized " <> concatMap (showExpr level) exprs
      AssignVar isRef assignee expr ->
        indent <> "AssignVar " <> if isRef then "(refed)" else "" <> " " <> show assignee <> " " <> showExpr 0 expr
      CommentX uid -> indent <> "CommentX " <> show uid
      MiscExpr name pos -> indent <> "MiscExpr " <> show name
      Subscript varName mbIndex ->
        indent <> "Subscript " <> show varName <> " " <> maybe "" (showExpr 0) mbIndex
      MemberAccess varName uid ->
        indent <> "MemberAccess " <> show varName <> " " <> show uid
      Conditional cond thenExpr elseExpr ->
        indent <> "Conditional \n" <> indent <> showExpr (level + 1) cond <> "\n" <> indent <> showExpr (level + 1) thenExpr <> "\n" <> showExpr 0 elseExpr
      Casting castType expr ->
        indent <> "Casting " <> show castType <> " " <> showExpr 0 expr
      ObjectCreation nameID args ->
        indent <> "ObjectCreation " <> show nameID <> " " <> concatMap (showExpr level) args
      ScopeCall nameIDs args ->
        indent <> "ScopeCall " <> show nameIDs <> " " <> concatMap (showExpr level) args
      Include inclMode expr ->
        indent <> "Include " <> show inclMode <> " " <> showExpr 0 expr
      AugmentedAssign var op expr ->
        indent <> "AugmentedAssign " <> show var <> " " <> show op <> " " <> showExpr 0 expr
      ScopedPropertyAccess baseName expr ->
        indent <> "ScopedPropertyAccess " <> show baseName <> " " <> showExpr 0 expr
      ErrorSuppression expr ->
        indent <> "ErrorSuppression " <> showExpr 0 expr
      _ -> show expr

{-
  Literal LiteralValue
  | Variable Int
  | Symbol Int
  | BinaryOp BinaryOps PhpExpression PhpExpression
  | UnaryOp UnaryOps PhpExpression
  | TernaryOp PhpExpression PhpExpression PhpExpression
  | FunctionCall Int [PhpExpression]
  | ArrayAccess PhpExpression PhpExpression
  | ArrayLiteral [PhpExpression]
  | Parenthesized [PhpExpression]
  | AssignLocal Int PhpExpression
  | RequireOnce PhpExpression
  | CommentX Int
  | MiscExpr String (TSPoint, TSPoint)

-}
