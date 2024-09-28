module Template.PHP.Print where


import qualified Data.Vector as V
import TreeSitter.Node ( Node(..), TSPoint(TSPoint, pointRow, pointColumn) )

import Template.PHP.Types

printNode :: Int -> NodeEntry -> IO ()
printNode level node = do
  let
    prefix = if level == 0 then "" else replicate ((level - 1) * 2) ' ' <> "| "
  putStrLn $ prefix <> name node <> " " <> showNodePos node
  if null node.children then
    pure ()
  else do
    mapM_ (printNode (succ level)) node.children


showNodePos :: NodeEntry -> String
showNodePos aNode =
  let
    startS = "(" ++ show aNode.start.pointRow ++ "," ++ show aNode.start.pointColumn ++ ")"
    endS = "(" ++ show aNode.end.pointRow ++ "," ++ show aNode.end.pointColumn ++ ")"
  in
  startS <> "-" <> endS


printPhpContext :: String -> PhpContext -> IO ()
printPhpContext content ctxt =
  let
    cLines = V.fromList $ lines content
    demandLines = V.map (fetchContent cLines) $ V.zip ctxt.contentDemands (V.fromList [0..])
  in do
  putStrLn $ "@[printPhpContext] logic: " <> showLogic 0 (V.toList ctxt.logic)
  putStrLn $ "@[printPhpContext] contentDemands: "
  V.mapM_ putStrLn demandLines
  where
  fetchContent :: V.Vector String -> (SegmentPos, Int) -> String
  fetchContent cLines ((start, end), lineNum) =
    let
      startLine = fromIntegral start.pointRow
      startCol = fromIntegral start.pointColumn
      endLine = fromIntegral end.pointRow
      endCol = fromIntegral end.pointColumn
      mainText
        | startLine == endLine = take (endCol - startCol) $ drop startCol (cLines V.! startLine)
        | endCol == 0 = let
                          prefix = drop startCol (cLines V.! startLine)
                          middle = if endLine == succ startLine then
                              ""
                            else
                              V.foldl (\acc x -> acc <> "\n" <> x) "" (V.slice startLine (endLine - startLine - 1) cLines)
                        in
                        prefix <> middle
        | otherwise = let
                        prefix = drop startCol (cLines V.! startLine)
                        middle = V.foldl (\acc x -> acc <> "\n" <> x) "" (V.slice (succ startLine) (endLine - startLine) cLines)
                        postfix = drop endCol (cLines V.! endLine)
                      in
                      prefix <> middle <> postfix
    in
    show lineNum <> ": " <> mainText <> "\n"

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
      Expression expr -> indent <> "Expression " <> showExpr (level + 1) expr
      Block actions -> indent <> "Block " <> showLogic (level + 1) actions
      CommentA uid -> indent <> "CommentA " <> show uid
      MiscST name pos -> indent <> "MiscST " <> show name

  showStatement :: Int -> PhpStatement -> String
  showStatement level stmt =
    let
      indent = replicate (level * 2) ' '
    in
    case stmt of
      IfST cond thenBlock elseBlock ->
        let
          elsePart = case elseBlock of
            Nothing -> ""
            Just aBlock -> showAction level aBlock
        in
        indent <> "IfST \n" <> indent <> showExpr level cond <> "\n" <> showAction level thenBlock <> "\n" <> elsePart
      ClassST name classDef -> indent <> "ClassST " <> show name <> " " <> show classDef

  showExpr :: Int -> PhpExpression -> String
  showExpr level expr =
    let
      indent = ""
    in
    case expr of
      Literal uid -> indent <> "Literal " <> show uid
      Variable uid -> indent <> "Variable " <> show uid
      BinaryOp op -> indent <> "BinaryOp " <> show op
      UnaryOp op arg -> indent <> "UnaryOp " <> show op <> " " <> showExpr 0 arg
      TernaryOp cond thenExpr elseExpr ->
        indent <> "TernaryOp \n" <> showExpr (level + 1) cond <> "\n" <> showExpr (level + 1) thenExpr <> "\n" <> showExpr 0 elseExpr
      ArrayAccess array index ->
        indent <> "ArrayAccess " <> showExpr (level + 1) array <> " " <> showExpr 0 index
      Parenthesized exprs ->
        indent <> "Parenthesized " <> concatMap (showExpr level) exprs
      CommentX uid -> indent <> "CommentX " <> show uid
      MiscExpr name pos -> indent <> "MiscExpr " <> show name

