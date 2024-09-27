{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}
module Template.PHP where

import Control.Monad.Cont (foldM_, foldM)
import Control.Monad.State (runState, execState, State (..), get, put, modify)

import Data.Int ( Int32 )
import qualified Data.Vector as V
import Data.Either (fromRight)
import Data.Maybe (fromJust)
import Data.Text (pack)

import Foreign.C.String ( newCStringLen, peekCString )
import Foreign.Ptr ( Ptr, nullPtr )
import Foreign.Marshal.Alloc ( malloc, free )
import Foreign.Marshal.Array ( mallocArray )
import Foreign.Storable ( peek, peekElemOff, poke )

import TreeSitter.Parser ( ts_parser_new, ts_parser_parse_string, ts_parser_set_language, Parser )
import TreeSitter.Tree ( ts_tree_root_node_p )
import TreeSitter.PHP ( tree_sitter_php )
import TreeSitter.Node ( nodeStartPoint ,ts_node_copy_child_nodes, Node(..)
              , TSPoint(TSPoint, pointRow, pointColumn) )

import Conclusion (GenError (..))
import qualified RunTime.Compiler.Common as C
import qualified RunTime.Compiler.Types as C
import Template.Types ( FileTempl )
import Template.TsParser ( tsParseFile )


tsParsePhp :: FilePath -> IO (Either GenError FileTempl)
tsParsePhp filePath = do
  putStrLn $ "@[tsParsePhp] parsing: " ++ filePath
  parser <- ts_parser_new
  ts_parser_set_language parser tree_sitter_php
  tryParsePhp parser filePath


tryParsePhp :: Ptr Parser -> FilePath -> IO (Either GenError FileTempl)
tryParsePhp parser path = do
  tmplString <- readFile path

  (cStr, strLen) <- newCStringLen tmplString
  tree <- ts_parser_parse_string parser nullPtr cStr strLen

  mem <- malloc
  ts_tree_root_node_p tree mem

  nodeA <- peek mem  -- header, imports, and declarations
  let childCount = fromIntegral nodeA.nodeChildCount

  tsNodeMem <- malloc
  poke tsNodeMem nodeA.nodeTSNode

  children <- mallocArray childCount
  ts_node_copy_child_nodes tsNodeMem children

  rezA <- parseTsChildren children childCount
  case rezA of
    Left err -> do
      putStrLn $ "@[tryParsePhp] parseTsChildren err: " <> show err
    Right logicCtxt -> do
      printPhpContext tmplString logicCtxt

  free children
  free tsNodeMem
  free cStr
  pure . Left $ SimpleMsg "tryParsePhp: done."


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
                        middle = V.foldl (\acc x -> acc <> "\n" <> x) "" (V.slice startLine (endLine - startLine - 1) cLines)
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



-- **** Parsing a TreeSitter's AST for PHP **** --
data NodeEntry = NodeEntry {
  name :: String
  , start :: TSPoint
  , end :: TSPoint
  , children :: [NodeEntry]
  }
  deriving Show


type SegmentPos = (TSPoint, TSPoint)


data PhpAction =
  Verbatim Int
  | Statement PhpStatement
  | Expression PhpExpression
  | Block [PhpAction]
  | CommentA Int
  | MiscST String SegmentPos
  deriving Show

data PhpStatement =
  IfST PhpExpression PhpAction (Maybe PhpAction)
  | ClassST Int [ PhpStatement ]
  deriving Show


data PhpExpression =
  Literal Int
  | Variable Int
  | BinaryOp Int
  | UnaryOp UnaryOps PhpExpression
  | TernaryOp PhpExpression PhpExpression PhpExpression
  | FunctionCall String [PhpExpression]
  | ArrayAccess PhpExpression PhpExpression
  | ArrayLiteral [PhpExpression]
  | Parenthesized [PhpExpression]
  | CommentX Int
  | MiscExpr String (TSPoint, TSPoint)
  deriving Show


data UnaryOps =
  NotOp
  | NegOp
  deriving Show


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


getNewDemand :: SegmentPos -> State PhpContext Int
getNewDemand sPos = do
  ctxt <- get
  let
    nID = V.length ctxt.contentDemands
    newCtxt = ctxt { contentDemands = V.snoc ctxt.contentDemands sPos }
  put newCtxt
  pure nID


getNewDemandWith :: PhpContext -> SegmentPos -> (PhpContext, Int)
getNewDemandWith ctxt sPos =
  let
    nID = V.length ctxt.contentDemands
    newCtxt = ctxt { contentDemands = V.snoc ctxt.contentDemands sPos }
  in
  (newCtxt, nID)



type ParseState = State PhpContext [Either C.CompError ()]


parseNodes :: [NodeEntry] -> Either String PhpContext
parseNodes nodes =
  let
    (results, endState) = runState (foldM parseTopLevel [Right ()] nodes) initPhpContext
  in
  case C.concatErrors results of
    Nothing -> Right endState
    Just err -> Left $ show err


parseTopLevel :: [Either C.CompError ()] -> NodeEntry -> ParseState
parseTopLevel accum node =
  case node.name of
    "text" -> do
      uid <- getNewDemand (node.start, node.end)
      modify $ \ctxt -> ctxt { logic = V.snoc ctxt.logic (Verbatim uid) }
      pure [Right ()]
    "if_statement" -> do
      rezA <- handle_if_statement 0 node.children
      case rezA of
        Left err -> pure $ [Left err]
        Right (cond, thenBlock, elseBlock) -> do
          modify $ \ctxt -> ctxt { logic = V.snoc ctxt.logic (Statement (IfST cond thenBlock elseBlock)) }
          pure [Right ()]
    "class_statement" -> do
      rezA <- handle_class_statement node.children
      case rezA of
        Left err -> pure $ [Left err]
        Right (nameID, classDef) -> do
          modify $ \ctxt -> ctxt { logic = V.snoc ctxt.logic (Statement (ClassST nameID classDef)) }
          pure [Right ()]
    "comment" -> do
      uid <- getNewDemand (node.start, node.end)
      modify $ \ctxt -> ctxt { logic = V.snoc ctxt.logic (CommentA uid) }
      pure $ [Right ()]
    _ -> do
      modify $ \ctxt -> ctxt { logic = V.snoc ctxt.logic ( MiscST node.name (node.start, node.end)) }
      pure [Right ()]


handle_if_statement :: Int -> [NodeEntry] -> State PhpContext (Either C.CompError (PhpExpression, PhpAction, Maybe PhpAction))
handle_if_statement mode children = do
  case children of
    [] -> pure . Left $ C.CompError [(0, "handle_if_statement: expected 3+ children, got 0")]
    first : restA ->
      if mode == 0 then
        case first.name of
          "if" -> do handle_if_statement 1 restA
          "comment" -> do
            uid <- getNewDemand (first.start, first.end)
            modify $ \ctxt -> ctxt { logic = V.snoc ctxt.logic (CommentA uid) }
            handle_if_statement 1 restA
          _ -> pure . Left $ C.CompError [(0, "handle_if_statement: expected 'if' or 'comment', got " <> first.name <> "; " <> showNodePos first)]
      else if mode == 1 then
        case first.name of
          "comment" -> do
            uid <- getNewDemand (first.start, first.end)
            modify $ \ctxt -> ctxt { logic = V.snoc ctxt.logic (CommentA uid) }
            handle_if_statement 1 restA
          "parenthesized_expression" -> do
            expr <- handle_paren_expression first
            case expr of
              Left err -> pure $ Left err
              Right aCond -> do
                case restA of
                  [] -> pure . Left $ C.CompError [(0, "handle_if_statement: expected 2 children, got 1.")]
                  thenBlock : restBlocks -> do
                    eiThenA <- handle_statement thenBlock
                    eiMbElseA <- handle_else_clause 0 [] restBlocks
                    case (eiThenA, eiMbElseA) of
                      (Left errA, Left errB) -> pure . Left . fromJust $ C.concatErrors [Left errA, Left errB]
                      (_, Left err) -> pure $ Left err
                      (Left err, _) -> pure $ Left err
                      (Right thenA, Right mbElseA) ->
                        pure $ Right (aCond, thenA, mbElseA)
          _ -> pure . Left $ C.CompError [(0, "handle_if_statement: expected 'expression' or 'comment', got " <> first.name <> "; " <> showNodePos first)]
      else
        pure . Left $ C.CompError [(0, "handle_if_statement: unknown mode " <> show mode)]



handle_else_clause :: Int -> [PhpAction] -> [NodeEntry] -> State PhpContext (Either C.CompError (Maybe PhpAction))
handle_else_clause mode accum children =
  case children of
    [] -> if null accum then pure $ Right Nothing else pure $ Right (Just (Block accum))
    first : restA -> do
      case mode of
        0 ->
          case first.name of
            "comment" -> do
              uid <- getNewDemand (first.start, first.end)
              handle_else_clause 0 (accum <> [CommentA uid]) restA
            "else_clause" ->
              case first.children of
                [] -> pure . Left $ C.CompError [(0, "handle_if_statement: unexpected end of children after 'else_clause'")]
                elseKw : restB ->
                  if elseKw.name == "else" then
                    handle_else_clause 1 accum restB
                  else
                    pure . Left $ C.CompError [(0, "handle_if_statement: unexpected block after else, got " <> elseKw.name <> "; " <> showNodePos elseKw)]
            _ -> pure . Left $ C.CompError [(0, "handle_if_statement: unexpected block after else, got " <> first.name <> "; " <> showNodePos first)]
        1 ->
          case first.name of
            "comment" -> do
              uid <- getNewDemand (first.start, first.end)
              handle_else_clause 1 (accum <> [CommentA uid]) restA
            _ -> do
              rezA <- handle_statement first
              case rezA of
                Left err -> pure $ Left err
                Right anAction -> pure . Right . Just $ Block (accum <> [ anAction ])


handle_paren_expression :: NodeEntry -> State PhpContext (Either C.CompError PhpExpression)
handle_paren_expression node =
  -- TODO: implement.
  case node.children of
    [] -> pure . Left $ C.CompError [(0, "handle_paren_expression: unexpected end of children after '('")]
    first : restA -> case first.name of
        "(" -> do
          rezA <- handle_paren_elements 0 [] restA
          case rezA of
            Left err -> pure $ Left err
            Right aExpr -> pure $ Right $ Parenthesized aExpr
        _ -> pure . Left $ C.CompError [(0, "handle_paren_expression: expected '(', got " <> node.name <> "; " <> showNodePos node)]


handle_paren_elements :: Int -> [PhpExpression] -> [NodeEntry] -> State PhpContext (Either C.CompError [PhpExpression])
handle_paren_elements mode accum children =
  case children of
    [] -> pure . Left $ C.CompError [(0, "handle_paren_elements: unexpected end of children after '('")]
    first : rest ->
      if first.name == "comment" then do
        uid <- getNewDemand (first.start, first.end)
        handle_paren_elements mode (accum <> [CommentX uid]) rest
      else
        case mode of
          0 -> do
            anExpr <- handle_expression first
            case anExpr of
              Left err -> pure $ Left err
              Right anArg -> handle_paren_elements 1 (accum <> [anArg]) rest
          1 -> case first.name of
            "," -> handle_paren_elements 0 accum rest
            ")" -> pure $ Right accum
            _ -> pure . Left $ C.CompError [(0, "handle_paren_elements: unexpected expr after argument, got " <> first.name <> "; " <> showNodePos first)]


handle_statement :: NodeEntry -> State PhpContext (Either C.CompError PhpAction)
handle_statement node =
  case node.name of
    "compound_statement" -> do
      rezA <- mapM handle_statement node.children
      case C.concatErrors rezA of
        Nothing -> pure $ Right $ Block (map (fromRight (CommentA 0)) rezA)
        Just err -> pure $ Left err
    "expression_statement" ->
      case node.children of
        [] -> pure . Left $ C.CompError [(0, "handle_statement: no children for expression_statement")]
        first : restA -> do
          rezA <- handle_expression first
          case rezA of
            Left err -> pure $ Left err
            Right anExpr -> case restA of
              [] -> pure . Left $ C.CompError [(0, "missing ending ';' on expression_statement")]
              [endNode] -> if endNode.name == ";" then
                  pure . Right $ Expression anExpr
                else
                  pure . Left $ C.CompError [(0, "handle_statement: unexpected block after expression, got " <> endNode.name <> "; " <> showNodePos endNode)]
              _ ->
                pure . Left $ C.CompError [(0, "handle_statement: extra blocks after expression, should have 1, got " <> show (length restA))]
    "comment" -> do
      uid <- getNewDemand (node.start, node.end)
      pure . Right $ CommentA uid
    _ ->
      pure $ Right $ MiscST node.name (node.start, node.end)


handle_expression :: NodeEntry -> State PhpContext (Either C.CompError PhpExpression)
handle_expression node =
  -- TODO: implement.
  case node.name of
    "comment" -> do
      uid <- getNewDemand (node.start, node.end)
      pure . Right $ CommentX uid
    "parenthesized_expression" -> pure . Right $ Parenthesized []
    "binary_expression" -> pure . Right $ BinaryOp 0
    "unary_op_expression" -> case node.children of
      [] -> pure . Left $ C.CompError [(0, "handle_expression: no children for unary_op_expression")]
      first : restA -> case first.name of
        "!" -> do
          rezA <- handle_expression (head restA)
          case rezA of
            Left err -> pure $ Left err
            Right anExpr -> pure . Right $ UnaryOp NotOp anExpr
        "-" -> do
          rezA <- handle_expression (head restA)
          case rezA of
            Left err -> pure $ Left err
            Right anExpr -> pure . Right $ UnaryOp NegOp anExpr
        _ -> pure . Left $ C.CompError [(0, "handle_expression: expected '!' or '-', got " <> node.name <> "; " <> showNodePos node)]
    "variable_name" ->
      handle_variable_name node.children
    "member_access_expression" ->
      -- TODO: implement: variable "->" name
      pure . Right $ MiscExpr node.name (node.start, node.end)
    _ -> pure . Right $ MiscExpr node.name (node.start, node.end)


handle_arguments :: [NodeEntry] -> State PhpContext (Either C.CompError [PhpExpression])
handle_arguments children =
  case children of
    [] -> pure $ Right []
    first : rest ->
      if first.name == "(" then
        handle_argument 0 [] rest
      else
        pure . Left $ C.CompError [(0, "handle_arguments: expected '('")]


handle_argument :: Int -> [PhpExpression] -> [NodeEntry] -> State PhpContext (Either C.CompError [PhpExpression])
handle_argument mode accum argList =
  case argList of
    [] -> pure $ Right accum
    [argNode] ->
      case argNode.name of
        "comment" -> do
          uid <- getNewDemand (argNode.start, argNode.end)
          pure . Right $ accum <> [ CommentX uid ]
        "argument" -> do
          expr <- handle_expression argNode
          case expr of
            Left err -> pure $ Left err
            Right aExpr -> pure . Right $ (accum <> [aExpr])
        _ -> pure . Left $ C.CompError [(0, "handle_argument: unexpected block after else, got " <> argNode.name <> "; " <> showNodePos argNode)]
    first : restA ->
      case first.name of
        "," -> handle_argument 0 accum restA
        ")" -> pure . Right $ accum
        _ -> pure . Left $ C.CompError [(0, "handle_argument: unexpected block after else, got " <> first.name <> "; " <> showNodePos first)]


handle_class_statement :: [NodeEntry] -> State PhpContext (Either C.CompError (Int, [PhpStatement]))
handle_class_statement children = do
  -- TODO: implement.
  pure . Left $ C.CompError [(0, "handle_class_statement: not implemented")]


handle_variable_name :: [NodeEntry] -> State PhpContext (Either C.CompError PhpExpression)
handle_variable_name nodeList =
  case nodeList of
    [] -> pure . Left $ C.CompError [(0, "handle_variable_name: unexpected end of children after '('")]
    first : restA ->
      if first.name == "$" then do
        case restA of
          [ aName ] ->
            if aName.name == "name" then do
              uid <- getNewDemand (aName.start, aName.end)
              pure . Right $ Variable uid
            else
              pure . Left $ C.CompError [(0, "handle_variable_name: expected 'name', got " <> aName.name <> "; " <> showNodePos aName)]
          _ -> pure . Left $ C.CompError [(0, "handle_variable_name: expected 'name', got a list.")]
      else
        pure . Left $ C.CompError [(0, "handle_variable_name: expected '$', got " <> first.name <> "; " <> showNodePos first)]


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


parseTsChildren :: Ptr Node -> Int -> IO (Either GenError PhpContext)
parseTsChildren children count = do
  -- algo: do the descent of ts nodes and extract into verbatim and logic blocks; parse the syntax of each logic block, reassemble into a tree of statements/expressions.
  nodeGraph <- analyzeChildren 0 children count
  -- putStrLn $ "@[parseTsChildren] nodeGraph: " <> show nodeGraph
  mapM_ (printNode 0) nodeGraph
  let
    eiLogicCtxt = parseNodes nodeGraph
  case eiLogicCtxt of
    Left err ->
      let
        errMsg = "@[parseTsChildren] parseNodes err: " <> show err
      in do
      putStrLn errMsg
      pure . Left $ SimpleMsg (pack errMsg)
    Right logicCtxt -> pure $ Right logicCtxt


analyzeChildren :: Int -> Ptr Node -> Int -> IO [NodeEntry]
analyzeChildren level children count = do
  foldM (\accum index -> do
      aNode <- analyzChild level children index
      pure $ accum <> [ aNode ]
    ) [] [0 .. count - 1]


analyzChild :: Int -> Ptr Node -> Int -> IO NodeEntry
analyzChild level children pos = do
  child <- peekElemOff children pos
  -- analyze child's children:
  rezA <- case fromIntegral child.nodeChildCount of
    0 -> pure []
    subCount -> do
      subChildren <- mallocArray subCount
      tsNodeMem <- malloc
      poke tsNodeMem child.nodeTSNode
      ts_node_copy_child_nodes tsNodeMem subChildren
      rezA <- analyzeChildren (level + 1) subChildren subCount
      -- TODO: verify if the subChildren entries need to be freed before the array holder itself is freed.
      free subChildren
      free tsNodeMem
      pure rezA

  blockName <- peekCString child.nodeType
  let pA = nodeStartPoint child
      pB = child.nodeEndPoint
      startS = " (" ++ show pA.pointRow ++ "," ++ show pA.pointColumn ++ ")"
      endS = "(" ++ show pB.pointRow ++ "," ++ show pB.pointColumn ++ ")"
  pure $ NodeEntry blockName pA pB rezA
  -- putStrLn $ "@[analyzChild] blockName: " <> replicate (level * 2) ' ' <> show blockName <> " ; " <> startS <> " - " <> endS
