module Template.PHP.Scanner where

import Control.Monad.State (runState, execState, State (..), get, put, modify)
import Control.Monad.Cont (foldM_, foldM)

import Data.Either (fromRight)
import Data.Maybe (fromJust)
import Data.Text (pack)
import qualified Data.Vector as V

import qualified RunTime.Compiler.Common as C
import qualified RunTime.Compiler.Types as C

import Template.PHP.Print (showNodePos)
import Template.PHP.Types


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


parseNodesB :: [NodeEntry] -> Either String PhpContext
parseNodesB nodes =
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
      rezA <- ifStmtH 0 node.children
      case rezA of
        Left err -> pure $ [Left err]
        Right (cond, thenBlock, elseBlock) -> do
          modify $ \ctxt -> ctxt { logic = V.snoc ctxt.logic (Statement (IfST cond thenBlock elseBlock)) }
          pure [Right ()]
    "class_statement" -> do
      rezA <- classStmtH node.children
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


ifStmtH :: Int -> [NodeEntry] -> State PhpContext (Either C.CompError (PhpExpression, PhpAction, Maybe PhpAction))
ifStmtH mode children = do
  case children of
    [] -> pure . Left $ C.CompError [(0, "ifStmtH: expected 3+ children, got 0")]
    first : restA ->
      if mode == 0 then
        case first.name of
          "if" -> do ifStmtH 1 restA
          "comment" -> do
            uid <- getNewDemand (first.start, first.end)
            modify $ \ctxt -> ctxt { logic = V.snoc ctxt.logic (CommentA uid) }
            ifStmtH 1 restA
          _ -> pure . Left $ C.CompError [(0, "ifStmtH: expected 'if' or 'comment', got " <> first.name <> "; " <> showNodePos first)]
      else if mode == 1 then
        case first.name of
          "comment" -> do
            uid <- getNewDemand (first.start, first.end)
            modify $ \ctxt -> ctxt { logic = V.snoc ctxt.logic (CommentA uid) }
            ifStmtH 1 restA
          "parenthesized_expression" -> do
            expr <- parenExprH first
            case expr of
              Left err -> pure $ Left err
              Right aCond -> do
                case restA of
                  [] -> pure . Left $ C.CompError [(0, "ifStmtH: expected 2 children, got 1.")]
                  thenBlock : restBlocks -> do
                    eiThenA <- allStmtH thenBlock
                    eiMbElseA <- elseClauseH 0 [] restBlocks
                    case (eiThenA, eiMbElseA) of
                      (Left errA, Left errB) -> pure . Left . fromJust $ C.concatErrors [Left errA, Left errB]
                      (_, Left err) -> pure $ Left err
                      (Left err, _) -> pure $ Left err
                      (Right thenA, Right mbElseA) ->
                        pure $ Right (aCond, thenA, mbElseA)
          _ -> pure . Left $ C.CompError [(0, "ifStmtH: expected 'expression' or 'comment', got " <> first.name <> "; " <> showNodePos first)]
      else
        pure . Left $ C.CompError [(0, "ifStmtH: unknown mode " <> show mode)]



elseClauseH :: Int -> [PhpAction] -> [NodeEntry] -> State PhpContext (Either C.CompError (Maybe PhpAction))
elseClauseH mode accum children =
  case children of
    [] -> case accum of 
        [] -> pure $ Right Nothing
        [ele] -> pure $ Right (Just ele)
        _ -> pure $ Right (Just (Block accum))
    first : restA -> do
      case mode of
        0 ->
          case first.name of
            "comment" -> do
              uid <- getNewDemand (first.start, first.end)
              elseClauseH 0 (accum <> [CommentA uid]) restA
            "else_clause" ->
              case first.children of
                [] -> pure . Left $ C.CompError [(0, "ifStmtH: unexpected end of children after 'else_clause'")]
                elseKw : restB ->
                  if elseKw.name == "else" then
                    elseClauseH 1 accum restB
                  else
                    pure . Left $ C.CompError [(0, "ifStmtH: unexpected block after else, got " <> elseKw.name <> "; " <> showNodePos elseKw)]
            _ -> pure . Left $ C.CompError [(0, "ifStmtH: unexpected block after else, got " <> first.name <> "; " <> showNodePos first)]
        1 ->
          case first.name of
            "comment" -> do
              uid <- getNewDemand (first.start, first.end)
              elseClauseH 1 (accum <> [CommentA uid]) restA
            _ -> do
              rezA <- allStmtH first
              case rezA of
                Left err -> pure $ Left err
                Right anAction -> pure . Right . Just $ if null accum then anAction else Block (accum <> [ anAction ])


parenExprH :: NodeEntry -> State PhpContext (Either C.CompError PhpExpression)
parenExprH node =
  -- TODO: implement.
  case node.children of
    [] -> pure . Left $ C.CompError [(0, "parenExprH: unexpected end of children after '('")]
    first : restA -> case first.name of
        "(" -> do
          rezA <- parenElemH 0 [] restA
          case rezA of
            Left err -> pure $ Left err
            Right aExpr -> pure $ Right $ Parenthesized aExpr
        _ -> pure . Left $ C.CompError [(0, "parenExprH: expected '(', got " <> node.name <> "; " <> showNodePos node)]


parenElemH :: Int -> [PhpExpression] -> [NodeEntry] -> State PhpContext (Either C.CompError [PhpExpression])
parenElemH mode accum children =
  case children of
    [] -> pure . Left $ C.CompError [(0, "parenElemH: unexpected end of children after '('")]
    first : rest ->
      if first.name == "comment" then do
        uid <- getNewDemand (first.start, first.end)
        parenElemH mode (accum <> [CommentX uid]) rest
      else
        case mode of
          0 -> do
            anExpr <- allExprH first
            case anExpr of
              Left err -> pure $ Left err
              Right anArg -> parenElemH 1 (accum <> [anArg]) rest
          1 -> case first.name of
            "," -> parenElemH 0 accum rest
            ")" -> pure $ Right accum
            _ -> pure . Left $ C.CompError [(0, "parenElemH: unexpected expr after argument, got " <> first.name <> "; " <> showNodePos first)]


allStmtH :: NodeEntry -> State PhpContext (Either C.CompError PhpAction)
allStmtH node =
  case node.name of
    "compound_statement" -> do
      rezA <- mapM allStmtH node.children
      case C.concatErrors rezA of
        Nothing -> pure $ Right $ Block (map (fromRight (CommentA 0)) rezA)
        Just err -> pure $ Left err
    "expression_statement" ->
      case node.children of
        [] -> pure . Left $ C.CompError [(0, "allStmtH: no children for expression_statement")]
        first : restA -> do
          rezA <- allExprH first
          case rezA of
            Left err -> pure $ Left err
            Right anExpr -> case restA of
              [] -> pure . Left $ C.CompError [(0, "missing ending ';' on expression_statement")]
              [endNode] -> if endNode.name == ";" then
                  pure . Right $ Expression anExpr
                else
                  pure . Left $ C.CompError [(0, "allStmtH: unexpected block after expression, got " <> endNode.name <> "; " <> showNodePos endNode)]
              _ ->
                pure . Left $ C.CompError [(0, "allStmtH: extra blocks after expression, should have 1, got " <> show (length restA))]
    "comment" -> do
      uid <- getNewDemand (node.start, node.end)
      pure . Right $ CommentA uid
    _ ->
      pure $ Right $ MiscST node.name (node.start, node.end)


allExprH :: NodeEntry -> State PhpContext (Either C.CompError PhpExpression)
allExprH node =
  -- TODO: implement.
  case node.name of
    "comment" -> do
      uid <- getNewDemand (node.start, node.end)
      pure . Right $ CommentX uid
    "parenthesized_expression" -> pure . Right $ Parenthesized []
    "binary_expression" -> pure . Right $ BinaryOp 0
    "unary_op_expression" -> case node.children of
      [] -> pure . Left $ C.CompError [(0, "allExprH: no children for unary_op_expression")]
      first : restA -> case first.name of
        "!" -> do
          rezA <- allExprH (head restA)
          case rezA of
            Left err -> pure $ Left err
            Right anExpr -> pure . Right $ UnaryOp NotOp anExpr
        "-" -> do
          rezA <- allExprH (head restA)
          case rezA of
            Left err -> pure $ Left err
            Right anExpr -> pure . Right $ UnaryOp NegOp anExpr
        _ -> pure . Left $ C.CompError [(0, "allExprH: expected '!' or '-', got " <> node.name <> "; " <> showNodePos node)]
    "variable_name" ->
      varNameEleH node.children
    "member_access_expression" ->
      -- TODO: implement: variable "->" name
      pure . Right $ MiscExpr node.name (node.start, node.end)
    _ -> pure . Right $ MiscExpr node.name (node.start, node.end)


argArrayH :: [NodeEntry] -> State PhpContext (Either C.CompError [PhpExpression])
argArrayH children =
  case children of
    [] -> pure $ Right []
    first : rest ->
      if first.name == "(" then
        argElemH 0 [] rest
      else
        pure . Left $ C.CompError [(0, "argArrayH: expected '('")]


argElemH :: Int -> [PhpExpression] -> [NodeEntry] -> State PhpContext (Either C.CompError [PhpExpression])
argElemH mode accum argList =
  case argList of
    [] -> pure $ Right accum
    [argNode] ->
      case argNode.name of
        "comment" -> do
          uid <- getNewDemand (argNode.start, argNode.end)
          pure . Right $ accum <> [ CommentX uid ]
        "argument" -> do
          expr <- allExprH argNode
          case expr of
            Left err -> pure $ Left err
            Right aExpr -> pure . Right $ (accum <> [aExpr])
        _ -> pure . Left $ C.CompError [(0, "argElemH: unexpected block after else, got " <> argNode.name <> "; " <> showNodePos argNode)]
    first : restA ->
      case first.name of
        "," -> argElemH 0 accum restA
        ")" -> pure . Right $ accum
        _ -> pure . Left $ C.CompError [(0, "argElemH: unexpected block after else, got " <> first.name <> "; " <> showNodePos first)]


classStmtH :: [NodeEntry] -> State PhpContext (Either C.CompError (Int, [PhpStatement]))
classStmtH children = do
  -- TODO: implement.
  pure . Left $ C.CompError [(0, "classStmtH: not implemented")]


varNameEleH :: [NodeEntry] -> State PhpContext (Either C.CompError PhpExpression)
varNameEleH nodeList =
  case nodeList of
    [] -> pure . Left $ C.CompError [(0, "varNameEleH: unexpected end of children after '('")]
    first : restA ->
      if first.name == "$" then do
        case restA of
          [ aName ] ->
            if aName.name == "name" then do
              uid <- getNewDemand (aName.start, aName.end)
              pure . Right $ Variable uid
            else
              pure . Left $ C.CompError [(0, "varNameEleH: expected 'name', got " <> aName.name <> "; " <> showNodePos aName)]
          _ -> pure . Left $ C.CompError [(0, "varNameEleH: expected 'name', got a list.")]
      else
        pure . Left $ C.CompError [(0, "varNameEleH: expected '$', got " <> first.name <> "; " <> showNodePos first)]

