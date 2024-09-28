{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}
module Template.PHP where

import Control.Monad.Cont (foldM, lift, MonadPlus (..))
import Control.Applicative (asum, many, some, (<|>))

import Data.Data (Data (..))
import Control.Lens (Identity)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Identity (Identity(..))
import Data.Text (pack)

import qualified Data.Vector as V

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
import Template.Types ( FileTempl )

import Template.PHP.Print (printNode, printPhpContext)
import Template.PHP.Scanner (parseNodesB)
import qualified Template.PHP.ScannerB as B
import qualified Template.PHP.Class as B
import qualified Template.PHP.State as B
import qualified Template.PHP.Error as E
import Template.PHP.Types
import Text.Cannelle.Hugo.AST (Statement)


newtype TError = TError String
  deriving (Show, Eq, Ord)
instance Data TError where
  toConstr _ = error "toConstr"
  gunfold _ _ = error "gunfold"
  dataTypeOf _ = error "dataTypeOf"


type ScannerB = B.ScannerT (E.ScanError TError) Identity

testScannerB :: [NodeEntry] -> Either TError [PhpAction]
testScannerB nodes =
  let
    Identity (endState, result) = B.doScanT testS (B.initState nodes)
  in
  case result of
    Left err -> Left $ TError $ show err
    Right value -> Right value


testS :: ScannerB [PhpAction]
testS = do
  rezA <- textS
  rezB <- phpTagS
  rezC <- commentS
  rezD <- ifS
  pure [ rezA, rezB, rezC, rezD ]

statementS :: ScannerB PhpAction
statementS = asum [
  textS
  , phpTagS
  , commentS
  , ifS
  ]


{-
     , commentS
  pure [ rezA, rezB, rezD ]
-}

commentS :: ScannerB PhpAction
commentS = do
  rez <- B.single "comment"
  pure $ CommentA 0

ifS :: ScannerB PhpAction
ifS = do
  rez <- B.single "if_statement"
  pure $ CommentA 1

textS :: ScannerB PhpAction
textS = do
  rez <- B.single "text"
  pure $ CommentA 2

phpTagS :: ScannerB PhpAction
phpTagS = do
  rez <- B.single "php_tag"
  pure $ CommentA 3




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

  rezA <- parseTsAst children childCount
  case rezA of
    Left err -> do
      putStrLn $ "@[tryParsePhp] parseTsAst err: " <> show err
    Right logicCtxt -> do
      printPhpContext tmplString logicCtxt

  free children
  free tsNodeMem
  free cStr
  pure . Left $ SimpleMsg "tryParsePhp: done."


-- **** Parsing a TreeSitter's AST for PHP **** --

parseTsAst :: Ptr Node -> Int -> IO (Either GenError PhpContext)
parseTsAst children count = do
  -- algo: do the descent of ts nodes and extract into verbatim and logic blocks; parse the syntax of each logic block, reassemble into a tree of statements/expressions.
  nodeGraph <- analyzeChildren 0 children count
  -- putStrLn $ "@[parseTsChildren] nodeGraph: " <> show nodeGraph
  mapM_ (printNode 0) nodeGraph
  putStrLn "@[parseTsAst] testing ScannerB"
  case testScannerB nodeGraph of
    Left err -> putStrLn $ "@[parseTsAst] testScannerB err: " <> show err
    Right value -> putStrLn $ "@[parseTsAst] tested ScannerB: " <> show value
  let
    eiLogicCtxt = parseNodesB nodeGraph
  case eiLogicCtxt of
    Left err ->
      let
        errMsg = "@[parseTsAst] parseNodes err: " <> show err
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
