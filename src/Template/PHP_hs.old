{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
module Template.PHP where

import Control.Monad (when)
import Control.Monad.Cont (foldM)

import qualified Data.ByteString as Bs
import Data.Text (pack, unpack)
import qualified Data.Text.Encoding as T
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

import Cannelle.PHP.NeParser (testScannerB)
import Cannelle.PHP.Print (printPhpContext, printNode)
import Cannelle.PHP.Types
import Cannelle.PHP.Parser.Types (TError (..))
import Cannelle.PHP.AST (PhpContext (..))

import Conclusion (GenError (..))
import Template.Types ( FileTempl )


tsParsePhp :: FilePath -> IO (Either GenError FileTempl)
tsParsePhp filePath = do
  putStrLn $ "@[tsParsePhp] parsing: " ++ filePath
  parser <- ts_parser_new
  ts_parser_set_language parser tree_sitter_php
  tryParsePhp parser filePath


tryParsePhp :: Ptr Parser -> FilePath -> IO (Either GenError FileTempl)
tryParsePhp parser path = do
  tmplString <- Bs.readFile path

  (cStr, strLen) <- newCStringLen $ unpack . T.decodeUtf8 $ tmplString
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
      putStrLn "@[tryParsePhp] logicCtxt: "
      printPhpContext tmplString logicCtxt

  free children
  free tsNodeMem
  free cStr
  pure . Left $ SimpleMsg "tryParsePhp: done."


-- **** Parsing a TreeSitter's AST for PHP **** --

parseTsAst :: Ptr Node -> Int -> IO (Either GenError PhpContext)
parseTsAst children count = do
  -- TODO: define the debug flag as part of parameters.
  let showGraph = True
  -- algo: do the descent of ts nodes and extract into verbatim and logic blocks; parse the syntax of each logic block, reassemble into a tree of statements/expressions.
  nodeGraph <- analyzeChildren 0 children count
  -- putStrLn $ "@[parseTsChildren] nodeGraph: " <> show nodeGraph
  when showGraph $ mapM_ (printNode 0) nodeGraph
  let
    scanRez = testScannerB nodeGraph
  case scanRez of
    Left (TError errMsg) -> pure . Left . SimpleMsg . pack $ "@[parseTsAst] testScannerB err: " <> errMsg
    Right context -> pure $ Right context

  -- testScannerC nodeGraph
  {-
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
  -}

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
