module Template.Haskell where

import Control.Monad ( forM_, when )

import qualified Data.Map as Mp

import TreeSitter.Parser ( ts_parser_new, ts_parser_parse_string, ts_parser_set_language )
import TreeSitter.Tree ( ts_tree_root_node_p )
import TreeSitter.Haskell ( tree_sitter_haskell )
import TreeSitter.Node ( nodeStartPoint ,ts_node_copy_child_nodes, Node(..)
              , TSPoint(TSPoint, pointRow, pointColumn) )
import Foreign.C.String ( newCStringLen, peekCString )
import Foreign.Ptr ( Ptr, nullPtr )
import Foreign.Marshal.Alloc ( malloc )
import Foreign.Marshal.Array ( mallocArray )
import Foreign.Storable ( peek, peekElemOff, poke )

import Conclusion (GenError (..))
import Template.Types
import qualified Control.Lens.Internal.Zoom as Ccl


treeSitterHS :: FilePath -> IO (Either GenError FileTempl)
treeSitterHS path = do
  putStrLn $ "treeSitter File: " ++ path ++ " ------------"

  parser <- ts_parser_new
  ts_parser_set_language parser tree_sitter_haskell

  source <- readFile path

  (codeStr, strLen) <- newCStringLen source
  tree <- ts_parser_parse_string parser nullPtr codeStr strLen

  mem <- malloc
  ts_tree_root_node_p tree mem

  nodeA <- peek mem  -- header, imports, and declarations
  let childCount = fromIntegral nodeA.nodeChildCount

  children <- mallocArray childCount
  tsNodeMem <- malloc
  poke tsNodeMem nodeA.nodeTSNode
  ts_node_copy_child_nodes tsNodeMem children

  -- printChildren children childCount 0
  rezA <- analyzeTree children childCount
  case rezA of
    Left err -> pure $ Left err
    Right templTree -> pure . Right $ FileTempl path Nothing Mp.empty [] []


data TemplTsTree = TemplTsTree {
  totalNodes :: Int
  , verbatimBlocks :: [Node]
  , logicBlocks :: [Node]
  }


analyzeTree :: Ptr Node -> Int -> IO (Either GenError TemplTsTree)
analyzeTree children count =
  -- TODO: do the descent of ts nodes and extract into verbatim and logic blocks; parse the syntax of each logic block, reassemble into a tree of statements/expressions.
  pure $ Right $ TemplTsTree count [] []


printChildren :: Ptr Node -> Int -> Int -> IO ()
printChildren children count level = forM_
  [0 .. count - 1]
  (\n -> do
    child <- peekElemOff children n
    printNode level child
    let subCount = fromIntegral child.nodeChildCount
    when (subCount > 0) $ do
      subChildren <- mallocArray subCount
      tsNodeMem <- malloc
      poke tsNodeMem child.nodeTSNode
      ts_node_copy_child_nodes tsNodeMem subChildren

      printChildren subChildren subCount (level + 1)
      putStrLn $ replicate level ' ' ++ "==="

  )

printNode :: Int -> Node -> IO ()
printNode offset n = do
  theType <- peekCString n.nodeType
  let pA = nodeStartPoint n
      start = " (" ++ show pA.pointRow ++ "," ++ show pA.pointColumn ++ ")"
  let pB = n.nodeEndPoint
      end = "(" ++ show pB.pointRow ++ "," ++ show pB.pointColumn ++ ")"
  putStrLn $ replicate offset ' ' ++ theType ++ start ++ "-" ++ end
