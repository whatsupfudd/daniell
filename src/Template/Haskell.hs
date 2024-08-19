module Template.Haskell where

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
import Control.Monad ( forM_ )


testTreeSitter :: FilePath -> IO ()
testTreeSitter path = do
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

{-
  putStrLn "declarations ------------"
  nodeB <- peekElemOff children 3
  let nextChildCount = fromIntegral nodeB.nodeChildCount

  nextChildren <- mallocArray nextChildCount
  nextTsNode   <- malloc
  poke nextTsNode nodeB.nodeTSNode
  ts_node_copy_child_nodes nextTsNode nextChildren

  printChildren nextChildren nextChildCount
-}

  putStrLn "---------"

printChildren :: Ptr Node -> Int -> Int -> IO ()
printChildren children count level = forM_
  [0 .. count - 1]
  (\n -> do
    child <- peekElemOff children n
    printNode level child
    let subCount = fromIntegral child.nodeChildCount
    if subCount > 0 then do
      subChildren <- mallocArray subCount
      tsNodeMem <- malloc
      poke tsNodeMem child.nodeTSNode
      ts_node_copy_child_nodes tsNodeMem subChildren

      printChildren subChildren subCount (level + 1)
      putStrLn $ replicate level ' ' ++ "==="
    else
      pure ()

  )

printNode :: Int -> Node -> IO ()
printNode offset n = do
  theType <- peekCString n.nodeType
  let pA = nodeStartPoint n
      start = " (" ++ show pA.pointRow ++ "," ++ show pA.pointColumn ++ ")"
  let pB = n.nodeEndPoint
      end = "(" ++ show pB.pointRow ++ "," ++ show pB.pointColumn ++ ")"
  putStrLn $ replicate offset ' ' ++ theType ++ start ++ "-" ++ end
