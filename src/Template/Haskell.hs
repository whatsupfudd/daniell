module Template.Haskell where

import Control.Monad ( forM_, when )

import qualified Data.Map as Mp

import Foreign.C.String ( newCStringLen, peekCString )
import Foreign.Ptr ( Ptr, nullPtr )
import Foreign.Marshal.Alloc ( malloc, free )
import Foreign.Marshal.Array ( mallocArray )
import Foreign.Storable ( peek, peekElemOff, poke )

import TreeSitter.Parser ( ts_parser_new, ts_parser_parse_string, ts_parser_set_language )
import TreeSitter.Tree ( ts_tree_root_node_p )
import TreeSitter.Haskell ( tree_sitter_haskell )
import TreeSitter.Node ( nodeStartPoint ,ts_node_copy_child_nodes, Node(..)
              , TSPoint(TSPoint, pointRow, pointColumn) )
import TreeSitter.Language (symbolToName, fromTSSymbol)

import Conclusion (GenError (..))
import Template.Types
import qualified Control.Lens.Internal.Zoom as Ccl


treeSitterHS :: FilePath -> IO (Either GenError FileTempl)
treeSitterHS path = do
  putStrLn $ "@[treeSitterHS] parsing: " ++ path

  parser <- ts_parser_new
  ts_parser_set_language parser tree_sitter_haskell

  source <- readFile path

  (codeStr, strLen) <- newCStringLen source
  tree <- ts_parser_parse_string parser nullPtr codeStr strLen

  mem <- malloc
  ts_tree_root_node_p tree mem

  nodeA <- peek mem  -- header, imports, and declarations
  let childCount = fromIntegral nodeA.nodeChildCount

  tsNodeMem <- malloc
  poke tsNodeMem nodeA.nodeTSNode

  children <- mallocArray childCount
  ts_node_copy_child_nodes tsNodeMem children

  putStrLn $ "@[printChildren] >>>"
  printChildren children childCount 0
  putStrLn $ "@[printChildren] <<<"

  rezA <- analyzeChildren children childCount

  free children
  free tsNodeMem
  free codeStr

  case rezA of
    Left err -> pure $ Left err
    Right templTree -> pure . Right $ FileTempl path Nothing Mp.empty [] []


data TemplTsTree = TemplTsTree {
  totalNodes :: Int
  , verbatimBlocks :: [Node]
  , logicBlocks :: [Node]
  }


analyzeChildren :: Ptr Node -> Int -> IO (Either GenError TemplTsTree)
analyzeChildren children count =
  -- TODO: do the descent of ts nodes and extract into verbatim and logic blocks; parse the syntax of each logic block, reassemble into a tree of statements/expressions.
  {-
    -- but: une liste de verbatim/logic.
    --   il faut descendre dans chaque enfant pour trouver s'il y a un bloc => descente en premier
    --   en descente, on aggrege la pos de depart/courante quand ce n'est pas un bloc,
    --     si c'est un bloc, on termine l'aggregation, ajoute le bloc a la liste,
    --     et recommence le verbatim avec la pos suivante.
    foldM (analyzeNode children) ([] :: [ParseBlock]) [0 .. count - 1]
    data ParseBlock = Verbatim (TSPoint, TSPoint) | Logic (TSPoint, TSPoint)
    analyzeNode :: Ptr Node -> [ParseBlock] -> Int -> IO [ParseBlock]
    analyzeNode children pBlocks pos = do
      child <- peekElemOff children pos
      -- analyze child's children:
      p2Blocks <- case fromIntegral child.nodeChildCount of
        0 -> pBlocks
        subCount -> do
        subChildren <- mallocArray subCount
        tsNodeMem <- malloc
        poke tsNodeMem child.nodeTSNode
        ts_node_copy_child_nodes tsNodeMem subChildren
        childrenBlocks <- analyzeChildren subChildren subCount
        free subChildren
        free tsNodeMem
        pure childrenBlocks

      let pA = nodeStartPoint child
          pB = child.nodeEndPoint
          blockType = fromTSSymbol child.nodeSymbol
      
      case blockType of
        "dant" -> pure $ (verbatims, Logic (pA, pB) : logics)
        _ -> pure $ (Verbatim (pA, pB) : verbatims, logics)
        _ -> pure $ (verbatims, logics)
  -}
  pure $ Right $ TemplTsTree count [] []


printChildren :: Ptr Node -> Int -> Int -> IO ()
printChildren children count level = do
  forM_
    [0 .. count - 1]
    (\n -> do
      child <- peekElemOff children n
      printNode level child
      let subCount = fromIntegral child.nodeChildCount
      when (subCount > 0) $ do
        putStrLn $ replicate (level*2) ' ' ++ "["
        subChildren <- mallocArray subCount
        tsNodeMem <- malloc
        poke tsNodeMem child.nodeTSNode
        ts_node_copy_child_nodes tsNodeMem subChildren

        printChildren subChildren subCount (level + 1)

        free subChildren
        free tsNodeMem
        putStrLn $ replicate (level*2) ' ' ++ "]"
    )

printNode :: Int -> Node -> IO ()
printNode offset n = do
  theType <- peekCString n.nodeType
  let pA = nodeStartPoint n
      start = " (" ++ show pA.pointRow ++ "," ++ show pA.pointColumn ++ ")"
      pB = n.nodeEndPoint
      end = "(" ++ show pB.pointRow ++ "," ++ show pB.pointColumn ++ ")"
      -- symbolInfo = symbolToName symbol theType
  putStrLn $ replicate (offset*2) ' ' ++ theType ++ "<" ++ show n.nodeSymbol ++ ">" ++ start ++ "-" ++ end
