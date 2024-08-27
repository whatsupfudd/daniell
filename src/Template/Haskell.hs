{-# OPTIONS_GHC -Wno-overlapping-patterns #-}
module Template.Haskell where

import Control.Monad ( forM_, when )
import Control.Monad.Cont (foldM)

import qualified Data.ByteString as BS
import Data.Either (fromLeft, fromRight)
import qualified Data.Text.Encoding as TE
import qualified Data.Map as Mp
import Data.Text (pack)
import qualified Data.Vector as Vc

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
import qualified Template.Fuddle.BParser as Fp
import qualified Template.Fuddle.Compiler as Fc



treeSitterHS :: FilePath -> IO (Either GenError FileTempl)
treeSitterHS path = do
  putStrLn $ "@[treeSitterHS] parsing: " ++ path

  parser <- ts_parser_new
  ts_parser_set_language parser tree_sitter_haskell

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

  free children
  free tsNodeMem
  free cStr

  case rezA of
    Left err -> pure $ Left err
    Right tsTree ->     
      if tsTree.hasLogic then do
          -- parse the blocks to create a VM code.
          {-
          putStrLn $ "@[printChildren] >>>"
          printChildren children childCount 0
          putStrLn $ "@[printChildren] <<<"
          -}
          eicompiRez <- compileParseBlocks path tmplString tsTree
          case eicompiRez of
            Left err -> do
              putStrLn "@[treeSitterHS] compileParseBlocks err: "
              print err
              pure $ Left err
            Right vmCode -> do
              putStrLn $ "@[treeSitterHS] vmCode: " ++ show vmCode
              {- serialize that info to the cache file for the template (same path minus name ext + .dtch) -}
              pure . Right $ FileTempl path Nothing Mp.empty [ Noop ] []
      else
          pure . Right $ FileTempl path Nothing Mp.empty [ CloneVerbatim path ] []


compileParseBlocks :: String -> String -> TemplTsTree -> IO (Either GenError Fc.VMCode)
compileParseBlocks codeName fileContent tsTree =
  let
    tmplText = TE.encodeUtf8 . pack $ fileContent
    linesList = BS.split 10 tmplText
    lines = Vc.fromList linesList
  in do
  rezA <- mapM (\b ->
    let
      blockText = getBlockContent lines b
    in do
      -- putStrLn $ "@[compileParseBlocks] block: " ++ show b
      -- putStrLn $ "@[compileParseBlocks] content: " ++ show blockText
      case b of
        Logic _ -> do
          parseRez <- Fp.parseLogicBlock (startPos b) codeName blockText
          case parseRez of
            Left err -> pure $ Left err
            Right stmts -> pure $ Right stmts
        Verbatim _ ->
          let
            parseRez = Fp.parseVerbatimBlock blockText
          in
          pure $ parseRez
    ) tsTree.blocks
  let
    (lefts, rights) = foldl eiSplit ([], []) rezA
  if null lefts then
    case Fp.astBlocksToTree (fromRight [] $ sequence rights) of
      Left err -> pure . Left $ err
      Right astTree -> do
        -- putStrLn $ "@[compileParseBlocks] ast blocks: " ++ show (sequence rights)
        -- putStrLn $ "@[compileParseBlocks] ast tree: " ++ show astTree
        case Fc.compileAstTree astTree of
          Left err -> pure $ Left err
          Right vmCode -> pure $ Right vmCode
  else
    let
      combinedMsg = foldl (\accum lErr ->
          case lErr of
            Left err ->
              case err of
                SimpleMsg aMsg -> accum <> "\n" <> aMsg
                _ -> accum <> "\nunknown err: " <> (pack . show $ err)
          ) "" lefts
    in
    pure . Left . SimpleMsg $ combinedMsg
  where
    eiSplit :: ([Either a b], [Either a b]) -> Either a b -> ([Either a b], [Either a b])
    eiSplit (lefts, rights) eiItem =
      case eiItem of
        Left err -> (lefts <> [eiItem], rights)
        Right aCode -> (lefts, rights <> [eiItem])


startPos :: ParseBlock -> (Int, Int)
startPos pBlock =
  case pBlock of
    Verbatim (pA, _) -> (fromIntegral pA.pointRow, fromIntegral pA.pointColumn)
    Logic (pA, _) -> (fromIntegral pA.pointRow, fromIntegral pA.pointColumn)


getBlockContent :: Vc.Vector BS.ByteString -> ParseBlock -> BS.ByteString
getBlockContent lines pBlock =
  let
    (TSPoint rA cA, TSPoint rB cB) = case pBlock of
        Verbatim (pA, pB) -> (pA, pB)
        Logic (pA, pB) -> (pA, pB)
    eleLength = fromIntegral $ cB - cA + 1
    startPos = fromIntegral cA
  in
  if rA == rB then
    BS.take eleLength . BS.drop startPos $ lines Vc.! fromIntegral rA
  else
    mergeLines lines rA rB eleLength startPos
  where
  mergeLines lines rA rB eleLength startPos =
    foldl (\accum (line, remainder) ->
        if remainder > 0 then
          accum <> line <> "\n"
        else
          accum <> (BS.take eleLength . BS.drop startPos $ line)
      ) "" [(lines Vc.! fromIntegral r, rB - r) | r <- [rA .. rB]]


data TemplTsTree = TemplTsTree {
  hasLogic :: Bool
  , blocks :: [ParseBlock]
  }


data ParseBlock =
    Verbatim (TSPoint, TSPoint)
    | Logic (TSPoint, TSPoint)

instance Show ParseBlock where
  show (Verbatim (pA, pB)) = "Verbatim (" <> show pA.pointRow <> ", "
        <> show pA.pointColumn <> ")-(" <> show pB.pointRow <> ", " <> show pB.pointColumn <> ")"
  show (Logic (pA, pB)) = "Logic (" <> show pA.pointRow <> ", "
        <> show pA.pointColumn <> ")-(" <> show pB.pointRow <> ", " <> show pB.pointColumn <> ")"


parseTsChildren :: Ptr Node -> Int -> IO (Either GenError TemplTsTree)
parseTsChildren children count = do
  -- algo: do the descent of ts nodes and extract into verbatim and logic blocks; parse the syntax of each logic block, reassemble into a tree of statements/expressions.
  (blocks, hasLogic) <- analyzeChildren children count
  -- TODO: consolidate the blocks.
  condensedBlocks <-
    if hasLogic then do
      putStrLn $ "@[parseTsChildren] has logic blocks."
      pure $ mergePBlocks blocks
    else
      let
        (min, max) = foldl (\(minP, maxP) block ->
          case block of
            Verbatim (pA, pB) -> (minTsP minP pA, maxTsP maxP pB)
            Logic (pA, pB) -> (minP, maxP)  -- this should never happen...
          ) (TSPoint 0 0, TSPoint 0 0) blocks
        in do
          putStrLn $ "@[parseTsChildren] single verbatim, min: " ++ show min ++ ", max: " ++ show max
          pure [Verbatim (min, max)]
  -- putStrLn $ "@[parseTsChildren] blocks: " ++ show blocks
  pure $ Right $ TemplTsTree hasLogic condensedBlocks


minTsP :: TSPoint -> TSPoint -> TSPoint
minTsP (TSPoint r1 c1) (TSPoint r2 c2)
  | r1 < r2 = TSPoint r1 c1
  | r1 == r2 = TSPoint r1 (min c1 c2)
  | otherwise = TSPoint r2 c2
maxTsP :: TSPoint -> TSPoint -> TSPoint
maxTsP (TSPoint r1 c1) (TSPoint r2 c2)
  | r1 > r2 = TSPoint r1 c1
  | r1 == r2 = TSPoint r1 (max c1 c2)
  | otherwise = TSPoint r2 c2


mergePBlocks :: [ParseBlock] -> [ParseBlock]
mergePBlocks =
  reverse . foldl (\accum b ->
      case b of
        Logic (pA, pB) ->
          case accum of
            [] -> [ b ]
            Logic (p1A, p1B) : rest -> b : Logic (p1A, p1B) : rest
            Verbatim (p1A, p1B) : rest -> b : Verbatim (p1A, p1B) : rest
        Verbatim (pA, pB) ->
          case accum of
            [] -> [ b ]
            Verbatim (p1A, p1B) : rest -> Verbatim (minTsP p1A pA, maxTsP p1B pB) : rest
            rest -> Verbatim (pA, pB) : rest
    ) []

-- but: une liste de verbatim/logic.
--   il faut descendre dans chaque enfant pour trouver s'il y a un bloc => descente en premier
--   en descente, on aggrege la pos de depart/courante quand ce n'est pas un bloc,
--     si c'est un bloc, on termine l'aggregation, ajoute le bloc a la liste,
--     et recommence le verbatim avec la pos suivante.
analyzeChildren :: Ptr Node -> Int -> IO ([ParseBlock], Bool)
analyzeChildren children count =
  foldM (\(curBlocks, curLogicF) index -> do
      (childBlocks, logicF) <- analyzChild children index
      pure $ (curBlocks <> childBlocks, curLogicF || logicF)
    ) ([] :: [ParseBlock], False) [0 .. count - 1]


analyzChild :: Ptr Node -> Int -> IO ([ParseBlock], Bool)
analyzChild children pos = do
  child <- peekElemOff children pos
  -- analyze child's children:
  (childrenBlocks, childrenLogicF) <- case fromIntegral child.nodeChildCount of
    0 -> pure ([], False)
    subCount -> do
      subChildren <- mallocArray subCount
      tsNodeMem <- malloc
      poke tsNodeMem child.nodeTSNode
      ts_node_copy_child_nodes tsNodeMem subChildren
      rezA <- analyzeChildren subChildren subCount
      free subChildren
      free tsNodeMem
      pure rezA

  blockName <- peekCString child.nodeType
  let pA = nodeStartPoint child
      pB = child.nodeEndPoint

  case blockName of
    "dantempl" -> pure $ (childrenBlocks <> [Logic (pA, pB)], True)
    _ -> pure $ (childrenBlocks <> [ Verbatim (pA, pB) ], childrenLogicF)


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

        free tsNodeMem
        free subChildren
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
