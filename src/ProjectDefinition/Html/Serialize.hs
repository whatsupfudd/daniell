{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}
module ProjectDefinition.Html.Serialize where

import Control.Monad (unless, when, replicateM)
import qualified Control.Monad.State.Strict as StS

import Data.Binary.Put (putByteString, putInt32be, runPut, Put)
import Data.Binary.Get( Get, getInt32be, isolate, runGetOrFail)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Data.Int (Int32, Int64)
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IM
import qualified Data.IntSet as IS
import Data.Text (Text)
import qualified Data.Text.Encoding as TE
import Data.Vector (Vector)
import qualified Data.Vector as V

import ProjectDefinition.Html.Types

--------------------------------------------------------------------------------
-- Compact text table serialisation
--------------------------------------------------------------------------------

-- | Serialise the ordered compact text table.
--
-- Expected input:
--   * entries are sorted by Int32 ID ascending
--   * IDs are contiguous and match vector positions: 0,1,2,3,...
--
-- Binary layout:
--   [Int32BE lengthsTableByteLen]
--   [Int32BE textBlockByteLen]
--   [Int32BE text0Utf8Len]
--   [Int32BE text1Utf8Len]
--   ...
--   [utf8(text0)]
--   [utf8(text1)]
--   ...
--
-- Lengths are UTF-8 byte lengths, not character counts.
serializeCompactText :: Vector (Int32, Text) -> Either String ByteString
serializeCompactText entries = do
  validateOrderedCompactText entries
  pure (serializeCompactTextUnchecked entries)

-- | Same as 'serializeCompactText', but skips validation.
serializeCompactTextUnchecked :: Vector (Int32, Text) -> ByteString
serializeCompactTextUnchecked entries =
  let utf8Entries :: Vector ByteString
      utf8Entries =
        V.map (TE.encodeUtf8 . snd) entries

      textLengths :: Vector Int32
      textLengths =
        V.map (fromIntegral . BS.length) utf8Entries

      lengthsTableByteLen :: Int32
      lengthsTableByteLen =
        fromIntegral (V.length textLengths * 4)

      textBlockByteLen :: Int32
      textBlockByteLen =
        V.foldl' (\acc bs -> acc + fromIntegral (BS.length bs)) 0 utf8Entries

      out = runPut $ do
        putInt32be lengthsTableByteLen
        putInt32be textBlockByteLen
        V.mapM_ putInt32be textLengths
        V.mapM_ putByteString utf8Entries
  in LBS.toStrict out

-- | Verify that the ordered text pool is actually ordered and contiguous:
--     (0, ...)
--     (1, ...)
--     (2, ...)
--     ...
--
-- This matches the deserialisation strategy where the position in the length table
-- is used as the lookup index / text ID.
validateOrderedCompactText :: Vector (Int32, Text) -> Either String ()
validateOrderedCompactText = V.ifoldM_ checkOne ()
  where
  checkOne :: () -> Int -> (Int32, Text) -> Either String ()
  checkOne () ix (txtId, _) = do
    let expected = fromIntegral ix
    unless (txtId == expected) $ Left ("serializeCompactText: expected text ID "
          <> show expected <> " at vector position " <> show ix
          <> ", but got " <> show txtId <> "."
      )

--------------------------------------------------------------------------------
-- Optional helpers
--------------------------------------------------------------------------------

-- | Extract just the lengths table as raw bytes.
serializeCompactTextLengthsTable :: Vector (Int32, Text) -> Either String ByteString
serializeCompactTextLengthsTable entries = do
  validateOrderedCompactText entries
  let utf8Entries = V.map (TE.encodeUtf8 . snd) entries
      textLengths = V.map (fromIntegral . BS.length) utf8Entries :: Vector Int32
      out = runPut $ V.mapM_ putInt32be textLengths
  pure (LBS.toStrict out)

-- | Extract just the compact UTF-8 text block as raw bytes.
serializeCompactTextBlock :: Vector (Int32, Text) -> Either String ByteString
serializeCompactTextBlock entries = do
  validateOrderedCompactText entries
  let utf8Entries = V.map (TE.encodeUtf8 . snd) entries
      out = runPut $ V.mapM_ putByteString utf8Entries
  pure (LBS.toStrict out)


--------------------------------------------------------------------------------
-- Serialisation of HtmlDocumentC
--------------------------------------------------------------------------------

-- | Fixed node record stored in the binary node table.
--
-- nodeType:
--   1 = element node
--   2 = text node
--
-- payload:
--   * for element nodes: tag code (known tags positive, unknown tags negative)
--   * for text nodes   : pooled text ID
--
-- parentId:
--   * -1 for root nodes
--
-- childStart / childCount:
--   direct child node IDs live in the child-refs table
--
-- subtreeSize:
--   number of nodes in this subtree including the node itself
--   useful to skip subtrees quickly in analytic traversals
--
data FlatNodeRec = FlatNodeRec
  { nodeTypeR    :: !Int32
  , payloadR     :: !Int32
  , parentIdR    :: !Int32
  , attrStartR   :: !Int32
  , attrCountR   :: !Int32
  , childStartR  :: !Int32
  , childCountR  :: !Int32
  , subtreeSizeR :: !Int32
  }

data FlatAttrRec = FlatAttrRec
  { attrNameR  :: !Int32
  , attrValueR :: !Int32
  }

data FlatBuild = FlatBuild
  { nextNodeIdB   :: !Int32
  , nextAttrIxB   :: !Int32
  , nextChildIxB  :: !Int32
  , nodesB        :: !(IntMap FlatNodeRec)
  , attrsRevB     :: ![FlatAttrRec]
  , childRefsRevB :: ![Int32]
  , trackedRevB   :: !(IntMap [Int32])
  }

emptyFlatBuild :: FlatBuild
emptyFlatBuild =
  FlatBuild
    { nextNodeIdB   = 0
    , nextAttrIxB   = 0
    , nextChildIxB  = 0
    , nodesB        = IM.empty
    , attrsRevB     = []
    , childRefsRevB = []
    , trackedRevB   = IM.empty
    }

type FlatM = StS.State FlatBuild

-- | Main entry point.
serializeHtmlDocumentC :: HtmlDocumentC -> ByteString
serializeHtmlDocumentC docC =
  let
    (rootIdsV, st0) =
      StS.runState (traverse (flattenNodeC (-1)) docC.domC) emptyFlatBuild

    rootIds :: [Int32]
    rootIds = V.toList rootIdsV

    tracked :: IntMap [Int32]
    tracked = fmap reverse (trackedRevB st0)

    rootsBlock    = encodeRootsBlock rootIds
    trackedBlock  = encodeTrackedBlock tracked
    nodesBlock    = encodeNodesBlock (IM.elems (nodesB st0))
    attrsBlock    = encodeAttrsBlock (reverse (attrsRevB st0))
    childRefsBlock = encodeChildRefsBlock (reverse (childRefsRevB st0))

    firstHtmlId   = firstTrackedNode htmlTagCode tracked
    firstHeadId   = firstTrackedNode headTagCode tracked
    firstTitleId  = firstTrackedNode titleTagCode tracked
    firstBodyId   = firstTrackedNode bodyTagCode tracked
    firstScriptId = firstTrackedNode scriptTagCode tracked
    firstStyleId  = firstTrackedNode styleTagCode tracked
    firstLinkId   = firstTrackedNode linkTagCode tracked
    firstMetaId   = firstTrackedNode metaTagCode tracked

    out = runPut $ do
      -- header
      putInt32be 1
      putInt32be docC.docHeaderC

      putInt32be firstHtmlId
      putInt32be firstHeadId
      putInt32be firstTitleId
      putInt32be firstBodyId
      putInt32be firstScriptId
      putInt32be firstStyleId
      putInt32be firstLinkId
      putInt32be firstMetaId

      putInt32be (byteLen32 rootsBlock)
      putInt32be (byteLen32 trackedBlock)
      putInt32be (byteLen32 nodesBlock)
      putInt32be (byteLen32 attrsBlock)
      putInt32be (byteLen32 childRefsBlock)

      -- blocks
      putByteStringStrict rootsBlock
      putByteStringStrict trackedBlock
      putByteStringStrict nodesBlock
      putByteStringStrict attrsBlock
      putByteStringStrict childRefsBlock
  in
    LBS.toStrict out

--------------------------------------------------------------------------------
-- Flatten HtmlDocumentC
--------------------------------------------------------------------------------

flattenNodeC :: Int32 -> HtmlNodeC -> FlatM Int32
flattenNodeC parentId = \case
  TextEntryC txtId -> do
    nodeId <- allocNodeId
    insertNodeRec
      nodeId
      FlatNodeRec
        { nodeTypeR    = 2
        , payloadR     = txtId
        , parentIdR    = parentId
        , attrStartR   = 0
        , attrCountR   = 0
        , childStartR  = 0
        , childCountR  = 0
        , subtreeSizeR = 1
        }
    pure nodeId

  CommentEntryC txtId -> do
    nodeId <- allocNodeId
    insertNodeRec
      nodeId
      FlatNodeRec
        { nodeTypeR    = 3
        , payloadR     = txtId
        , parentIdR    = parentId
        , attrStartR   = 0
        , attrCountR   = 0
        , childStartR  = 0
        , childCountR  = 0
        , subtreeSizeR = 1
        }
    pure nodeId

  NodeHC tagIDC attributesC childrenC -> do
    nodeId <- allocNodeId

    attrStart <- StS.gets nextAttrIxB
    appendAttrs attributesC
    let
      attrCount = fromIntegral (V.length attributesC)

    childIds <- V.toList <$> traverse (flattenNodeC nodeId) childrenC

    childStart <- StS.gets nextChildIxB
    appendChildRefs childIds
    childRecs <- traverse lookupNodeRecOrBug childIds

    let
      childCount = fromIntegral (length childIds)
      subtreeSize = 1 + sum (map subtreeSizeOfNode childRecs)

    insertNodeRec nodeId FlatNodeRec {
          nodeTypeR    = 1
        , payloadR     = tagIDC
        , parentIdR    = parentId
        , attrStartR   = attrStart
        , attrCountR   = attrCount
        , childStartR  = childStart
        , childCountR  = childCount
        , subtreeSizeR = subtreeSize
        }

    recordTrackedTag tagIDC nodeId
    pure nodeId


lookupNodeRecOrBug :: Int32 -> FlatM FlatNodeRec
lookupNodeRecOrBug nodeId = do
  st <- StS.get
  case IM.lookup (fromIntegral nodeId) st.nodesB of
    Just rec -> pure rec
    Nothing  -> error ("serializeHtmlDocumentC: missing node record for node ID " <> show nodeId)


allocNodeId :: FlatM Int32
allocNodeId = do
  st <- StS.get
  StS.put st { nextNodeIdB = st.nextNodeIdB + 1 }
  pure st.nextNodeIdB

insertNodeRec :: Int32 -> FlatNodeRec -> FlatM ()
insertNodeRec nodeId rec = do
  st <- StS.get
  StS.put st { nodesB = IM.insert (fromIntegral nodeId) rec st.nodesB }


subtreeSizeOfNode :: FlatNodeRec -> Int32
subtreeSizeOfNode = subtreeSizeR

appendAttrs :: Vector AttributeC -> FlatM ()
appendAttrs attrsV = do
  st <- StS.get
  let
    attrsList = map (\attrib -> FlatAttrRec attrib.nameC attrib.valueC) (V.toList attrsV)
    n = fromIntegral (length attrsList)
  StS.put st {
      nextAttrIxB = st.nextAttrIxB + n
    , attrsRevB   = reverse attrsList <> st.attrsRevB
    }

appendChildRefs :: [Int32] -> FlatM ()
appendChildRefs childIds = do
  st <- StS.get
  let n = fromIntegral (length childIds)
  StS.put st
    { nextChildIxB  = st.nextChildIxB + n
    , childRefsRevB = reverse childIds <> st.childRefsRevB
    }

recordTrackedTag :: Int32 -> Int32 -> FlatM ()
recordTrackedTag tagCode nodeId
  | shouldTrackTagCode tagCode = do
      st <- StS.get
      let
        tracked' = IM.insertWith (<>) (fromIntegral tagCode) [nodeId] st.trackedRevB
      StS.put st { trackedRevB = tracked' }
  | otherwise = pure ()

--------------------------------------------------------------------------------
-- Which tags get direct pointer tables
--------------------------------------------------------------------------------

trackedTagCodes :: IS.IntSet
trackedTagCodes =
  IS.fromList
    [ fromIntegral htmlTagCode
    , fromIntegral headTagCode
    , fromIntegral titleTagCode
    , fromIntegral bodyTagCode
    , fromIntegral scriptTagCode
    , fromIntegral styleTagCode
    , fromIntegral linkTagCode
    , fromIntegral metaTagCode
    ]

shouldTrackTagCode :: Int32 -> Bool
shouldTrackTagCode tagCode =
  tagCode > 0 && IS.member (fromIntegral tagCode) trackedTagCodes

htmlTagCode, headTagCode, titleTagCode, bodyTagCode :: Int32
scriptTagCode, styleTagCode, linkTagCode, metaTagCode :: Int32

htmlTagCode   = fromIntegral (tagIdToInt HtmlT)
headTagCode   = fromIntegral (tagIdToInt HeadT)
titleTagCode  = fromIntegral (tagIdToInt TitleT)
bodyTagCode   = fromIntegral (tagIdToInt BodyT)
scriptTagCode = fromIntegral (tagIdToInt ScriptT)
styleTagCode  = fromIntegral (tagIdToInt StyleT)
linkTagCode   = fromIntegral (tagIdToInt LinkT)
metaTagCode   = fromIntegral (tagIdToInt MetaT)

firstTrackedNode :: Int32 -> IntMap [Int32] -> Int32
firstTrackedNode tagCode tracked =
  case IM.lookup (fromIntegral tagCode) tracked of
    Just (x:_) -> x
    _          -> -1

--------------------------------------------------------------------------------
-- Block encoding
--------------------------------------------------------------------------------

-- roots block:
--   [Int32 rootCount]
--   [rootNodeId ...]
encodeRootsBlock :: [Int32] -> ByteString
encodeRootsBlock rootIds =
  LBS.toStrict $ runPut $ do
    putInt32be (fromIntegral (length rootIds))
    mapM_ putInt32be rootIds

-- tracked block:
--   [Int32 groupCount]
--   repeated:
--     [Int32 tagCode]
--     [Int32 nodeCount]
--     [nodeId ...]
encodeTrackedBlock :: IntMap [Int32] -> ByteString
encodeTrackedBlock tracked =
  let groups = IM.toAscList tracked
  in LBS.toStrict $ runPut $ do
      putInt32be (fromIntegral (length groups))
      mapM_
        (\(tagCodeI, nodeIds) -> do
            putInt32be (fromIntegral tagCodeI)
            putInt32be (fromIntegral (length nodeIds))
            mapM_ putInt32be nodeIds
        )
        groups

-- nodes block:
--   [Int32 nodeCount]
--   repeated fixed-width records of 8 Int32:
--     nodeType, payload, parentId, attrStart, attrCount, childStart, childCount, subtreeSize
encodeNodesBlock :: [FlatNodeRec] -> ByteString
encodeNodesBlock recs =
  LBS.toStrict $ runPut $ do
    putInt32be (fromIntegral (length recs))
    mapM_ putNodeRec recs
  where
  putNodeRec flatNodeRec = do
    putInt32be flatNodeRec.nodeTypeR
    putInt32be flatNodeRec.payloadR
    putInt32be flatNodeRec.parentIdR
    putInt32be flatNodeRec.attrStartR
    putInt32be flatNodeRec.attrCountR
    putInt32be flatNodeRec.childStartR
    putInt32be flatNodeRec.childCountR
    putInt32be flatNodeRec.subtreeSizeR

-- attrs block:
--   [Int32 attrCount]
--   repeated:
--     [Int32 nameId]
--     [Int32 valueId]
encodeAttrsBlock :: [FlatAttrRec] -> ByteString
encodeAttrsBlock recs =
  LBS.toStrict $ runPut $ do
    putInt32be (fromIntegral (length recs))
    mapM_ putAttrRec recs
  where
    putAttrRec flatAttrRec = do
      putInt32be flatAttrRec.attrNameR
      putInt32be flatAttrRec.attrValueR

-- child refs block:
--   [Int32 childRefCount]
--   [childNodeId ...]
encodeChildRefsBlock :: [Int32] -> ByteString
encodeChildRefsBlock refs =
  LBS.toStrict $ runPut $ do
    putInt32be (fromIntegral (length refs))
    mapM_ putInt32be refs

--------------------------------------------------------------------------------
-- Small helpers
--------------------------------------------------------------------------------

byteLen32 :: ByteString -> Int32
byteLen32 = fromIntegral . BS.length

putByteStringStrict :: ByteString -> Data.Binary.Put.Put
putByteStringStrict = putByteString


--------------------------------------------------------------------------------
-- Deserialisation of HtmlDocumentC
--------------------------------------------------------------------------------

deserializeHtmlDocumentC :: ByteString -> Either String HtmlDocumentC
deserializeHtmlDocumentC bs =
  case runGetOrFail getHtmlDocumentC (LBS.fromStrict bs) of
    Left (_, _, err) -> Left ("deserializeHtmlDocumentC: " <> err)

    Right (rest, _, docC)
      | LBS.null rest -> Right docC
      | otherwise -> Left ("deserializeHtmlDocumentC: trailing bytes remaining after decode: " <> show (LBS.length rest))


getHtmlDocumentC :: Get HtmlDocumentC
getHtmlDocumentC = do
  version <- getInt32be
  when (version /= 1) $
    fail ("unsupported HtmlDocumentC version: " <> show version)

  docHeaderC0 <- getInt32be

  -- Fast analytic pointers from the serialised header.
  -- We read them to stay aligned with the format, even if the recursive rebuild
  -- does not need them directly.
  _firstHtmlNodeId <- getInt32be
  _firstHeadNodeId <- getInt32be
  _firstTitleNodeId <- getInt32be
  _firstBodyNodeId <- getInt32be
  _firstScriptNodeId <- getInt32be
  _firstStyleNodeId <- getInt32be
  _firstLinkNodeId <- getInt32be
  _firstMetaNodeId <- getInt32be

  rootsBlockByteLen <- fromIntegral <$> getBlockLen "roots block"
  trackedBlockByteLen <- fromIntegral <$> getBlockLen "tracked block"
  nodesBlockByteLen <- fromIntegral <$> getBlockLen "nodes block"
  attrsBlockByteLen <- fromIntegral <$> getBlockLen "attrs block"
  childRefsBlockByteLen <- fromIntegral <$> getBlockLen "child refs block"

  rootIds <- isolate rootsBlockByteLen getRootsBlock
  _tracked <- isolate trackedBlockByteLen getTrackedBlock
  nodeRecs <- isolate nodesBlockByteLen getNodesBlock
  attrRecs <- isolate attrsBlockByteLen getAttrsBlock
  childRefs <- isolate childRefsBlockByteLen getChildRefsBlock

  let
    nodesV = V.fromList nodeRecs
    attrsV = V.fromList attrRecs
    childRefsV = V.fromList childRefs

  domNodes <-
    case traverse (rebuildHtmlNodeC nodesV attrsV childRefsV) rootIds of
      Left err -> fail err
      Right xs -> pure xs

  pure HtmlDocumentC {
      docHeaderC = docHeaderC0
    , domC       = V.fromList domNodes
    }

--------------------------------------------------------------------------------
-- Block readers
--------------------------------------------------------------------------------

getBlockLen :: String -> Get Int32
getBlockLen label = do
  n <- getInt32be
  when (n < 0) $
    fail ("negative byte length for " <> label <> ": " <> show n)
  pure (fromIntegral n)

-- roots block:
--   [Int32 rootCount]
--   [rootNodeId ...]
getRootsBlock :: Get [Int32]
getRootsBlock = do
  cnt <- getCount "root count"
  replicateM cnt getInt32be

-- tracked block:
--   [Int32 groupCount]
--   repeated:
--     [Int32 tagCode]
--     [Int32 nodeCount]
--     [nodeId ...]
--
-- We keep the parsed result available in case later code wants to expose it for
-- analytics, but the recursive HtmlDocumentC rebuild does not require it.
getTrackedBlock :: Get (IM.IntMap [Int32])
getTrackedBlock = do
  groupCnt <- getCount "tracked group count"
  groups <- replicateM groupCnt $ do
    tagCode <- getInt32be
    nodeCnt <- getCount "tracked node count"
    nodeIds <- replicateM nodeCnt getInt32be
    pure (fromIntegral tagCode, nodeIds)
  pure (IM.fromList groups)

-- nodes block:
--   [Int32 nodeCount]
--   repeated fixed-width records of 8 Int32:
--     nodeType, payload, parentId, attrStart, attrCount, childStart, childCount, subtreeSize
getNodesBlock :: Get [FlatNodeRec]
getNodesBlock = do
  cnt <- getCount "node count"
  replicateM cnt $ do
    nodeTypeR <- getInt32be
    payloadR <- getInt32be
    parentIdR <- getInt32be
    attrStartR <- getInt32be
    attrCountR <- getInt32be
    childStartR <- getInt32be
    childCountR <- getInt32be
    FlatNodeRec nodeTypeR payloadR parentIdR attrStartR attrCountR childStartR childCountR <$> getInt32be

-- attrs block:
--   [Int32 attrCount]
--   repeated:
--     [Int32 nameId]
--     [Int32 valueId]
getAttrsBlock :: Get [FlatAttrRec]
getAttrsBlock = do
  cnt <- getCount "attribute count"
  replicateM cnt $ do
    attrNameR  <- getInt32be
    FlatAttrRec attrNameR <$> getInt32be

-- child refs block:
--   [Int32 childRefCount]
--   [childNodeId ...]
getChildRefsBlock :: Get [Int32]
getChildRefsBlock = do
  cnt <- getCount "child ref count"
  replicateM cnt getInt32be

getCount :: String -> Get Int
getCount label = do
  n <- getInt32be
  when (n < 0) $
    fail ("negative " <> label <> ": " <> show n)
  pure (fromIntegral n)

--------------------------------------------------------------------------------
-- Recursive rebuild from flat tables
--------------------------------------------------------------------------------

rebuildHtmlNodeC
  :: Vector FlatNodeRec
  -> Vector FlatAttrRec
  -> Vector Int32
  -> Int32
  -> Either String HtmlNodeC
rebuildHtmlNodeC nodesV attrsV childRefsV nodeId = do
  rec <- lookupNodeRecE nodesV nodeId
  case nodeTypeR rec of
    1 -> do
      attrsSlice <- sliceVectorE "attributes" attrsV (attrStartR rec) (attrCountR rec)
      childIds   <- sliceVectorE "child refs" childRefsV (childStartR rec) (childCountR rec)

      childrenV <-
        V.fromList <$> traverse (rebuildHtmlNodeC nodesV attrsV childRefsV) (V.toList childIds)

      let
        attrsV' = V.map (\flatAttrRec -> AttributeC { nameC  = flatAttrRec.attrNameR, valueC = flatAttrRec.attrValueR }) attrsSlice

      pure NodeHC { tagIDC = payloadR rec, attributesC = attrsV', childrenC = childrenV }

    2 -> pure (TextEntryC (payloadR rec))
    3 -> pure (CommentEntryC (payloadR rec))

    other ->
      Left
        ( "rebuildHtmlNodeC: unknown node type "
            <> show other
            <> " for node ID "
            <> show nodeId
        )

lookupNodeRecE :: Vector FlatNodeRec -> Int32 -> Either String FlatNodeRec
lookupNodeRecE nodesV nodeId = do
  ix <- int32ToIndex "node ID" nodeId
  case nodesV V.!? ix of
    Just rec ->
      pure rec
    Nothing ->
      Left
        ( "lookupNodeRecE: node ID out of bounds: "
            <> show nodeId
            <> ", node count = "
            <> show (V.length nodesV)
        )

sliceVectorE
  :: String
  -> Vector a
  -> Int32
  -> Int32
  -> Either String (Vector a)
sliceVectorE label vec start32 count32 = do
  start <- int32ToIndex (label <> " start") start32
  count <- int32ToIndex (label <> " count") count32
  let end = start + count
  unless (end <= V.length vec) $
    Left
      ( "sliceVectorE: "
          <> label
          <> " slice out of bounds: start="
          <> show start
          <> ", count="
          <> show count
          <> ", length="
          <> show (V.length vec)
      )
  pure (V.slice start count vec)

int32ToIndex :: String -> Int32 -> Either String Int
int32ToIndex label n
  | n < 0 =
      Left (label <> " must be non-negative, got " <> show n)
  | otherwise =
      pure (fromIntegral n)