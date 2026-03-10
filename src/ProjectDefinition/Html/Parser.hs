{-# LANGUAGE LambdaCase #-}
module ProjectDefinition.Html.Parser where

import Control.Monad.State.Strict

import Data.Char (toLower)
import Data.Int (Int32)
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import Data.List (foldl', sortOn)
import Data.Text (Text)
import qualified Data.Text.Lazy as Tl
import qualified Data.Text.Lazy.Builder as Tlb
import qualified Data.Text as T
import Data.Vector (Vector)
import qualified Data.Vector as V
import Text.HTML.Parser (Attr(..), Token(..))

import ProjectDefinition.Html.Types

--------------------------------------------------------------------------------
-- Tree construction
--------------------------------------------------------------------------------

htmlDocumentFromTokens :: [Token] -> HtmlDocument
htmlDocumentFromTokens toks =
  let st = foldl' step initialState toks
      finalState = flushOpenFrames st
  in HtmlDocument { 
        docHeader = T.strip (T.unwords (reverse finalState.headerRev))
      , dom       = V.fromList (reverse finalState.rootRev)
      }

--------------------------------------------------------------------------------
-- Internal builder state
--------------------------------------------------------------------------------

data BuildState = BuildState
  { headerRev :: ![Text]
  , rootRev   :: ![HtmlNode]
  , stack     :: ![Frame]
  }

data Frame = Frame
  { frameTagID    :: !TagID
  , frameAttrs    :: !(Vector Attribute)
  , frameChildren :: ![HtmlNode]   -- reversed
  }

initialState :: BuildState
initialState = BuildState
  { headerRev = []
  , rootRev   = []
  , stack     = []
  }

step :: BuildState -> Token -> BuildState
step st = \case
  Doctype t ->
    st { headerRev = t : st.headerRev }

  ContentText t ->
    appendText t st

  ContentChar c ->
    appendText (T.singleton c) st

  Comment builder ->
    let
      t = Tlb.toLazyText builder
    in
    appendComment (Tl.toStrict t) st

  TagSelfClose rawName attrs0 ->
    appendNode (mkLeafNode rawName attrs0) st

  TagOpen rawName attrs0
    | isVoidTagName rawName ->
        appendNode (mkLeafNode rawName attrs0) st
    | otherwise ->
        st
          { stack =
              Frame
                { frameTagID    = labelToTagId rawName
                , frameAttrs    = attrsToVector attrs0
                , frameChildren = []
                }
              : st.stack
          }

  TagClose rawName ->
    closeTag rawName st

--------------------------------------------------------------------------------
-- Node helpers
--------------------------------------------------------------------------------

mkLeafNode :: Text -> [Attr] -> HtmlNode
mkLeafNode rawName attrs0 =
  NodeH
    { tagID      = labelToTagId rawName
    , attributes = attrsToVector attrs0
    , children   = V.empty
    }

mkNodeFromFrame :: Frame -> HtmlNode
mkNodeFromFrame fr =
  NodeH
    { tagID      = fr.frameTagID
    , attributes = fr.frameAttrs
    , children   = V.fromList (reverse fr.frameChildren)
    }

attrsToVector :: [Attr] -> Vector Attribute
attrsToVector =
  V.fromList . fmap (\(Attr n v) -> Attribute n v)

appendText :: Text -> BuildState -> BuildState
appendText t st
  | T.null t   = st
  | otherwise  = appendNode (TextEntry t) st

appendComment :: Text -> BuildState -> BuildState
appendComment t st
  | T.null t   = st
  | otherwise  = appendNode (CommentEntry t) st

appendNode :: HtmlNode -> BuildState -> BuildState
appendNode node st =
  case st.stack of
    [] ->
      st { rootRev = prependNodeMerged node st.rootRev }

    fr : rest ->
      st
        { stack =
            fr { frameChildren = prependNodeMerged node fr.frameChildren }
            : rest
        }

prependNodeMerged :: HtmlNode -> [HtmlNode] -> [HtmlNode]
prependNodeMerged newNode acc =
  case (newNode, acc) of
    (TextEntry a, TextEntry b : xs) -> TextEntry (a <> b) : xs
    _                               -> newNode : acc

--------------------------------------------------------------------------------
-- Closing logic (forgiving HTML-style recovery)
--------------------------------------------------------------------------------

closeTag :: Text -> BuildState -> BuildState
closeTag rawName st =
  let wanted = (T.toLower . T.strip) rawName
  in case break (frameMatches wanted) st.stack of
      (_, []) ->
        -- No matching open tag: ignore stray close tag.
        st

      (toAutoClose, matchFr : rest) ->
        let restWithAutos = foldl' attachClosedFrame rest (reverse toAutoClose)
            restWithMatch = attachClosedFrame restWithAutos matchFr
        in st { stack = restWithMatch }

frameMatches :: Text -> Frame -> Bool
frameMatches wanted fr =
  tagIdName fr.frameTagID == wanted

attachClosedFrame :: [Frame] -> Frame -> [Frame]
attachClosedFrame stk fr =
  let node = mkNodeFromFrame fr
  in case stk of
      [] ->
        [Frame (UnknownT "__root__") V.empty [node]]
      parent : more ->
        parent { frameChildren = prependNodeMerged node parent.frameChildren } : more

flushOpenFrames :: BuildState -> BuildState
flushOpenFrames st =
  case st.stack of
    [] ->
      st

    fr : rest ->
      let node = mkNodeFromFrame fr
      in case rest of
          [] ->
            flushOpenFrames st
              { stack   = []
              , rootRev = prependNodeMerged node st.rootRev
              }

          parent : more ->
            flushOpenFrames st
              { stack =
                  parent
                    { frameChildren = prependNodeMerged node parent.frameChildren }
                  : more
              }

--------------------------------------------------------------------------------
-- Tag support
--------------------------------------------------------------------------------


isVoidTagName :: Text -> Bool
isVoidTagName rawName =
  (T.toLower . T.strip) rawName `elem`
    [ "area"
    , "base"
    , "br"
    , "col"
    , "embed"
    , "hr"
    , "img"
    , "input"
    , "link"
    , "meta"
    , "param"
    , "source"
    , "track"
    , "wbr"
    ]

--------------------------------------------------------------------------------
-- Public entry point
--------------------------------------------------------------------------------

-- | Convert a regular HtmlDocument into:
--     (1) a compact text pool keyed by Int32
--     (2) a DOM tree that only references pooled text IDs
--
-- Reserve:
--   0 -> ""
--
compactHtmlDocument :: HtmlDocument -> (TextPool, HtmlDocumentC)
compactHtmlDocument doc =
  let
    (docC0, st) = runState (compactDocumentM doc) initialPoolState
  in
  (poolById st, docC0)

-- | Deterministic, ID-ordered view of the text pool.
-- Useful when serialising the pool into a compact blob or DB rows.
orderedTextPool :: TextPool -> Vector (Int32, Text)
orderedTextPool pool =
  let
    pairs = sortOn fst (HM.toList pool)
  in
  V.fromList pairs

--------------------------------------------------------------------------------
-- Internal pool state
--------------------------------------------------------------------------------

data PoolState = PoolState {
    nextTextId :: !Int32
  , poolByText :: !(HashMap Text Int32)
  , poolById   :: !(HashMap Int32 Text)
  }


initialPoolState :: PoolState
initialPoolState = PoolState {nextTextId = 1, poolByText = HM.singleton "" 0, poolById = HM.singleton 0 ""}


type PoolM = State PoolState

internText :: Text -> PoolM Int32
internText txt = do
  st <- get
  case HM.lookup txt st.poolByText of
    Just tid -> pure tid
    Nothing ->
      let
        tid = st.nextTextId
      in do
      put st { nextTextId = tid + 1
        , poolByText = HM.insert txt tid st.poolByText
        , poolById   = HM.insert tid txt st.poolById
        }
      pure tid

--------------------------------------------------------------------------------
-- Compactification logic
--------------------------------------------------------------------------------

compactDocumentM :: HtmlDocument -> PoolM HtmlDocumentC
compactDocumentM doc = do
  headerId <- internText doc.docHeader
  dom' <- traverse compactNodeM doc.dom
  pure HtmlDocumentC
    { docHeaderC = headerId
    , domC = dom'
    }


compactNodeM :: HtmlNode -> PoolM HtmlNodeC
compactNodeM = \case
  TextEntry txt ->
    TextEntryC <$> internText txt
  CommentEntry txt ->
    CommentEntryC <$> internText txt
  node -> do
    tagCode <- compactTagCode node.tagID
    attrs'  <- traverse compactAttributeM node.attributes
    kids'   <- traverse compactNodeM node.children
    pure NodeHC{tagIDC = tagCode, attributesC = attrs', childrenC = kids'}


compactAttributeM :: Attribute -> PoolM AttributeC
compactAttributeM attr = do
  nameId  <- internText attr.name
  valueId <- internText attr.value
  pure AttributeC { nameC  = nameId, valueC = valueId }


-- | Known tags become positive tag codes via tagIdToInt.
-- | Unknown tags become negative codes, where:
--       negate textId
--   points to the interned original label in the text pool.
compactTagCode :: TagID -> PoolM TagCode
compactTagCode = \case
  UnknownT rawLabel -> negate <$> internText rawLabel
  knownTag -> pure (fromIntegral (tagIdToInt knownTag))

--------------------------------------------------------------------------------
-- Optional helpers for later serialisation / inspection
--------------------------------------------------------------------------------

isUnknownTagCode :: TagCode -> Bool
isUnknownTagCode code = code < 0

unknownTagTextId :: TagCode -> Maybe TextID
unknownTagTextId code = if code < 0 then Just (abs code) else Nothing

lookupTextPool :: TextID -> TextPool -> Maybe Text
lookupTextPool = HM.lookup