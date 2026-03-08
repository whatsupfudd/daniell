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

--------------------------------------------------------------------------------
-- Public document model
--------------------------------------------------------------------------------

data HtmlDocument = HtmlDocument { 
    docHeader :: !Text
  , dom :: !(Vector HtmlNode)
  }
  deriving (Eq, Show)

data HtmlNode
  = NodeH { 
        tagID :: !TagID
      , attributes :: !(Vector Attribute)
      , children :: !(Vector HtmlNode)
      }
  | TextEntry !Text
  | CommentEntry !Text
  deriving (Eq, Show)

data Attribute = Attribute { 
    name :: !Text
  , value :: !Text
  }
  deriving (Eq, Show)

-- Extend this as needed. Keep UnknownT for forwards compatibility.
data TagID
  = HtmlT
  | HeadT
  | TitleT
  | BodyT
  | MetaT
  | LinkT
  | ScriptT
  | StyleT
  | DivT
  | SpanT
  | PT
  | AT
  | ImgT
  | BrT
  | HrT
  | InputT
  | FormT
  | ButtonT
  | LabelT
  | SelectT
  | OptionT
  | TextareaT
  | UlT
  | OlT
  | LiT
  | TableT
  | TheadT
  | TbodyT
  | TfootT
  | TrT
  | TdT
  | ThT
  | SectionT
  | ArticleT
  | HeaderT
  | FooterT
  | MainT
  | NavT
  | AsideT
  | HT1
  | HT2
  | HT3
  | HT4
  | HT5
  | HT6
  | StrongT
  | EmT
  | SmallT
  | PreT
  | CodeT
  | UnknownT !Text
  deriving (Eq, Ord, Show)

tagIdToInt :: TagID -> Int
tagIdToInt = \case
  HtmlT      -> 1
  HeadT      -> 2
  TitleT     -> 3
  BodyT      -> 4
  MetaT      -> 5
  LinkT      -> 6
  ScriptT    -> 7
  StyleT     -> 8
  DivT       -> 9
  SpanT      -> 10
  PT         -> 11
  AT         -> 12
  ImgT       -> 13
  BrT        -> 14
  HrT        -> 15
  InputT     -> 16
  FormT      -> 17
  ButtonT    -> 18
  LabelT     -> 19
  SelectT    -> 20
  OptionT    -> 21
  TextareaT  -> 22
  UlT        -> 23
  OlT        -> 24
  LiT        -> 25
  TableT     -> 26
  TheadT     -> 27
  TbodyT     -> 28
  TfootT     -> 29
  TrT        -> 30
  TdT        -> 31
  ThT        -> 32
  SectionT   -> 33
  ArticleT   -> 34
  HeaderT    -> 35
  FooterT    -> 36
  MainT      -> 37
  NavT       -> 38
  AsideT     -> 39
  HT1        -> 40
  HT2        -> 41
  HT3        -> 42
  HT4        -> 43
  HT5        -> 44
  HT6        -> 45
  StrongT    -> 46
  EmT        -> 47
  SmallT     -> 48
  PreT       -> 49
  CodeT      -> 50
  UnknownT _ -> 1000

labelToTagId :: Text -> TagID
labelToTagId rawName =
  case normalizeTagName rawName of
    "html"     -> HtmlT
    "head"     -> HeadT
    "title"    -> TitleT
    "body"     -> BodyT
    "meta"     -> MetaT
    "link"     -> LinkT
    "script"   -> ScriptT
    "style"    -> StyleT
    "div"      -> DivT
    "span"     -> SpanT
    "p"        -> PT
    "a"        -> AT
    "img"      -> ImgT
    "br"       -> BrT
    "hr"       -> HrT
    "input"    -> InputT
    "form"     -> FormT
    "button"   -> ButtonT
    "label"    -> LabelT
    "select"   -> SelectT
    "option"   -> OptionT
    "textarea" -> TextareaT
    "ul"       -> UlT
    "ol"       -> OlT
    "li"       -> LiT
    "table"    -> TableT
    "thead"    -> TheadT
    "tbody"    -> TbodyT
    "tfoot"    -> TfootT
    "tr"       -> TrT
    "td"       -> TdT
    "th"       -> ThT
    "section"  -> SectionT
    "article"  -> ArticleT
    "header"   -> HeaderT
    "footer"   -> FooterT
    "main"     -> MainT
    "nav"      -> NavT
    "aside"    -> AsideT
    "h1"       -> HT1
    "h2"       -> HT2
    "h3"       -> HT3
    "h4"       -> HT4
    "h5"       -> HT5
    "h6"       -> HT6
    "strong"   -> StrongT
    "em"       -> EmT
    "small"    -> SmallT
    "pre"      -> PreT
    "code"     -> CodeT
    other      -> UnknownT other

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
  let wanted = normalizeTagName rawName
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

normalizeTagName :: Text -> Text
normalizeTagName = T.toLower . T.strip

tagIdName :: TagID -> Text
tagIdName = \case
  HtmlT      -> "html"
  HeadT      -> "head"
  TitleT     -> "title"
  BodyT      -> "body"
  MetaT      -> "meta"
  LinkT      -> "link"
  ScriptT    -> "script"
  StyleT     -> "style"
  DivT       -> "div"
  SpanT      -> "span"
  PT         -> "p"
  AT         -> "a"
  ImgT       -> "img"
  BrT        -> "br"
  HrT        -> "hr"
  InputT     -> "input"
  FormT      -> "form"
  ButtonT    -> "button"
  LabelT     -> "label"
  SelectT    -> "select"
  OptionT    -> "option"
  TextareaT  -> "textarea"
  UlT        -> "ul"
  OlT        -> "ol"
  LiT        -> "li"
  TableT     -> "table"
  TheadT     -> "thead"
  TbodyT     -> "tbody"
  TfootT     -> "tfoot"
  TrT        -> "tr"
  TdT        -> "td"
  ThT        -> "th"
  SectionT   -> "section"
  ArticleT   -> "article"
  HeaderT    -> "header"
  FooterT    -> "footer"
  MainT      -> "main"
  NavT       -> "nav"
  AsideT     -> "aside"
  HT1        -> "h1"
  HT2        -> "h2"
  HT3        -> "h3"
  HT4        -> "h4"
  HT5        -> "h5"
  HT6        -> "h6"
  StrongT    -> "strong"
  EmT        -> "em"
  SmallT     -> "small"
  PreT       -> "pre"
  CodeT      -> "code"
  UnknownT t -> t

isVoidTagName :: Text -> Bool
isVoidTagName rawName =
  normalizeTagName rawName `elem`
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


{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- Add these imports to Cannelle.HTML.Document

--------------------------------------------------------------------------------
-- Compact / serialisation-oriented representation
--------------------------------------------------------------------------------

type TextID = Int32

-- | Positive values are known tags (via tagIdToInt).
-- | Negative values encode unknown tag names:
--     tagCode < 0  ==>  abs tagCode == TextID of the unknown tag label
type TagCode = Int32
type TextPool = HashMap Int32 Text


data HtmlDocumentC = HtmlDocumentC { 
    docHeaderC :: !TextID
  , domC :: !(Vector HtmlNodeC)
  }
  deriving (Eq, Show)

data HtmlNodeC = NodeHC { 
        tagIDC :: !TagCode
      , attributesC :: !(Vector AttributeC)
      , childrenC :: !(Vector HtmlNodeC)
      }
  | TextEntryC !TextID
  | CommentEntryC !TextID
  deriving (Eq, Show)

data AttributeC = AttributeC { 
    nameC  :: !TextID
  , valueC :: !TextID
  }
  deriving (Eq, Show)

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