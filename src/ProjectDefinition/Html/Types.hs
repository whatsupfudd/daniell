{-# LANGUAGE LambdaCase #-}
module ProjectDefinition.Html.Types where

import Data.Int (Int32)
import Data.HashMap.Strict (HashMap)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Vector (Vector)
import qualified Data.Vector as V



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
  case (T.toLower . T.strip) rawName of
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

data HtmlNodeC =
  NodeHC { 
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
