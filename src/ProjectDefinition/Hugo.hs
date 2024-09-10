{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use second" #-}
{-# HLINT ignore "Use bimap" #-}
{-# OPTIONS_GHC -Wno-overlapping-patterns #-}
module ProjectDefinition.Hugo where

import Control.Monad (foldM)

import qualified Data.ByteString as Bs
import Data.List (isPrefixOf)
import Data.Maybe (fromMaybe, isJust)
import qualified Data.Map as Mp
import Data.Text (Text, pack, unpack)
import qualified Data.Text.IO as T
import qualified Data.Vector as Vc

import System.FilePath ((</>), dropExtension)

import qualified Toml as Tm
import qualified Toml.Type.PrefixTree as Tpt
import qualified Data.HashMap.Strict as Hm
import qualified Data.List.NonEmpty as Nem
import qualified Data.Yaml as Ym
import qualified Data.Aeson as Ae
import qualified Data.Aeson.Types as Ae
import qualified Data.Aeson.KeyMap as Ae
import Data.Scientific (toRealFloat)

import Options.Runtime (RunOptions (..), TechOptions (..))
import Options.Types (HugoBuildOptions (..))
import Conclusion (GenError (..))
import qualified FileSystem.Types as Fs
import Generator.Types (WorkPlan (..), WorkItem)
import Markup.Types (MarkupPage (..), Content (..), ContentEncoding (..), FrontMatter (..), FMEncoding (..), Definition (..))
import Markup.Page (parseContent)

import ProjectDefinition.Types


{- For Hugo project, use archetype/* to create a new document in the content section -}


-- Analysis context for a Hugo project (configurations, etc) that will be used to drive generation.
data LocaleConfig = LanguageConfig {
    symbol :: Text
    , disabled :: Bool
    , languageCode :: Text
    , languageDirection :: Text
    , languageName :: Text
    , title :: Text
    , weight :: Integer
  }
  deriving Show

-- TODO: assert language code using the iso639 package.
defaultLocale = LanguageConfig {
    symbol = "en"
    , disabled = False
    , languageCode = ""
    , languageDirection = ""
    , languageName = ""
    , title = ""
    , weight = 0
  }

data AnalyzeContext = AnalyzeContext {
    globalVars :: Mp.Map Text DictEntry
    , defaultVars :: Mp.Map Text DictEntry
    , otherVars :: Mp.Map Text (Mp.Map Text DictEntry)
    , mergedConfigs :: Mp.Map Text DictEntry
  }
  deriving Show

defaultContext :: AnalyzeContext
defaultContext = AnalyzeContext {
    globalVars = Mp.empty
    , defaultVars = Mp.empty
    , otherVars = Mp.empty
    , mergedConfigs = Mp.empty
  }


data DictEntry =
  StringDV Text
  | IntDV Integer
  | DoubleDV Double
  | BoolDV Bool
  | DictDV (Mp.Map Text DictEntry)
  | ListDV [ DictEntry ]
  deriving Show


instance Ae.FromJSON DictEntry where
  parseJSON :: Ae.Value -> Ae.Parser DictEntry
  parseJSON val =
    case val of
      Ae.Array anArray -> ListDV <$> mapM Ae.parseJSON (Vc.toList anArray)
      Ae.String s -> pure $ StringDV s
      Ae.Number n -> pure $ DoubleDV $ toRealFloat n
      Ae.Bool b -> pure $ BoolDV b
      Ae.Null -> pure $ StringDV ""
      Ae.Object obj -> -- DictDV Mp.fromList <$> mapM (\(key, value) -> pure (pack . show $ key, value)) (Ae.toList obj)
        DictDV <$> Ae.parseJSON val


defaultComponents :: HugoComponents
defaultComponents = HugoComponents {
    templCore = HugoTemplCore {
      archetypes = []
      , assets = []
      , dataSet = []
      , i18n = []
      , layouts = []
      , resource = []
      , static = []
      , projConfig = []
      , miscs = []
    }
    , config = []
    , content = []
    , public = []
    , themes = Mp.empty
    , staticDest = "public"
  }


-- ***** Utilities for extracting command line options *****
getHugoOpts :: TechOptions -> Maybe HugoBuildOptions
getHugoOpts (HugoOptions options) = Just options
getHugoOpts _ = Nothing

extractOptions :: RunOptions -> HugoBuildOptions -> RunOptions
extractOptions rtOpts buildOpts =
  rtOpts { techOpts = HugoOptions buildOpts }


-- ***** Logic for dealing with a project work (create, build) *****

analyseProject :: RunOptions -> Fs.PathFiles -> IO (Either GenError WorkPlan)
analyseProject rtOpts pathFiles =
  case getHugoOpts rtOpts.techOpts of
    Nothing -> pure $ Left $ SimpleMsg "Not a Hugo project."
    Just hugoOpts ->
        let
          content = classifyContent (rtOpts, hugoOpts) pathFiles
        in
        analyseContent (rtOpts, hugoOpts) $ ProjectDefinition rtOpts.baseDir (Site (Hugo content)) [] pathFiles


classifyContent :: (RunOptions, HugoBuildOptions) -> Fs.PathFiles -> HugoComponents
classifyContent (rtOpts, hugoOpts) pathFiles =
  let
    fileSet = classifyFiles pathFiles
  in
    organizeFiles fileSet
  where
  classifyFiles :: Fs.PathFiles -> FileSet
  classifyFiles =
    foldl (\accum (dirPath, files) ->
            foldl (flip (classifyFile dirPath)) accum files
      ) (Mp.empty, [])
  classifyFile :: FilePath -> Fs.FileItem -> FileSet -> FileSet
  classifyFile dirPath aFile (fileDict, miscItems) =
    case aFile of
      Fs.KnownFile aKind aPath -> (Mp.insertWith (<>) aKind [(dirPath, aFile)] fileDict, miscItems)
      Fs.MiscFile aPath -> (fileDict, miscItems <> [ (dirPath, aFile) ])

organizeFiles :: FileSet -> HugoComponents
organizeFiles (knownFiles, miscFiles) =
  let
    (orgSet, themeSet) = Mp.foldl' (foldl (flip organizeAFile)) (Mp.empty, Mp.empty) knownFiles
  in
  HugoComponents {
    templCore = defineTemplCore orgSet
    , config = Mp.findWithDefault [] "config" orgSet
    , content = Mp.findWithDefault [] "content" orgSet
    , public = Mp.findWithDefault [] "public" orgSet
    , themes = Mp.map defineTemplCore themeSet
    , staticDest = "public"
  }
  where
  defineTemplCore :: OrgMap -> HugoTemplCore
  defineTemplCore orgSet = HugoTemplCore {
      archetypes = Mp.findWithDefault [] "archetypes" orgSet
      , assets = Mp.findWithDefault [] "assets" orgSet
      , dataSet = Mp.findWithDefault [] "data" orgSet
      , i18n = Mp.findWithDefault [] "i18n" orgSet
      , layouts = Mp.findWithDefault [] "layouts" orgSet
      , resource = Mp.findWithDefault [] "resource" orgSet
      , static = Mp.findWithDefault [] "static" orgSet
      , projConfig = Mp.findWithDefault [] "projConfig" orgSet
      , miscs = Mp.findWithDefault [] "miscs" orgSet
    }

  organizeAFile :: FileWithPath -> (OrgMap, Mp.Map String OrgMap) -> (OrgMap, Mp.Map String OrgMap)
  organizeAFile (dirPath, aFile) (orgMap, themeMap)
    | "config" `isPrefixOf` dirPath = (Mp.insertWith (<>) "config" [(drop 7 dirPath, aFile)] orgMap, themeMap)
    | "content" `isPrefixOf` dirPath = (Mp.insertWith (<>) "content" [(drop 8 dirPath, aFile)] orgMap, themeMap)
    | "public" `isPrefixOf` dirPath = (Mp.insertWith (<>) "public" [(drop 7 dirPath, aFile)] orgMap, themeMap)
    | "themes" `isPrefixOf` dirPath = (orgMap, organizeFileForTheme (drop 7 dirPath, aFile) themeMap)
    | otherwise = (organizeFileForTemplCore (dirPath, aFile) orgMap, themeMap)

  organizeFileForTemplCore :: FileWithPath -> OrgMap -> OrgMap
  organizeFileForTemplCore (dirPath, aFile) orgMap
    | dirPath == "" = case aFile of
            Fs.KnownFile aKind aPath ->
              case aKind of
                Fs.Toml -> Mp.insertWith (<>) "projConfig" [(dirPath, aFile)] orgMap
                Fs.Yaml -> Mp.insertWith (<>) "projConfig" [(dirPath, aFile)] orgMap
                Fs.Json -> Mp.insertWith (<>) "projConfig" [(dirPath, aFile)] orgMap
                _ -> Mp.insertWith (<>) "miscs" [(dirPath, aFile)] orgMap
            _ -> Mp.insertWith (<>) "miscs" [(dirPath, aFile)] orgMap
    | "archetypes" `isPrefixOf` dirPath = Mp.insertWith (<>) "archetypes" [(drop 11 dirPath, aFile)] orgMap
    | "assets" `isPrefixOf` dirPath = Mp.insertWith (<>) "assets" [(drop 7 dirPath, aFile)] orgMap
    | "data" `isPrefixOf` dirPath = Mp.insertWith (<>) "data" [(drop 5 dirPath, aFile)] orgMap
    | "i18n" `isPrefixOf` dirPath = Mp.insertWith (<>) "i18n" [(drop 5 dirPath, aFile)] orgMap
    | "layouts" `isPrefixOf` dirPath = Mp.insertWith (<>) "layouts" [(drop 8 dirPath, aFile)] orgMap
    | "resources" `isPrefixOf` dirPath = Mp.insertWith (<>) "resource" [(drop 10 dirPath, aFile)] orgMap
    | "static" `isPrefixOf` dirPath = Mp.insertWith (<>) "static" [(drop 7 dirPath, aFile)] orgMap
    | otherwise = Mp.insertWith (<>) "miscs" [(dirPath, aFile)] orgMap

  organizeFileForTheme :: FileWithPath -> Mp.Map String OrgMap -> Mp.Map String OrgMap
  organizeFileForTheme (dirPath, aFile) themeMap =
    let
      (themeName, filePath) = break (== '/') dirPath
      templCore = fromMaybe Mp.empty (Mp.lookup themeName themeMap)
      newTemplCore = organizeFileForTemplCore (drop 1 filePath, aFile) templCore
    in
    Mp.insert themeName newTemplCore themeMap


analyseContent :: (RunOptions, HugoBuildOptions) -> ProjectDefinition -> IO (Either GenError WorkPlan)
analyseContent (rtOpts, hugoOpts) (ProjectDefinition baseDir (Site (Hugo components)) [] pathFiles) =
  let
    globConfig = scanForGlobConfig hugoOpts.environment components.config
    markupFiles = extractMarkup components.content
    -- dbgContent = "NextJS Site project definition: " <> pack (show markupFiles)
  in do
  runConfigs <- analyzeConfig rtOpts globConfig
  case runConfigs of
    Left err -> pure $ Left err
    Right context -> do
      mrkpPages <- analyzeMarkups rtOpts context markupFiles
      case mrkpPages of
        Left err -> pure $ Left err
        Right mrkpPages ->
          let
            eiTheme = selectTheme hugoOpts context
          in
          case eiTheme of
            Left err -> pure $ Left err
            Right aLabel ->
              let
                mbProjLayout = if null components.templCore.layouts then Nothing else Just $ buildTemplateFromCore components.templCore
                mbTheme = case Mp.lookup (unpack aLabel) components.themes of
                  Nothing -> Nothing
                  Just aTmplCore -> Just $ buildTemplateFromCore aTmplCore
              in
              case (mbTheme, mbProjLayout) of
                (Nothing, Nothing) -> pure . Left . SimpleMsg $ "@[analyseContent] no theme directory (" <> aLabel <> ") nor site layout available."
                (_, _) -> do
                  putStrLn $ "@[analyseContent] theme: " <> show mbTheme
                  putStrLn $ "@[analyseContent] proj layout: " <> show mbProjLayout
                  case assignContentToTheme context components (mbProjLayout, mbTheme) mrkpPages of
                    Left err -> pure $ Left err
                    Right workPlan -> pure $ Right workPlan


-- TODO: get this working!
assignContentToTheme :: AnalyzeContext -> HugoComponents -> (Maybe Template, Maybe Template) ->[ MarkupPage ] -> Either GenError WorkPlan
assignContentToTheme context components (mbTheme, mbLayout) markupPages =
  let
    rezWork = foldM analyzePage [] markupPages
  in
  Left $ SimpleMsg "@[assignPagesToTheme] not implemented yet."
  where
  analyzePage :: [ WorkItem ] -> MarkupPage -> Either GenError [ WorkItem ]
  analyzePage accum aPage =
    if isMarkdown aPage.content.encoding then
      -- TODO: get this working!
      let
        mbLocale = (\fm -> case Mp.lookup "language" fm.fields of
              Just (ValueDF aLang) -> Just aLang
              Nothing -> Nothing
            )
            =<< aPage.frontMatter
      in
        Right []
    else
      Left . SimpleMsg $ "@[assignContentToTheme] markup encoding " <> pack (show aPage.content.encoding) <> " not supported yet."

  isMarkdown :: ContentEncoding -> Bool
  isMarkdown (ParsedMarkdown _) = True
  isMarkdown RawMarkdown = True
  isMarkdown _ = False


data Template = Template {
    core :: HugoTemplCore
    , config :: Mp.Map Text DictEntry
    , layout :: LayoutTmpl
  }
  deriving Show

type ThemeTmplPages = Mp.Map Text PageTmpl
type ThemeTmplMaps = Mp.Map Text ThemeTmplPages
data LayoutTmpl = LayoutTmpl {
    topLevel :: ThemeTmplPages
    , defaults :: ThemeTmplPages
    , kinds :: Mp.Map Text ThemeTmplPages
    , partials :: Mp.Map Text ThemeTmplPages
    , shortcodes :: ThemeTmplPages
  }
  deriving Show

data PageTmpl =
  FileRef FileWithPath
  | Compiled CompiledTemplate
  deriving Show

type CompiledTemplate = Bs.ByteString


buildTemplateFromCore :: HugoTemplCore -> Template
buildTemplateFromCore coreTmpl =
  let
    (topLevels, lo1) = findItems "" coreTmpl.layouts
    (defaults, lo2) = findItems "_default" lo1
    (shortcodes, lo3) = findItems "shortcodes" lo2
    (partials, lo4) = findSpecials "partials" lo3
    kindItems = findRemainingKinds lo4
  in
  Template {
    core = coreTmpl
    , config = Mp.empty
    , layout = LayoutTmpl {
        topLevel = topLevels
        , defaults = defaults
        , partials = partials
        , shortcodes = shortcodes
        , kinds = kindItems
    }
  }
  where
  findItems prefix = foldl (\(accum, leftover) i ->
      maybe (accum, i : leftover) (\(aPath, anItem) -> (Mp.insert aPath anItem accum, leftover)) $ matchLayout prefix i
    ) (Mp.empty, [])

  isValidLayout :: Fs.FileKind -> Bool
  isValidLayout aFile = aFile `elem` [Fs.Html, Fs.Rss, Fs.Xml, Fs.Json]

  matchLayout :: FilePath -> FileWithPath -> Maybe (Text, PageTmpl)
  matchLayout prefix item@(dirPath, aFile) =
    if dirPath == prefix then
      case aFile of
        Fs.KnownFile aKind aPath -> if isValidLayout aKind then
            Just (pack aPath, FileRef item)
          else
            Nothing
        _ -> Nothing
    else
      Nothing

  findSpecials :: FilePath -> [ FileWithPath ] -> (Mp.Map Text ThemeTmplPages, [ FileWithPath ])
  findSpecials prefix =
    foldl (\(accum, leftover) fileWithPath@(dirPath, fileItem) ->
      if prefix `isPrefixOf` dirPath  then
        let
          kind = pack $ drop (length prefix + 1) dirPath
        in
        case fileItem of
          Fs.KnownFile aKind aPath -> if isValidLayout aKind then
            (case Mp.lookup kind accum of
                Nothing -> Mp.insert kind (Mp.singleton (pack aPath) (FileRef fileWithPath)) accum
                Just kMap -> Mp.insert kind (Mp.insert (pack aPath) (FileRef fileWithPath) kMap) accum
              , leftover)
            else
              (accum, fileWithPath : leftover)
          _ -> (accum, fileWithPath : leftover)
      else
        (accum, fileWithPath : leftover)
    ) (Mp.empty, [])

  -- Warning: findRemainingKinds assumes that other special dirPath have been taken care of before.
  findRemainingKinds :: [ FileWithPath ] -> Mp.Map Text ThemeTmplPages
  findRemainingKinds =
    foldl (\accum fileWithPath@(dirPath, fileItem) ->
      let
        kind = pack dirPath
      in
      case fileItem of
        Fs.KnownFile aKind aPath -> if isValidLayout aKind then
          case Mp.lookup kind accum of
            Nothing -> Mp.insert kind (Mp.singleton (pack aPath) (FileRef fileWithPath)) accum
            Just kMap -> Mp.insert kind (Mp.insert (pack aPath) (FileRef fileWithPath) kMap) accum
          else
            accum
        _ -> accum
    ) Mp.empty

selectTheme :: HugoBuildOptions -> AnalyzeContext -> Either GenError Text
selectTheme options context =
  case options.theme of
    Just aTheme -> Right aTheme
    Nothing -> case Mp.lookup "theme" context.globalVars of
      Just (StringDV aTheme) -> Right aTheme
      _ -> case Mp.lookup "theme" context.defaultVars of
        Just (StringDV aTheme) -> Right aTheme
        _ -> Left $ SimpleMsg "@[selectTheme] Theme not found in configurations."


extractMarkup :: [ FileWithPath ] -> Mp.Map String [ FileWithPath ]
extractMarkup = foldl (\accum (dirPath, aFile) ->
    case aFile of
      Fs.KnownFile kind aPath ->
        if kind == Fs.Markdown then
          Mp.insertWith (<>) dirPath [(dirPath, aFile)] accum
        else
          accum
      _ -> accum
  ) Mp.empty


scanForGlobConfig :: Maybe Text -> [ FileWithPath ] -> (Maybe FileWithPath, Maybe FileWithPath, [ FileWithPath ])
scanForGlobConfig mbEnvironment = foldl (\accum@(topConfig, defaultConfig, otherConfigs) aFile ->
    case aFile of
      (dirPath, Fs.KnownFile kind filePath) ->
        if kind == Fs.Toml || kind == Fs.Yaml || kind == Fs.Json then
          if dirPath == "" && ("hugo." `isPrefixOf` filePath || "config." `isPrefixOf` filePath) then
            (Just aFile, defaultConfig, otherConfigs)
          else if dirPath == "_default" && ("hugo." `isPrefixOf` filePath || "config." `isPrefixOf` filePath) then
            (topConfig, Just aFile, otherConfigs)
          else
            (topConfig, defaultConfig, otherConfigs <> [ aFile ])
        else
          accum
      _ -> accum
  ) (Nothing, Nothing, [])
  -- Right $ WorkPlan { destDir = "", items = []}


-- TODO: use context locale configuration to include/exclude pages.
analyzeMarkups :: RunOptions -> AnalyzeContext -> Mp.Map String [ FileWithPath ] -> IO (Either GenError [ MarkupPage ])
analyzeMarkups rtOpts context markupFiles = do
  foldM (\accum (topPath, files) -> case accum of
      Left err -> pure $ Left err
      Right accumPages -> do
        groupPages <- foldM (\eiAccum aFile -> case eiAccum of
            Left err -> pure $ Left err
            Right somePages -> do
              eiMrkPage <- analyzeMarkupPage rtOpts rtOpts.baseDir topPath aFile
              case eiMrkPage of
                Left err -> pure $ Left err
                Right aPage -> pure $ Right $ somePages <> [ aPage ]
          ) (Right []) files
        case groupPages of
          Left err -> pure $ Left err
          Right morePages -> pure $ Right $ accumPages <> morePages
    ) (Right []) (Mp.toList markupFiles)


-- TODO: generalize for all markup types.
analyzeMarkupPage :: RunOptions -> FilePath -> FilePath -> FileWithPath -> IO (Either GenError MarkupPage)
analyzeMarkupPage rtOpts rootDir groupPath (aDirPath, Fs.KnownFile Fs.Markdown filePath) =
  let
    inProjPath = groupPath </> filePath
    fullPath = rootDir </> "content" </> inProjPath
  in do
  -- putStrLn $ "@[analyzeMarkupPage] analyzing: " <> fullPath
  parseContent rtOpts fullPath

analyzeMarkupPage rtOpts rootDir groupPath (aDirPath, Fs.KnownFile _ filePath) = do
  putStrLn $ "@[analyzeMarkupPage] error, trying to analyze a non-markup file: " <> groupPath </> filePath
  pure $ Left $ SimpleMsg "Trying to analyze a non-markup file."

analyzeMarkupPage rtOpts rootDir groupPath (aDirPath, Fs.MiscFile filePath) = do
  putStrLn $ "@[analyzeMarkupPage] error, trying to analyze a non-markup file: " <> groupPath </> filePath
  pure $ Left $ SimpleMsg "Trying to analyze a non-markup file."


analyzeConfig :: RunOptions -> (Maybe FileWithPath, Maybe FileWithPath, [ FileWithPath ]) -> IO (Either GenError AnalyzeContext)
analyzeConfig rtOpts (topConfig, defaultConfig, otherConfigs) =
  maybe (pure $ Right defaultContext) (parseTopConfig defaultContext rtOpts.baseDir) topConfig
        >>= \case
            Left err -> pure $ Left err
            Right ctxt -> maybe (pure $ Right ctxt) (parseDefaultConfig ctxt rtOpts.baseDir) defaultConfig
        >>= \case
            Left err -> pure $ Left err
            Right ctxt -> parseOtherConfigs ctxt rtOpts.baseDir otherConfigs
        >>= \case
            Left err -> pure $ Left err
            Right ctxt -> pure $ Right ctxt
        >>= \case
            Left err -> pure $ Left err
            Right ctxt -> pure $ mergeConfig ctxt
  where
  mergeConfig :: AnalyzeContext -> Either GenError AnalyzeContext
  mergeConfig context =
    Right context

  parseTopConfig :: AnalyzeContext -> FilePath -> FileWithPath -> IO (Either GenError AnalyzeContext)
  parseTopConfig context rootDir aFile = do
    eiRez <- parseConfigFile rootDir aFile
    case eiRez of
      Left err -> pure $ Left err
      Right aConfig -> pure $ Right context { globalVars = aConfig }

  parseDefaultConfig :: AnalyzeContext -> FilePath -> FileWithPath -> IO (Either GenError AnalyzeContext)
  parseDefaultConfig context rootDir aFile = do
    eiRez <- parseConfigFile rootDir aFile
    case eiRez of
      Left err -> pure $ Left err
      Right aConfig -> pure $ Right context { defaultVars = aConfig }

  parseOtherConfigs :: AnalyzeContext -> FilePath -> [ FileWithPath ] -> IO (Either GenError AnalyzeContext)
  parseOtherConfigs context rootDir otherConfigs = do
    -- putStrLn "Parsing other configs..."
    foldM (\accum aFile@(dirPath, aFileItem) -> case accum of
          Left err -> pure $ Left err
          Right aCtxt -> case aFileItem of
              Fs.KnownFile kind filePath ->
                if kind == Fs.Toml || kind == Fs.Yaml || kind == Fs.Json then do
                  eiRez <- parseConfigFile rootDir aFile
                  case eiRez of
                    Left err -> pure $ Left err
                    Right aConfig -> pure $ Right aCtxt { otherVars = Mp.insertWith (<>) (pack $ dropExtension filePath) aConfig aCtxt.otherVars }
                else
                  pure $ Right aCtxt
              _ -> pure $ Left $ SimpleMsg "Misc file in other configs."
        ) (Right context) otherConfigs


parseConfigFile :: FilePath -> FileWithPath -> IO (Either GenError (Mp.Map Text DictEntry))
parseConfigFile rootDir (dirPath, Fs.KnownFile kind filePath) = do
  let
    fullPath = rootDir </> "config" </> dirPath </> filePath
  -- putStrLn $ "@[parseDefaultConfig] reading: " <> fullPath
  -- putStrLn $ "@[parseDefaultConfig] analyzing: " <> fullPath
  case kind of
    Fs.Toml -> do
      srcText <- T.readFile fullPath
      case Tm.parse srcText of
        Left err -> do
          putStrLn $ "@[parseDefaultConfig] error parsing TOML: " <> show err
          pure . Left $ SimpleMsg ("TOML err: " <> pack (show err))
        Right tomlVal ->
          -- putStrLn $ "@[parseDefaultConfig] parsed TOML: " <> show tomlVal
          pure $ case tomlToDict tomlVal of
            Left err -> Left $ SimpleMsg ("TOML conversion err: " <> pack err)
            Right aDict -> Right aDict
    Fs.Yaml -> do
      eiValue <- Ym.decodeFileEither fullPath :: IO (Either Ym.ParseException DictEntry)
      case eiValue of
        Left err ->
          pure . Left . SimpleMsg $ "@[parseDefaultConfig] error in file " <> pack fullPath <> ", YAML err: " <> pack (show err)
        Right aVal ->
          case aVal of
            DictDV aDict -> pure $ Right aDict
            _ ->
              pure . Left . SimpleMsg $ "@[parseDefaultConfig] error in file " <> pack fullPath <> ", YAML root is not a dictionary."
    Fs.Json -> do
      putStrLn "@[parseDefaultConfig] parsing JSON..."
      eiJsonRez <- Ae.eitherDecodeFileStrict' fullPath :: IO (Either String Ae.Object)
      case eiJsonRez of
        Left err -> do
          putStrLn $ "@[parseDefaultConfig] error parsing JSON: " <> err
          pure . Left $ SimpleMsg ("JSON err: " <> pack err)
        Right jsonVal -> do
          putStrLn $ "@[parseDefaultConfig] parsed JSON: " <> show jsonVal
          pure . Left $ SimpleMsg "JSON parsing not implemented yet."

parseConfigFile rootDir (dirPath, Fs.MiscFile aPath) = do
  putStrLn "@[parseConfigFile] misc file!"
  pure . Left . SimpleMsg $ "@[parseConfigFile] error, trying to parse misc file " <> pack aPath


tomlToDict :: Tm.TOML -> Either String (Mp.Map Text DictEntry)
tomlToDict tomlBlock =
  let
    eiPairs = foldM (\accum (fKey Tm.:|| rKey, value)-> case tomlToValue value of
          Left err -> Left $ "@[tomlToDict] pair error: " <> show err
          Right aVal -> Right $ Mp.insert fKey.unPiece aVal accum
        ) Mp.empty $ Hm.toList tomlBlock.tomlPairs
    eiTables =
      case eiPairs of
        Left _ -> eiPairs
        Right pairs -> foldM (\accum (fKey Tm.:|| rKey, value) -> case tomlToDict value of
              Left err -> Left $ "@[tomlToDict] table error: " <> err
              Right aVal -> Right $ Mp.insert fKey.unPiece (DictDV aVal) accum
          ) pairs (Tpt.toList tomlBlock.tomlTables)
  in
  case eiTables of
    Left err -> eiTables
    Right tables -> foldM (\accum (fKey Tm.:|| rKey, arrayVal) ->
        let
          mbExistingArray = Mp.lookup fKey.unPiece accum
          newValues = mapM tomlToDict (Nem.toList arrayVal)
        in
        case newValues of
          Left err -> Left $ "@[tomlToDict] table array conversion error: " <> err
          Right anArray ->
            case mbExistingArray of
              Nothing -> Right $ Mp.insert fKey.unPiece (ListDV (map DictDV anArray)) accum
              Just (ListDV existingArray) -> Right $ Mp.insert fKey.unPiece (ListDV $ existingArray <> map DictDV anArray) accum
              _ -> Left $ "@[tomlToDict] array error, key " <> unpack fKey.unPiece <> " already exists for a non-array value."
      ) tables (Hm.toList tomlBlock.tomlTableArrays)
  where
  tomlToValue :: Tm.AnyValue -> Either Tm.MatchError DictEntry
  tomlToValue (Tm.AnyValue v) =
    case Tm.matchBool v of
      Right b -> Right $ BoolDV b
      Left _ -> case Tm.matchText v of
        Right s -> Right $ StringDV s
        Left _ -> case Tm.matchInteger v of
          Right i -> Right $ IntDV i
          Left _ -> case Tm.matchDouble v of
            Right d -> Right $ DoubleDV d
            Left _ -> case Tm.matchArray tomlToValue v of
                Right aArray -> Right $ ListDV aArray
                Left _ -> Tm.mkMatchError Tm.TText (Tm.Text $ "@[tomlToValue] unknown value type: " <> pack (show v))

validConfigKeys :: [ Text ]
validConfigKeys = [
    "archetypeDir", "assetDir", "baseURL", "build", "buildDrafts", "buildExpired", "buildFuture", "caches"
  , "canonifyURLs", "capitalizeListTitles", "cascade", "cleanDestinationDir", "contentDir", "copyright", "dataDir"
  , "defaultContentLanguage", "defaultContentLanguageInSubdir", "disableAliases", "disableHugoGeneratorInject", "disableKinds"
  , "disableLanguages", "disableLiveReload", "disablePathToLower", "enableEmoji", "enableGitInfo"
  , "enableMissingTranslationPlaceholders", "enableRobotsTXT", "environment", "frontmatter", "hasCJKLanguage", "ignoreCache"
  , "ignoreLogs", "ignoreVendorPaths", "imaging", "languageCode", "languages", "layoutDir", "markup", "mediaTypes", "menus"
  , "minify", "module", "newContentEditor", "noBuildLock", "noChmod", "noTimes", "outputFormats", "page", "pagination"
  , "panicOnWarning", "permalinks", "pluralizeListTitles", "printI18nWarnings", "printPathWarnings", "printUnusedTemplates"
  , "publishDir", "refLinksErrorLevel", "refLinksNotFoundURL", "related", "relativeURLs", "removePathAccents", "renderSegments"
  , "sectionPagesMenu", "security", "segments", "sitemap", "summaryLength", "taxonomies", "templateMetrics", "templateMetricsHints"
  , "theme", "themesDir", "timeout", "timeZone", "title", "titleCaseStyle", "uglyURLs", "watch"
  ]

{-
  - config folder: contains sections of configuration, _default and others (eg 'production).
      The main config should be 'hugo.<ext>', a change in v0.109.0 from 'config.<ext>' (still supported).
    , content :: [ FileWithPath ]
    , public :: [ FileWithPath ]
    , themes :: ThemeMap
    , templCore :: HugoTemplCore
    , staticDest :: FilePath

    HugoTemplCore {
      archetypes :: [ FileWithPath ]
      , assets :: [ FileWithPath ]
      , dataSet :: [ FileWithPath ]
      , i18n :: [ FileWithPath ]
      , layouts :: [ FileWithPath ]
      , resource :: [ FileWithPath ]
      , static :: [ FileWithPath ]
      , projConfig :: [ FileWithPath ]
      , miscs :: [ FileWithPath ]
    }

- root configuration keys: build, caches, cascade, deployment, frontmatter, imaging, languages, markup, mediatypes, menus, minify, module, outputformats, outputs, params, permalinks, privacy, related, security, segments, server, services, sitemap, taxonomies.
  Each one can become a separate config file (eg build.toml, params.toml).
  Subfolders (eg config/staging/hugo.toml) will be used in conjunction with the --environment param (or HUGO_ENVIRONMENT env var).
  Using the 'server' command sets the default environment to 'development'; the 'build' command (just 'hugo' in gohugo) set the default
  environment to 'production'.

- Config env vars: DART_SASS_BINARY, HUGO_ENVIRONMENT / HUGO_ENV, HUGO_FILE_LOG_FORMAT, HUGO_MEMORYLIMIT, HUGO_NUMWORKERMULTIPLIER, HUGO_ENABLEGITINFO
    , HUGO<sep><key>, HUGO<sep>PARAMS<sep><key> where <sep> is any allowed delimiter (not '=', not NUL, more or less [a-zA-Z_]+[a-zA-Z0-9_]*).

* next and previous pages are sorted in a page collection according to weight, date, linkTitle, path (in that order). The sort order can be changed with:
page:
  nextPrevInSectionSortOrder: desc
  nextPrevSortOrder: desc


* build defaults:
build:
  buildStats:
    disableClasses: false
    disableIDs: false
    disableTags: false
    enable: false
  cacheBusters:
  - source: (postcss|tailwind)\.config\.js
    target: (css|styles|scss|sass)
  noJSConfigInAssets: false
  useResourceCacheWhen: fallback


* Front-Matter dates default:
frontmatter:
  date:
  - date
  - publishdate
  - pubdate
  - published
  - lastmod
  - modified
  expiryDate:
  - expirydate
  - unpublishdate
  lastmod:
  - :git
  - lastmod
  - modified
  - date
  - publishdate
  - pubdate
  - published
  publishDate:
  - publishdate
  - pubdate
  - published
  - date
* special values:
 :default
 :fileModTime
 :filename
 :git

* minify defaults:
minify:
  disableCSS: false
  disableHTML: false
  disableJS: false
  disableJSON: false
  disableSVG: false
  disableXML: false
  minifyOutput: false
  tdewolff:
    css:
      inline: false
      keepCSS2: true
      precision: 0
    html:
      keepComments: false
      keepConditionalComments: false
      keepDefaultAttrVals: true
      keepDocumentTags: true
      keepEndTags: true
      keepQuotes: false
      keepSpecialComments: true
      keepWhitespace: false
      templateDelims:
      - ""
      - ""
    js:
      keepVarNames: false
      precision: 0
      version: 2022
    json:
      keepNumbers: false
      precision: 0
    svg:
      inline: false
      keepComments: false
      precision: 0
    xml:
      keepWhitespace: false

* file caches:
caches:
  assets:
    dir: :resourceDir/_gen
    maxAge: -1
  getcsv:
    dir: :cacheDir/:project
    maxAge: -1
  getjson:
    dir: :cacheDir/:project
    maxAge: -1
  getresource:
    dir: :cacheDir/:project
    maxAge: -1
  images:
    dir: :resourceDir/_gen
    maxAge: -1
  misc:
    dir: :cacheDir/:project
    maxAge: -1
  modules:
    dir: :cacheDir/modules
    maxAge: -1

* http cache:
HTTPCache:
  cache:
    for:
      excludes:
      - '**'
      includes: null
  polls:
  - disable: true
    for:
      excludes: null
      includes:
      - '**'
    high: 0s
    low: 0s

In 'server' mode, the 'server.{toml, yaml, json}' configures server options, with params matching
 https://docs.netlify.com/routing/headers/#syntax-for-the-netlify-configuration-file, using https://github.com/gobwas/glob for globbing:
server:
  headers:
  - for: /**
    values:
      Content-Security-Policy: script-src localhost:1313
      Referrer-Policy: strict-origin-when-cross-origin
      X-Content-Type-Options: nosniff
      X-Frame-Options: DENY
      X-XSS-Protection: 1; mode=block

or:
redirects:
- force: false
  from: /myspa/**
  status: 200
  to: /myspa/


* config default merge settings:
build:
  _merge: none
caches:
  _merge: none
cascade:
  _merge: none
deployment:
  _merge: none
frontmatter:
  _merge: none
httpcache:
  _merge: none
imaging:
  _merge: none
languages:
  _merge: none
  en:
    _merge: none
    menus:
      _merge: shallow
    params:
      _merge: deep
markup:
  _merge: none
mediatypes:
  _merge: shallow
menus:
  _merge: shallow
minify:
  _merge: none
module:
  _merge: none
outputformats:
  _merge: shallow
outputs:
  _merge: none
page:
  _merge: none
pagination:
  _merge: none
params:
  _merge: deep
permalinks:
  _merge: none
privacy:
  _merge: none
related:
  _merge: none
security:
  _merge: none
segments:
  _merge: none
server:
  _merge: none
services:
  _merge: none
sitemap:
  _merge: none
taxonomies:
  _merge: none


The entire set of config values are:
archetypeDir 
(string) The directory where Hugo finds archetype files (content templates). Default is archetypes. Also see Module Mounts Config for an alternative way to configure this directory (from Hugo 0.56).

assetDir 
(string) The directory where Hugo finds asset files used in Hugo Pipes. Default is assets. Also see Module Mounts Config for an alternative way to configure this directory (from Hugo 0.56).

baseURL 
(string) The absolute URL (protocol, host, path, and trailing slash) of your published site (e.g., https://www.example.org/docs/).

build 
See Configure Build.

buildDrafts 
(bool) Include drafts when building. Default is false.

buildExpired 
(bool) Include content already expired. Default is false.

buildFuture 
(bool) Include content with a future publication date. Default is false.

caches 
See Configure File Caches.

canonifyURLs 
(bool) See details before enabling this feature. Default is false.

capitalizeListTitles 
New in v0.123.3
(bool) Whether to capitalize automatic list titles. Applicable to section, taxonomy, and term pages. Default is true. You can change the capitalization style in your site configuration to one of ap, chicago, go, firstupper, or none. See details.

cascade 
Pass down default configuration values (front matter) to pages in the content tree. The options in site config is the same as in page front matter, see Front Matter Cascade.

For a website in a single language, define the [[cascade]] in Front Matter. For a multilingual website, define the [[cascade]] in Site Config.

To remain consistent and prevent unexpected behavior, do not mix these strategies.

cleanDestinationDir 
(bool) When building, removes files from destination not found in static directories. Default is false.

contentDir 
(string) The directory from where Hugo reads content files. Default is content. Also see Module Mounts Config for an alternative way to configure this directory (from Hugo 0.56).

copyright 
(string) Copyright notice for your site, typically displayed in the footer.

dataDir 
(string) The directory from where Hugo reads data files. Default is data. Also see Module Mounts Config for an alternative way to configure this directory (from Hugo 0.56).

defaultContentLanguage 
(string) Content without language indicator will default to this language. Default is en.

defaultContentLanguageInSubdir 
(bool) Render the default content language in subdir, e.g. content/en/. The site root / will then redirect to /en/. Default is false.

disableAliases 
(bool) Will disable generation of alias redirects. Note that even if disableAliases is set, the aliases themselves are preserved on the page. The motivation with this is to be able to generate 301 redirects in an .htaccess, a Netlify _redirects file or similar using a custom output format. Default is false.

disableHugoGeneratorInject 
(bool) Hugo will, by default, inject a generator meta tag in the HTML head on the home page only. You can turn it off, but we would really appreciate if you don’t, as this is a good way to watch Hugo’s popularity on the rise. Default is false.

disableKinds 
(string slice) Disable rendering of the specified page kinds, any of 404, home, page, robotstxt, rss, section, sitemap, taxonomy, or term.

disableLanguages 
See disable a language.

disableLiveReload 
(bool) Disable automatic live reloading of browser window. Default is false.

disablePathToLower 
(bool) Do not convert the url/path to lowercase. Default is false.

enableEmoji 
(bool) Enable Emoji emoticons support for page content; see the emoji shortcode quick reference guide. Default is false.

enableGitInfo 
(bool) Enable .GitInfo object for each page (if the Hugo site is versioned by Git). This will then update the Lastmod parameter for each page using the last git commit date for that content file. Default is false.

enableMissingTranslationPlaceholders 
(bool) Show a placeholder instead of the default value or an empty string if a translation is missing. Default is false.

enableRobotsTXT 
(bool) Enable generation of robots.txt file. Default is false.

environment 
(string) Build environment. Default is production when running hugo and development when running hugo server. See Configuration directory.

frontmatter 
See Front matter Configuration.

hasCJKLanguage 
(bool) If true, auto-detect Chinese/Japanese/Korean Languages in the content. This will make .Summary and .WordCount behave correctly for CJK languages. Default is false.

ignoreCache 
(bool) Ignore the cache directory. Default is false.

ignoreLogs 
(string slice) A slice of message identifiers corresponding to warnings and errors you wish to suppress. See erroridf and warnidf.

ignoreVendorPaths 
(string) Ignore vendored modules that match the given Glob pattern within the _vendor directory.

imaging 
See image processing configuration.

languageCode 
(string) A language tag as defined by RFC 5646. This value is used to populate:

The <language> element in the embedded RSS template
The lang attribute of the <html> element in the embedded alias template
The og:locale meta element in the embedded Open Graph template
When present in the root of the configuration, this value is ignored if one or more language keys exists. Please specify this value independently for each language key.

languages 
See Configure Languages.

layoutDir 
(string) The directory that contains templates. Default is layouts.

markup 
See Configure Markup.

mediaTypes 
See Configure Media Types.

menus 
See Menus.

minify 
See Configure Minify.

module 
Module configuration see module configuration.

newContentEditor 
(string) The editor to use when creating new content.

noBuildLock 
(bool) Don’t create .hugo_build.lock file. Default is false.

noChmod 
(bool) Don’t sync permission mode of files. Default is false.

noTimes 
(bool) Don’t sync modification time of files. Default is false.

outputFormats 
See custom output formats.

page 
See configure page.

pagination 
See configure pagination.

panicOnWarning 
(bool) Whether to panic on first WARNING. Default is false.

permalinks 
See Content Management.

pluralizeListTitles 
(bool) Whether to pluralize automatic list titles. Applicable to section pages. Default is true.

printI18nWarnings 
(bool) Whether to log WARNINGs for each missing translation. Default is false.

printPathWarnings 
(bool) Whether to log WARNINGs when Hugo publishes two or more files to the same path. Default is false.

printUnusedTemplates 
(bool) Whether to log WARNINGs for each unused template. Default is false.

publishDir 
(string) The directory to where Hugo will write the final static site (the HTML files etc.). Default is public.

refLinksErrorLevel 
(string) When using ref or relref to resolve page links and a link cannot be resolved, it will be logged with this log level. Valid values are ERROR (default) or WARNING. Any ERROR will fail the build (exit -1). Default is ERROR.

refLinksNotFoundURL 
(string) URL to be used as a placeholder when a page reference cannot be found in ref or relref. Is used as-is.

related 
See Related Content.

relativeURLs 
(bool) See details before enabling this feature. Default is false.

removePathAccents 
(bool) Removes non-spacing marks from composite characters in content paths. Default is false.

content/post/hügó.md → https://example.org/post/hugo/
renderSegments 
New in v0.124.0
(string slice) A list of segments to render. If not set, everything will be rendered. This is more commonly set in a CLI flag, e.g. hugo --renderSegments segment1,segment2. The segment names must match the names in the segments configuration.

sectionPagesMenu 
See Menus.

security 
See Security Policy.

segments 
See Segments.

sitemap 
Default sitemap configuration.

summaryLength 
(int) Applicable to automatic summaries, the minimum number of words to render when calling the Summary method on a Page object. In this case the Summary method returns the content, truncated to the paragraph closest to the summaryLength.

taxonomies 
See Configure Taxonomies.

templateMetrics 
(bool) Whether to print template execution metrics to the console. Default is false. See Template metrics.

templateMetricsHints 
(bool) Whether to print template execution improvement hints to the console. Applicable when templateMetrics is true. Default is false. See Template metrics.

theme 
See module configuration for how to import a theme.

themesDir 
(string) The directory where Hugo reads the themes from. Default is themes.

timeout 
(string) Timeout for generating page contents, specified as a duration or in seconds. Note: this is used to bail out of recursive content generation. You might need to raise this limit if your pages are slow to generate (e.g., because they require large image processing or depend on remote contents). Default is 30s.

timeZone 
(string) The time zone (or location), e.g. Europe/Oslo, used to parse front matter dates without such information and in the time function. The list of valid values may be system dependent, but should include UTC, Local, and any location in the IANA Time Zone database.

title 
(string) Site title.

titleCaseStyle 
(string) Default is ap. See Configure Title Case.

uglyURLs 
(bool) When enabled, creates URL of the form /filename.html instead of /filename/. Default is false.

watch 
(bool) Watch filesystem for changes and recreate as needed. Default is false.
-}
