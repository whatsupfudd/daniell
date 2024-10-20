{-# LANGUAGE MultiParamTypeClasses #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# OPTIONS_GHC -Wno-overlapping-patterns #-}
{-# HLINT ignore "Use second" #-}
{-# HLINT ignore "Use bimap" #-}
{-# HLINT ignore "Use list comprehension" #-}

module ProjectDefinition.Hugo where

import Control.Monad (foldM)
import Control.Exception (try)

import qualified Data.ByteString as Bs
import Data.Int (Int32)
import Data.List (isPrefixOf)
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.Maybe (fromMaybe, isJust, isNothing)
import qualified Data.Map as Mp
import Data.Text (Text, pack, unpack)
import qualified Data.Text.Encoding as T
import qualified Data.Vector as Vc

import System.FilePath ((</>), splitDirectories)

import Data.LanguageCodes (fromChars)

import qualified Cannelle.Hugo.Parse as Cnl
import qualified Cannelle.Hugo.AST as Cnl

import Options.Runtime (RunOptions (..), TechOptions (..))
import Options.Types (HugoBuildOptions (..))
import Conclusion (GenError (..))
import qualified FileSystem.Types as Fs
import FileSystem.Types (FileWithPath)
import Generator.Types (ExecSystem (..), WorkPlan (..))
import Markup.Types (MarkupPage (..), Content (..), ContentEncoding (..), FrontMatter (..), FMEncoding (..), Definition (..))
import Markup.Page (parseContent)
import ProjectDefinition.Paraml (tomlToDict)

import ProjectDefinition.Types
import ProjectDefinition.Hugo.Config
import ProjectDefinition.Hugo.Types
import qualified Cannelle.VM.Context as VM

import Cannelle.Hugo.Types (CompContext (..), CompFunction (..), CompConstant (..))
import Cannelle.Hugo.Assembler (assemble, convertCompCteToTempl)
import Cannelle.Hugo.Compiler (compileStatements, FullCompContext (..))
import Cannelle.Hugo.AST (RawStatement (..))
import qualified Cannelle.VM.Engine as VM
import Cannelle.VM.Context (ConstantValue)

{- For Hugo project, use archetype/* to create a new document in the content section -}


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


type HgWorkPlan = WorkPlan HgEngine HgContext HgWorkItem

newtype HgEngine = HgEngine { hugoOpts :: HugoBuildOptions }
  deriving Show
data HgContext = HgContext {
      mergedConfigs :: Mp.Map Text DictEntry
    , pathPrefixes :: Mp.Map Int32 FilePath
    , compTemplates :: Mp.Map FilePath [RawStatement]
  }
  deriving Show

data HgWorkItem =
  ExecTmplForContent CTPair
  | ExecTmplForRoute { dirPath :: FilePath }

data CTPair = CTPair {
    kind :: Fs.FileKind
    , contentPrefix :: Int32
    , section :: FilePath
    , content :: Fs.FileItem
    , themePrefix :: Int32
    , template :: FilePath
  }
  deriving Show

instance Show HgWorkItem where
  show wi = case wi of
    ExecTmplForContent ctp -> "ExecTmplForContent> dirPath: " <> ctp.section <> ", content" <> show ctp.content <> ", template: " <> ctp.template
    ExecTmplForRoute aDirPath -> "ExecTmplForRoute> dirPath: " <> aDirPath

instance ExecSystem HgEngine HgContext HgWorkItem where
  runWorkItem rtOpts engine context wItem = do
    case wItem of
      ExecTmplForRoute dirPath -> do
        putStrLn $ "@[runWorkItem.HgEngine] src: " <> show wItem
        pure $ Right context
      ExecTmplForContent ctp ->
        let
          contentPath = context.pathPrefixes Mp.! ctp.contentPrefix
          themePath = context.pathPrefixes Mp.! ctp.themePrefix
          mkPath = contentPath </> ctp.section </> Fs.getItemPath ctp.content
          tmplPath = themePath </> ctp.template
        in do
        putStrLn $ "@[runWorkItem.HgEngine] content: " <> mkPath <> ", template: " <> tmplPath
        case Mp.lookup tmplPath context.compTemplates of
          Nothing -> do
            eiTemplSource <- try $ Bs.readFile tmplPath :: IO (Either IOError Bs.ByteString)
            case eiTemplSource of
              Left err ->
                let
                  errMsg =  "@[runWorkItem.HgEngine] readFile err: " <> show err
                in do
                putStrLn errMsg
                pure . Left . SimpleMsg $ pack errMsg
              Right templSource -> do
                rezB <- Cnl.parseTemplateSource (Just tmplPath) templSource
                case rezB of
                  Left errMsg ->
                    let
                      nErrMsg = "@[runWorkItem.HgEngine] parseTemplateSource err: " <> errMsg
                    in do
                    putStrLn nErrMsg
                    pure . Left . SimpleMsg $ pack nErrMsg
                  Right templateElements -> case Cnl.convertElements templateElements of
                    Left errMsg ->
                      let
                        nErrMsg = "@[runWorkItem.HgEngine] convertElements err: " <> errMsg
                      in do
                      putStrLn nErrMsg
                      pure . Left . SimpleMsg $ pack nErrMsg
                    Right rezC -> do
                      putStrLn "@[runWorkItem.HgEngine] convertElements: "
                      Cnl.showStatements rezC
                      case compileStatements "$main" rezC of
                        Left err -> do
                          putStrLn $ "@[runWorkItem.HgEngine] compileCode err: " <> show err
                          pure . Left . SimpleMsg $ "@[runWorkItem.HgEngine] compileCode err: " <> pack (show err)
                        Right compiledCode -> do
                          putStrLn $ "@[runWorkItem.HgEngine] compileCode: " <> show compiledCode
                          let
                            vmModule = VM.VMModule {
                                functions = Vc.fromList . map (\(compFct, fID) -> 
                                    VM.FunctionDef {
                                      moduleID = 0
                                      , fname = T.decodeUtf8 $ case compFct.name of
                                          "$main" -> compFct.name
                                          _ -> "$" <> compFct.name
                                      , args = Nothing
                                      , returnType = VM.FirstOrderSO VM.IntTO
                                      , body = case assemble compFct of
                                          Left err -> VM.ByteCode Vc.empty
                                          Right compiled -> VM.ByteCode compiled
                                    }
                                  ) $ Mp.elems compiledCode.functions
                              , constants = Vc.fromList . map (convertCompCteToTempl . fst) $ Mp.elems compiledCode.constants
                              , externModules = Mp.empty
                            }
                          rezE <- VM.execModule vmModule
                          case rezE of
                            Left errMsg -> do
                              putStrLn $ "@[runWorkItem.HgEngine] execModule err: " <> errMsg
                            Right (VM.ExecResult vmCtxt) -> do
                              putStrLn $ "@[runWorkItem.HgEngine] exec rez: " <> show vmCtxt
                          pure . Right $ context { compTemplates = Mp.insert tmplPath rezC context.compTemplates }
          Just stmts -> do
            putStrLn $ "@[runWorkItem.HgEngine] template already parsed: " <> tmplPath
            pure $ Right context

-- ***** Logic for dealing with a project work (create, build) *****

analyseProject :: RunOptions -> Fs.PathFiles -> IO (Either GenError HgWorkPlan)
analyseProject rtOpts pathFiles =
  case getHugoOpts rtOpts.techOpts of
    Nothing -> pure $ Left $ SimpleMsg "@[analyseProject] not a Hugo project."
    Just hugoOpts ->
        let
          content = classifyContent (rtOpts, hugoOpts) pathFiles
          projDef = ProjectDefinition rtOpts.baseDir (Site Hugo) [] pathFiles
        in
        analyseContent (rtOpts, hugoOpts) projDef content



analyseContent :: (RunOptions, HugoBuildOptions) -> ProjectDefinition -> HugoComponents -> IO (Either GenError HgWorkPlan)
analyseContent (rtOpts, hugoOpts) (ProjectDefinition baseDir (Site Hugo) [] pathFiles) components =
  -- TODO: analyze the configs first to confirm where the content/data/... files come from (implicit location or explicitly defined directories).
  --    Then load the file trees for each of these directories to assemble the correct list of files to analyze.
  let
    globConfig = scanForGlobConfig hugoOpts.environment components.config
    markupFiles = extractMarkup components.content
    fakePrefixes_0 = Mp.singleton 0 $ baseDir </> "content"
    -- dbgContent = "NextJS Site project definition: " <> pack (show markupFiles)
  in do
  putStrLn $ "@[analyseContent] markupFiles: " <> show markupFiles
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
                (mbTheme, fakePrefix_1) = case Mp.lookup (unpack aLabel) components.themes of
                  Nothing -> (Nothing, fakePrefixes_0)
                  Just aTmplCore -> (Just $ buildTemplateFromCore aTmplCore, Mp.insert 1 (baseDir </> "themes" </> unpack aLabel </> "layouts") fakePrefixes_0)
              in
              case (mbTheme, mbProjLayout) of
                (Nothing, Nothing) -> pure . Left . SimpleMsg $ "@[analyseContent] no theme directory (" <> aLabel <> ") nor site layout available."
                (_, _) -> do
                  putStrLn $ "@[analyseContent] theme: " <> show mbTheme
                  putStrLn $ "@[analyseContent] proj layout: " <> show mbProjLayout
                  putStrLn $ "@[analyseContent] markup pages: " <> show mrkpPages
                  case assignContentToTheme context components (maybesToArray (mbProjLayout, mbTheme)) mrkpPages of
                    Left err -> pure $ Left err
                    Right pairs ->
                      let
                        prefix_TODO_content = 0
                        prefix_TODO_template = 1
                        mbWorkItems = case pairs of
                          [] -> Nothing
                          h : t -> Just $ foldl (\accum aPair ->
                              accum <> mkWorkItem prefix_TODO_content prefix_TODO_template aPair
                            ) (mkWorkItem prefix_TODO_content prefix_TODO_template h) t
                        engineHg = HgEngine { hugoOpts = hugoOpts }
                        contextHg = HgContext {
                              mergedConfigs = context.mergedConfigs
                            , pathPrefixes = fakePrefix_1
                            , compTemplates = Mp.empty
                          }
                      in
                      pure $ case mbWorkItems of
                        Nothing -> Left $ SimpleMsg "@[analyseContent] no work items to process."
                        Just workItems -> Right WorkPlan { items = workItems, engine = engineHg, context = contextHg }
  where
  mkWorkItem :: Int32 -> Int32 -> (MarkupPage, PageTmpl) -> NonEmpty HgWorkItem
  mkWorkItem ctPrefix thPrefix (aPage, aTmpl) =
    let
      (dirPath, fileItem) = aPage.item
    in
    case aTmpl of
      FileRef (templPath, Fs.KnownFile aKind filePath) -> ExecTmplForContent (CTPair {
            kind = aKind
          , contentPrefix = ctPrefix
          , section = dirPath
          , content = fileItem
          , themePrefix = thPrefix
          , template = templPath </> filePath
        }) :| [] 
      _ -> ExecTmplForRoute dirPath :| []


maybesToArray :: (Maybe a, Maybe a) -> [ a ]
maybesToArray = foldr (\a accum -> maybe accum (: accum) a) []

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


-- TODO: get this working!
assignContentToTheme :: AnalyzeContext -> HugoComponents -> [ Template ] ->[ MarkupPage ] -> Either GenError [ (MarkupPage, PageTmpl) ]
assignContentToTheme context components themes markupPages =
  let
    language = case Mp.lookup "defaultContentLanguage" context.mergedConfigs of
      Just (StringDV aV) -> aV
      _ -> "en"
  in
  foldM (analyzePage context themes) [] markupPages
  where
  analyzePage :: AnalyzeContext -> [ Template ] -> [ (MarkupPage, PageTmpl) ] -> MarkupPage -> Either GenError [ (MarkupPage, PageTmpl) ]
  analyzePage context themes accum aPage =
    if isMarkdown aPage.content.encoding then
      -- TODO: apply the relevant front-matter and association rules to assign the page to a theme.
      let
        mbLocale = aPage.frontMatter >>= (\fm -> case Mp.lookup "language" fm.fields of
              Just (ValueDF aLang) -> Just aLang
              Nothing -> Nothing
            )
        (dirPath, aFile) = aPage.item
        (locale, sections) =
          let
            allDirs = splitDirectories dirPath
          in
          if isIso639 $ head allDirs then
            (head allDirs, tail allDirs)
          else
            ("", allDirs)
        mbPage = findPageTmpl themes (locale, sections) aPage
      in
      case mbPage of
        Nothing -> Right accum
        Just pageTmpl -> Right $ accum <> [ (aPage, pageTmpl) ]
    else
      Left . SimpleMsg $ "@[assignContentToTheme] markup encoding " <> pack (show aPage.content.encoding) <> " not supported yet."

  -- TODO: create multi-template associations (eg [baseof.html, single.html]) that represent a hierarchy of templates.
  findPageTmpl :: [ Template ] -> (FilePath, [FilePath]) -> MarkupPage -> Maybe PageTmpl
  findPageTmpl templates (locale, sections) mkPage =
    case templates of
      [] -> Nothing
      aTempl : rest ->
        let
          mkPagePath = Fs.getItemPath (snd mkPage.item)
          kind = case mkPage.frontMatter of
            Nothing -> if null sections && mkPagePath `elem` ["index.md", "_index.md" ] then Home else Single
            Just aFM -> case Mp.lookup "layout" aFM.fields of
              Just (ValueDF aType) -> case aType of
                StringDV "home" -> Home
                StringDV "section" -> Section
                StringDV "taxonomy" -> Taxonomy
                StringDV "term" -> Term
                StringDV "list" -> List
                StringDV "summary" -> Summary
                StringDV aLabel -> Other aLabel
                _ -> Single
              Nothing -> if null sections && mkPagePath `elem` [ "index.md", "_index.md" ] then Home else Single
        in
        case findInLayout aTempl.layout kind (locale, sections) mkPage of
          Just aPageTmpl -> Just aPageTmpl
          Nothing -> findPageTmpl rest (locale, sections) mkPage

  findInLayout :: LayoutTmpl -> PageKind -> (FilePath, [FilePath]) -> MarkupPage -> Maybe PageTmpl
  findInLayout layout kind (locale, sections) mkPage =
    case sections of
      [] -> case findAtTop layout.topLevel kind locale mkPage of
        Just aPageTmpl -> Just aPageTmpl
        Nothing -> findAtTop layout.defaults kind locale mkPage
      aSection : _ ->
        case findInKind layout.kinds kind (locale, aSection) mkPage of
          Just aPageTmpl -> Just aPageTmpl
          Nothing ->
            case findAtTop layout.topLevel kind locale mkPage of
              Just aPageTmpl -> Just aPageTmpl
              Nothing -> findAtTop layout.defaults kind locale mkPage

  findAtTop :: Mp.Map Text PageTmpl -> PageKind -> String -> MarkupPage -> Maybe PageTmpl
  findAtTop aMap kind locale mkPage =
    let
      -- TODO: find out how the prefix/ext are decided.
      prefix = case kind of
        Home -> "home"
        Single -> "single"
        List -> "list"
        Taxonomy -> "taxonomy"
        Term -> "term"
        Section -> "section"
        Summary -> "summary"
        Other aLabel -> aLabel
      renderExt = "html"
    in
    case Mp.lookup (prefix <> "." <> pack locale <> "." <> renderExt) aMap of
      Just aPageTmpl -> Just aPageTmpl
      Nothing -> case Mp.lookup (prefix <> "." <> renderExt) aMap of
        Just aPageTmpl -> Just aPageTmpl
        Nothing -> Mp.lookup ("single." <> renderExt) aMap

  findInKind :: Mp.Map Text ThemeTmplPages -> PageKind -> (String, String) -> MarkupPage -> Maybe PageTmpl
  findInKind categMap kind (locale, section) mkPage =
    case Mp.lookup (pack section) categMap of
      Just aSubMap -> findAtTop aSubMap kind locale mkPage
      Nothing -> Nothing


  isMarkdown :: ContentEncoding -> Bool
  isMarkdown (ParsedMarkdown _) = True
  isMarkdown RawMarkdown = True
  isMarkdown _ = False


isIso639 :: String -> Bool
isIso639 aStr =
  case aStr of
    [ a, b ] -> isJust $ fromChars a b
    _ -> False


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
analyzeMarkupPage rtOpts rootDir groupPath file =
  -- putStrLn $ "@[analyzeMarkupPage] analyzing: " <> fullPath
  parseContent rtOpts (rootDir </> "content") file

analyzeMarkupPage rtOpts rootDir groupPath (aDirPath, Fs.KnownFile _ filePath) = do
  putStrLn $ "@[analyzeMarkupPage] error, trying to analyze a non-markup file: " <> groupPath </> filePath
  pure $ Left $ SimpleMsg "Trying to analyze a non-markup file."

analyzeMarkupPage rtOpts rootDir groupPath (aDirPath, Fs.MiscFile filePath) = do
  putStrLn $ "@[analyzeMarkupPage] error, trying to analyze a non-markup file: " <> groupPath </> filePath
  pure $ Left $ SimpleMsg "Trying to analyze a non-markup file."


{-
  - config folder: contains sections of configuration, _default and others (eg 'production).
      The main config should be 'hugo.<ext>', a change in v0.109.0 from 'config.<ext>' (still supported).

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


------
Known media types and matching file extensions:
application/json	[json]
application/manifest+json	[webmanifest]
application/octet-stream	[webmanifest]
application/pdf	[pdf]
application/rss+xml	[xml rss]
application/toml	[toml]
application/wasm	[wasm]
application/xml	[xml]
application/yaml	[yaml yml]
font/otf	[otf]
font/ttf	[ttf]
image/bmp	[bmp]
image/gif	[gif]
image/jpeg	[jpg jpeg jpe jif jfif]
image/png	[png]
image/svg+xml	[svg]
image/tiff	[tif tiff]
image/webp	[webp]
text/asciidoc	[adoc asciidoc ad]
text/calendar	[ics]
text/css	[css]
text/csv	[csv]
text/html	[html htm]
text/javascript	[js jsm mjs]
text/jsx	[jsx]
text/markdown	[md mdown markdown]
text/org	[org]
text/pandoc	[pandoc pdc]
text/plain	[txt]
text/rst	[rst]
text/tsx	[tsx]
text/typescript	[ts]
text/x-sass	[sass]
text/x-scss	[scss]
video/3gpp	[3gpp 3gp]
video/mp4	[mp4]
video/mpeg	[mpg mpeg]
video/ogg	[ogv]
video/webm	[webm]
video/x-msvideo	[avi]

* Output formats:
Type            BaseName  Html?   PlTxt?  MediaType                   NoUgly  Path  Perma?  Protocol    Rel
amp           	index	    true	  false	  text/html	                  false	  amp	  true		            amphtml
calendar	      index	    false	  true	  text/calendar	              false		      false	  webcal://	  alternate
css	            styles	  false	  true	  text/css	                  false		      false		            stylesheet
csv	            index	    false	  true	  text/csv	                  false		      false		            alternate
html	          index	    true	  false	  text/html	                  false		      true		            canonical
json	          index	    false	  true	  application/json	          false		      false		            alternate
markdown	      index	    false	  true	  text/markdown	              false		      false		            alternate
robots	        robots	  false	  true	  text/plain	                false		      false		            alternate
rss	            index	    false	  false	  application/rss+xml	        true		      false		            alternate
sitemap	        sitemap	  false	  false	  application/xml	            false		      false		            sitemap
webappmanifest	manifest	false	  true	  application/manifest+json	  false		      false		            manifest
-}
