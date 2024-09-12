{-# LANGUAGE LambdaCase #-}
module ProjectDefinition.Hugo.Config where

import Control.Monad (foldM)
import System.FilePath ((</>), dropExtension)

import Data.Text (Text, pack)
import qualified Data.Text.IO as T
import qualified Data.Map as Mp

import qualified Toml as Tm
import qualified Data.Aeson as Ae
import qualified Data.Yaml as Ym

import Options.Runtime (RunOptions (..), TechOptions (..))
import Options.Types (HugoBuildOptions (..))

import Conclusion (GenError (..))
import qualified FileSystem.Types as Fs
import ProjectDefinition.Paraml (tomlToDict)
import ProjectDefinition.Types (DictEntry (..))
import ProjectDefinition.Hugo.Types


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

-- ***** Inspection and analysis logic *****
analyzeConfig :: RunOptions -> (Maybe Fs.FileWithPath, Maybe Fs.FileWithPath, [ Fs.FileWithPath ]) -> IO (Either GenError AnalyzeContext)
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
  -- TODO: get the RugFlag for the contextDefaultOptions from somewhere (build vs server).
  mergeConfig :: AnalyzeContext -> Either GenError AnalyzeContext
  mergeConfig context =
    let
      mbHugoOpts = getHugoOpts rtOpts.techOpts
      environment = case mbHugoOpts of
        Nothing -> "production"
        Just hugoOpts ->
          case hugoOpts.environment of
            Just envName -> envName
            Nothing -> case Mp.lookup "environment" context.globalVars of
              Just (StringDV globEnvName) -> globEnvName
              _ -> case Mp.lookup "environment" context.defaultVars of
                Just (StringDV defEnvName) -> defEnvName
                _ -> "production"
      configVars = if Mp.null context.globalVars then [] else [ context.globalVars ]
                  <> case Mp.lookup environment context.otherVars of
                       Nothing -> [] 
                       Just envVars -> if Mp.null envVars then [] else [ envVars ]
                  <> if Mp.null context.defaultVars then [] else [ context.defaultVars ]
                  <> [ contextDefaultOptions BuildF ]
      valueList = map (resolveConfig mbHugoOpts configVars) ctxtOptionKeys
    in
    Right context { mergedConfigs = Mp.fromList $ foldl (\accum v -> case v of Nothing -> accum ; Just aVal -> aVal : accum) [] valueList }
    where
    resolveConfig :: Maybe HugoBuildOptions -> [ Mp.Map Text DictEntry ] -> Text -> Maybe (Text, DictEntry)
    resolveConfig mbHugoOpts configVars aKey =
      let
        mbValue = case mbHugoOpts of
          Nothing -> Nothing
          Just hugoOpts -> case checkBuildOptsForKey hugoOpts aKey of
            Just aVal -> case aVal of
              BoolBO aBool -> Just (aKey, BoolDV aBool)
              StringBO aText -> Just (aKey, StringDV aText)
            Nothing -> Nothing
      in
      case mbValue of
        Just aVal -> mbValue
        Nothing -> case findKeyInConfigs aKey configVars of
          Just aVal -> Just (aKey, aVal)
          Nothing -> case Mp.lookup aKey (contextDefaultOptions BuildF) of
            Just aVal -> Just (aKey, aVal)
            Nothing -> Nothing
    findKeyInConfigs :: Text -> [ Mp.Map Text DictEntry ] -> Maybe DictEntry
    findKeyInConfigs aKey configVars =
      case configVars of
        [] -> Nothing
        aConfig : rest -> case Mp.lookup aKey aConfig of
          Just aVal -> Just aVal
          Nothing -> findKeyInConfigs aKey rest
    {-
      - then assemble an array of main config, environment config and default config,
      - then iterate through the main config keys (ctxtOptionKeys):
        - first check if the HugoBuildOptions has been specified,
        - if not, then check for the 1st item of the config array,
        - if not, then check for the 2nd item of the config array,
        - if not, then check for the 3rd item of the config array,
        - if not, use the default value.
    -}

  parseTopConfig :: AnalyzeContext -> FilePath -> Fs.FileWithPath -> IO (Either GenError AnalyzeContext)
  parseTopConfig context rootDir aFile = do
    eiRez <- parseConfigFile rootDir aFile
    case eiRez of
      Left err -> pure $ Left err
      Right aConfig -> pure $ Right context { globalVars = aConfig }

  parseDefaultConfig :: AnalyzeContext -> FilePath -> Fs.FileWithPath -> IO (Either GenError AnalyzeContext)
  parseDefaultConfig context rootDir aFile = do
    eiRez <- parseConfigFile rootDir aFile
    case eiRez of
      Left err -> pure $ Left err
      Right aConfig -> pure $ Right context { defaultVars = aConfig }

  parseOtherConfigs :: AnalyzeContext -> FilePath -> [ Fs.FileWithPath ] -> IO (Either GenError AnalyzeContext)
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


parseConfigFile :: FilePath -> Fs.FileWithPath -> IO (Either GenError (Mp.Map Text DictEntry))
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


-- ***** Utilities for extracting command line & runtime options *****
getHugoOpts :: TechOptions -> Maybe HugoBuildOptions
getHugoOpts (HugoOptions options) = Just options
getHugoOpts _ = Nothing

setRunOptions :: RunOptions -> HugoBuildOptions -> RunOptions
setRunOptions rtOpts buildOpts =
  rtOpts { techOpts = HugoOptions buildOpts }

contextDefaultOptions :: RunMode -> Mp.Map Text DictEntry
contextDefaultOptions runFlag =
  Mp.fromList [
    ("archetypeDir", StringDV "archetypes")
    , ("assetDir", StringDV "assets")
    , ("baseURL", StringDV "http://localhost:8080/")
    , ("build", DictDV $ Mp.fromList [
          ("buildStats", DictDV $ Mp.fromList [
              ("disableClasses", BoolDV False)
            , ("disableIDs", BoolDV False)
            , ("disableTags", BoolDV False)
            , ("enable", BoolDV False)
          ])
        , ("cacheBusters", ListDV [
            DictDV $ Mp.fromList [
                ("source", StringDV "(postcss|tailwind)\\.config\\.js")
              , ("target", StringDV "(css|styles|scss|sass)")
            ]
          ])
        , ("noJSConfigInAssets", BoolDV False)
        , ("useResourceCacheWhen", StringDV "fallback")
      ])
    , ("buildDrafts", BoolDV False)
    , ("buildExpired", BoolDV False)
    , ("buildFuture", BoolDV False)
    , ("caches", DictDV $ Mp.fromList [
          ("assets", DictDV $ Mp.fromList [
              ("dir", StringDV ":resourceDir/_gen")
              , ("maxAge", IntDV (-1))
            ])
        , ("getcsv", DictDV $ Mp.fromList [
            ("dir", StringDV ":cacheDir/:project")
            , ("maxAge", IntDV (-1))
          ])
        , ("getjson", DictDV $ Mp.fromList [
            ("dir", StringDV ":cacheDir/:project")
            , ("maxAge", IntDV (-1))
          ])
        , ("getresource", DictDV $ Mp.fromList [
            ("dir", StringDV ":cacheDir/:project")
            , ("maxAge", IntDV (-1))
          ])
        , ("images", DictDV $ Mp.fromList [
            ("dir", StringDV ":resourceDir/_gen")
            , ("maxAge", IntDV (-1))
          ])
        , ("misc", DictDV $ Mp.fromList [
            ("dir", StringDV ":cacheDir/:project")
            , ("maxAge", IntDV (-1))
          ])
        , ("modules", DictDV $ Mp.fromList [
            ("dir", StringDV ":cacheDir/modules")
            , ("maxAge", IntDV (-1))
          ])
      ])
    , ("canonifyURLs", BoolDV False)
    , ("capitalizeListTitles", BoolDV True)
    , ("cascade", ListDV [])
        {- example of a cascade entry:
          DictDV $ Mp.fromList [
            ("_target", DictDV $ Mp.fromList [
                ("kind", StringDV "page")
              , ("lang", StringDV "en")
              , ("path", "/articles/**")
            ])
          , ("params", DictDV $ Mp.fromList [
              ("background", StringDV "yosemite.jpg")
            ])
          ])
        -}
    , ("cleanDestinationDir", BoolDV False)
    , ("contentDir", StringDV "content")
    , ("copyright", StringDV "")
    , ("dataDir", StringDV "data")
    , ("defaultContentLanguage", StringDV "en")
    , ("defaultContentLanguageInSubdir", BoolDV False)
    , ("disableAliases", BoolDV False)
    , ("disableHugoGeneratorInject", BoolDV False)
    , ("disableKinds", ListDV [])     -- Entries are StringDV, any of: 404, home, page, robotstxt, rss, section, sitemap, taxonomy, or term.
    , ("disableLanguages", ListDV []) -- Entries are StringDV, any of iso639: en, fr, de, etc.
    , ("disableLiveReload", BoolDV False)
    , ("disablePathToLower", BoolDV False)
    , ("enableEmoji", BoolDV False)
    , ("enableGitInfo", BoolDV False)
    , ("enableMissingTranslationPlaceholders", BoolDV False)
    , ("enableRobotsTXT", BoolDV False)
    , ("environment", StringDV (case runFlag of BuildF -> "production"; ServerF -> "development"))
    , ("frontmatter", DictDV $ Mp.fromList [
          ("date", ListDV [StringDV "date", StringDV "publishdate", StringDV "pubdate", StringDV "published", StringDV "lastmod", StringDV "modified"])
        , ("expiryDate", ListDV $ [StringDV "expiryDate", StringDV "unpublishdate"])
        , ("lastmod", ListDV $ [StringDV ":git", StringDV "lastmod", StringDV "modified", StringDV "date", StringDV "publishdate", StringDV "pubdate", StringDV "published"])
        , ("publishDate", ListDV $ [StringDV "publishDate", StringDV "pubdate", StringDV "published", StringDV "date"])
      ])
    , ("hasCJKLanguage", BoolDV False)
    , ("ignoreCache", BoolDV False)
    , ("ignoreLogs", ListDV [])   -- Entries are StringDV, matching errorsidf (error-<id>) & warnidf (warn-<id>).
    , ("ignoreVendorPaths", StringDV "")  -- A Glob pattern that will filter out files from the _vendor directory.
    , ("imaging", DictDV $ Mp.fromList [
          ("bgColor", StringDV "#ffffff")
        , ("hint", StringDV "photo")
        , ("quality", IntDV 75)
        , ("resampleFilter", StringDV "box")
        , ("exif", DictDV $ Mp.fromList [
              ("disableDate", BoolDV False)
            , ("disableLatLong", BoolDV False)
            , ("excludeFields", StringDV "")     -- regex pattern.
            , ("includeFields", StringDV "")     -- regex pattern.
        ])
      ])
    , ("languageCode", StringDV "en")
    , ("languages", DictDV $ Mp.fromList [
          ("en", DictDV $ Mp.fromList [
              ("disabled", BoolDV False)
            , ("languageCode", StringDV "")
            , ("languageDirection", StringDV "")
            , ("languageName", StringDV "")
            , ("title", StringDV "")
            , ("weight", IntDV 0)
            -- can also have pagination.
          ])
      ])
    , ("layoutDir", StringDV "layouts")
    , ("markup", DictDV $ Mp.fromList [
        ("defaultMarkdownHandler", StringDV "goldmark")   -- Golang's libs: asciidocext, goldmark, org, pandoc, rst.
      ])
    , ("mediaTypes", DictDV Mp.empty)
        {- example of a mediaTypes entry:
          ("text/enriched", DictDV $ Mp.fromList [
              ("suffixes", ListDV $ [StringDV "enr"])
          ])
        , ("text/html", DictDV $ Mp.fromList [
            ("suffixes", ListDV $ [StringDV "asp"])
          ])
        -}
    , ("menus", DictDV Mp.empty)
        {- example of a menu definition:
          ("main", ListDV $ [
              DictDV $ Mp.fromList [
                  ("name", StringDV "home")
                , ("pageRef", StringDV "/")
                , ("weight", IntDV 10)
              )
            , DictDV $ Mp.fromList [
                  ("name", StringDV "Products")
                , ("pageRef", StringDV "/products")
                , ("weight", IntDV 20)
              )
            , DictDV $ Mp.fromList [
                  ("name", StringDV "Services")
                , ("pageRef", StringDV "/services")
                , ("weight", IntDV 30)
              )
        -}
    , ("minify", DictDV $ Mp.fromList [
          ("disableCSS", BoolDV False)
        , ("disableHTML", BoolDV False)
        , ("disableJS", BoolDV False)
        , ("disableJSON", BoolDV False)
        , ("disableSVG", BoolDV False)
        , ("disableXML", BoolDV False)
        , ("minifyOuput", BoolDV False)
        , ("tdewolff", DictDV $ Mp.fromList [
              ("css", DictDV $ Mp.fromList [
                    ("inline", BoolDV False)
                  , ("keepCSS2", BoolDV False)
                  , ("precision", IntDV 0)
              ])
            , ("html", DictDV $ Mp.fromList [
                  ("keepComments", BoolDV False)
                , ("keepConditionalComments", BoolDV False)
                , ("keepDefaultAttrVals", BoolDV True)
                , ("keepDocumentTags", BoolDV True)
                , ("keepEndTags", BoolDV True)
                , ("keepQuotes", BoolDV False)
                , ("keepSpecialComments", BoolDV True)
                , ("keepWhitespace", BoolDV False)
                , ("templateDelims", ListDV $ [StringDV "", StringDV ""])
              ])
            , ("js", DictDV $ Mp.fromList [
                  ("keepVarNames", BoolDV False)
                , ("precision", IntDV 0)
                , ("version", StringDV "2022")
              ])
            , ("json", DictDV $ Mp.fromList [
                  ("keepNumbers", BoolDV False)
                , ("precision", IntDV 0)
              ])
            , ("svg", DictDV $ Mp.fromList [
                  ("inline", BoolDV False)
                , ("keepComments", BoolDV False)
                ,  ("precision", IntDV 0)
              ])
            , ("xml", DictDV $ Mp.fromList [
                ("keepWhitespace", BoolDV False)
              ])
          ])
      ])
    , ("module", DictDV $ Mp.fromList [
          ("mounts", ListDV [
            DictDV $ Mp.fromList [
                ("source", StringDV "content")
              , ("target", StringDV "content")
            ]
          , DictDV $ Mp.fromList [
                ("source", StringDV "static")
              , ("target", StringDV "static")
            ]
          , DictDV $ Mp.fromList [
                ("source", StringDV "layouts")
              , ("target", StringDV "layouts")
            ]
          , DictDV $ Mp.fromList [
                ("source", StringDV "data")
              , ("target", StringDV "data")
            ]
          , DictDV $ Mp.fromList [
                ("source", StringDV "assets")
              , ("target", StringDV "assets")
            ]
          , DictDV $ Mp.fromList [
                ("source", StringDV "i18n")
              , ("target", StringDV "i18n")
            ]
          , DictDV $ Mp.fromList [
                ("source", StringDV "archetypes")
              , ("target", StringDV "archetypes")
            ]
        ])
        {- example of other sections configuration:
            noProxy: none
            noVendor: ""
            private: '*.*'
            proxy: direct
            replacements: ""
            vendorClosest: false
            workspace: "off"
            hugoVersion:
              extended: false
              max: ""
              min: ""
            imports:
            - disable: false
              ignoreConfig: false
              ignoreImports: false
              path: github.com/gohugoio/hugoTestModules1_linux/modh1_2_1v
            - path: my-shortcodes
        -}
      ])
    , ("newContentEditor", BoolDV False)
    , ("noBuildLock", BoolDV False)
    , ("noChmod", BoolDV False)
    , ("noTimes", BoolDV False)
    , ("outputFormats", DictDV Mp.empty)
        {- example of outputFormats configuration:
            MyEnrichedFormat:
              baseName: myindex
              isPlainText: true
              mediaType: text/enriched
              protocol: bep://
        -}
    , ("page", DictDV $ Mp.fromList [
          ("nextPrevInSectionSortOrder", StringDV "desc")
        , ("nextPrevSortOrder", StringDV "desc")
      ])
    , ("pagination", DictDV $ Mp.fromList [
          ("disableAliases", BoolDV False)
        , ("pagerSize", IntDV 10)
        , ("path", StringDV "page")
      ])
    , ("panicOnWarning", BoolDV False)
    , ("permalinks", DictDV Mp.empty)
      {- example of permalinks configuration:
            page:
              posts: /articles/:year/:month/:slug/
              tutorials: /training/:slug/
            section:
              posts: /articles/
              tutorials: /training/
      -}
    , ("pluralizeListTitles", BoolDV True)
    , ("printI18nWarnings", BoolDV False)
    , ("printPathWarnings", BoolDV False)
    , ("printUnusedTemplates", BoolDV False)
    , ("publishDir", StringDV "public")
    , ("refLinksErrorLevel", StringDV "ERROR")
    , ("refLinksNotFoundURL", StringDV "")
    , ("related", DictDV $ Mp.fromList [
          ("indices", ListDV [
              DictDV $ Mp.fromList [
                  ("applyFilter", BoolDV False)
                , ("cardinalityThreshold", IntDV 0)
                , ("name", StringDV "keywords")
                , ("pattern", StringDV "")
                , ("toLower", BoolDV False)
                , ("type", StringDV "basic")
                , ("weight", IntDV 100)
              ]
            , DictDV $ Mp.fromList [
                  ("applyFilter", BoolDV False)
                , ("cardinalityThreshold", IntDV 0)
                , ("name", StringDV "date")
                , ("pattern", StringDV "")
                , ("toLower", BoolDV False)
                , ("type", StringDV "basic")
                , ("weight", IntDV 100)
              ]
            , DictDV $ Mp.fromList [
                  ("applyFilter", BoolDV False)
                , ("cardinalityThreshold", IntDV 0)
                , ("name", StringDV "tags")
                , ("pattern", StringDV "")
                , ("toLower", BoolDV False)
                , ("type", StringDV "basic")
                , ("weight", IntDV 100)
              ]            
          ])
        , ("threshold", IntDV 80)
        , ("toLower", BoolDV False)
      ])
    , ("relativeURLs", BoolDV False)
    , ("removePathAccents", BoolDV False)
    , ("renderSegments", ListDV [])    -- Segments in the "segments" directory to render.
    , ("sectionPagesMenu", StringDV "")
    , ("security", DictDV $ Mp.fromList [
          ("enableInlineShortCodes", BoolDV False)
        , ("exec", DictDV $ Mp.fromList [
              ("allow", ListDV [
                  StringDV "^(dart-)?sass(-embedded)?$"
                , StringDV "^go$"
                , StringDV "^git$"
                , StringDV "^npx$"
                , StringDV "^postcss$"
                , StringDV "^tailwindcss$"
              ])
            , ("osEnv", ListDV [
                StringDV "(?i)^((HTTPS?|NO)_PROXY|PATH(EXT)?|APPDATA|TE?MP|TERM|GO\\w+|(XDG_CONFIG_)?HOME|USERPROFILE|SSH_AUTH_SOCK|DISPLAY|LANG|SYSTEMDRIVE)$"
              ])
          ])
        , ("funcs", DictDV $ Mp.fromList [
              ("getenv", ListDV [
                  StringDV "^HUGO_"
                , StringDV "^CI$"
              ])
          ])
        , ("http", DictDV $ Mp.fromList [
              ("mediaTypes", StringDV "null")
              , ("methods", ListDV [ StringDV "(?i)GET|POST"])
              , ("urls", ListDV [ StringDV ".*" ])
          ])
      ])
    , ("segments", DictDV Mp.empty)
      {- example of a segment configuration:
          segment1:
            excludes:
            - lang: n*
            - lang: en
              output: rss
            includes:
            - kind: '{home,term,taxonomy}'
            - path: '{/docs,/docs/**}'
      -}
    , ("sitemap", DictDV $ Mp.fromList [
          ("changefreq", StringDV "")
          , ("disable", BoolDV False)
          , ("filename", StringDV "sitemap.xml")
          , ("priority", IntDV (-1))
      ])
    , ("summaryLength", IntDV 70)
    , ("taxonomies", DictDV $ Mp.fromList [
          ("category", StringDV "categories")
        , ("tag", StringDV "tags")
      ])
    , ("templateMetrics", BoolDV False)
    , ("templateMetricsHints", BoolDV False)
    , ("theme", StringDV "")
    , ("themesDir", StringDV "themes")
    , ("timeout", StringDV "30s")
    , ("timeZone", StringDV "")  -- At least UTC, Local, and locations of IANA time zone database.
    , ("title", StringDV "")
    , ("titleCaseStyle", StringDV "ap")
    , ("uglyURLs", BoolDV False)
    , ("watch", BoolDV False)
  ]

ctxtOptionKeys :: [ Text ]
ctxtOptionKeys = [
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


data BuildOptValue =
  StringBO Text
  | BoolBO Bool


checkBuildOptsForKey :: HugoBuildOptions -> Text -> Maybe BuildOptValue
checkBuildOptsForKey opts key =
  if key `elem` [
          "archetypeDir", "assetDir", "build", "caches"
        , "canonifyURLs", "capitalizeListTitles", "cascade", "copyright", "dataDir"
        , "defaultContentLanguage", "defaultContentLanguageInSubdir", "disableAliases", "disableHugoGeneratorInject"
        , "disableLanguages", "disableLiveReload", "disablePathToLower", "enableEmoji"
        , "enableMissingTranslationPlaceholders", "enableRobotsTXT", "frontmatter", "hasCJKLanguage"
        , "ignoreLogs", "imaging", "languageCode", "languages", "markup", "mediaTypes", "menus"
        , "module", "newContentEditor", "outputFormats", "page", "pagination"
        , "permalinks", "pluralizeListTitles"
        , "publishDir", "refLinksErrorLevel", "refLinksNotFoundURL", "related", "relativeURLs", "removePathAccents", "renderSegments"
        , "sectionPagesMenu", "security", "segments", "sitemap", "summaryLength", "taxonomies"
        , "timeout", "timeZone", "title", "titleCaseStyle", "uglyURLs" 
    ] then
      Nothing
  else
  case key of
    "baseURL" -> case opts.baseURL of
      Just val -> Just $ StringBO val
      Nothing -> Nothing
    "buildDrafts" -> case opts.buildDrafts of
      Just val -> Just $ BoolBO val
      Nothing -> Nothing
    "buildExpired" -> case opts.buildExpired of
      Just val -> Just $ BoolBO val
      Nothing -> Nothing
    "buildFuture" -> case opts.buildFuture of
      Just val -> Just $ BoolBO val
      Nothing -> Nothing
    "cleanDestinationDir" -> case opts.cleanDestinationDir of
      Just val -> Just $ BoolBO val
      Nothing -> Nothing
    "contentDir" -> case opts.contentDir of
      Just val -> Just $ StringBO val
      Nothing -> Nothing
    "disableKinds" -> case opts.disableKinds of
      Just val -> Just $ StringBO val
      Nothing -> Nothing
    "enableGitInfo" -> case opts.enableGitInfo of
      Just val -> Just $ BoolBO val
      Nothing -> Nothing
    "environment" -> case opts.environment of
      Just val -> Just $ StringBO val
      Nothing -> Nothing
    "ignoreCache" -> case opts.ignoreCache of
      Just val -> Just $ BoolBO val
      Nothing -> Nothing
    "ignoreVendorPaths" -> case opts.ignoreVendorPaths of
      Just val -> Just $ StringBO val
      Nothing -> Nothing
    "layoutDir" -> case opts.layoutDir of
      Just val -> Just $ StringBO val
      Nothing -> Nothing
    "minify" -> case opts.minify of
      Just val -> Just $ BoolBO val
      Nothing -> Nothing
    "noChmod" -> case opts.noChmod of
      Just val -> Just $ BoolBO val
      Nothing -> Nothing
    "noTimes" -> case opts.noTimes of
      Just val -> Just $ BoolBO val
      Nothing -> Nothing
    "panicOnWarning" -> case opts.panicOnWarning of
      Just val -> Just $ BoolBO val
      Nothing -> Nothing
    "printI18nWarnings" -> case opts.printI18nWarnings of
      Just val -> Just $ BoolBO val
      Nothing -> Nothing
    "printPathWarnings" -> case opts.printPathWarnings of
      Just val -> Just $ BoolBO val
      Nothing -> Nothing
    "printUnusedTemplates" -> case opts.printUnusedTemplates of
      Just val -> Just $ BoolBO val
      Nothing -> Nothing
    "templateMetrics" -> case opts.templateMetrics of
      Just val -> Just $ BoolBO val
      Nothing -> Nothing
    "templateMetricsHints" -> case opts.templateMetricsHints of
      Just val -> Just $ BoolBO val
      Nothing -> Nothing
    "theme" -> case opts.theme of
      Just val -> Just $ StringBO val
      Nothing -> Nothing
    "themesDir" -> case opts.themesDir of
      Just val -> Just $ StringBO val
      Nothing -> Nothing
    "watch" -> case opts.watch of
      Just val -> Just $ BoolBO val
      Nothing -> Nothing
    _ -> Nothing