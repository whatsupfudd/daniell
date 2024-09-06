{-# LANGUAGE LambdaCase #-}
module ProjectDefinition.Hugo where

import Data.List (isPrefixOf)
import Data.Maybe (fromMaybe)
import qualified Data.Map.Strict as Mp
import Data.Text (pack)
import System.FilePath ((</>))
import qualified Data.Text.IO as T

import qualified Toml as Tm
import qualified Data.Yaml as Ym
import qualified Data.Aeson as Ae

import qualified FileSystem.Types as Fs
import Options.Runtime (RunOptions (..), TechOptions (..), HugoRunOptions (..))
import Conclusion (GenError (..))
import Generator.Types (WorkPlan (..))

import ProjectDefinition.Types
import Data.Either (fromRight)
import Control.Monad (foldM)

{- For Hugo project, use archetype/* to create a new document in the content section -}

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
-}

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


analyseHugoProject :: RunOptions -> Fs.PathFiles -> IO (Either GenError WorkPlan)
analyseHugoProject rtOpts pathFiles =
    let
    content = classifyContent rtOpts pathFiles
  in
    analyseContent rtOpts $ ProjectDefinition rtOpts.baseDir (Site (Hugo content)) [] pathFiles


classifyContent :: RunOptions -> Fs.PathFiles -> HugoComponents
classifyContent rtOpts pathFiles =
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


analyseContent :: RunOptions -> ProjectDefinition -> IO (Either GenError WorkPlan)
analyseContent rtOpts (ProjectDefinition baseDir (Site (Hugo components)) [] pathFiles) =
  let
    globConfig = scanForGlobConfig (getHugoEnvironment rtOpts) components.config
    markupFiles = extractMarkup components.content
    dbgContent = "NextJS Site project definition: " <> pack (show markupFiles)
  in do
  runConfigs <- analyzeConfig rtOpts globConfig
  pure . Left $ SimpleMsg dbgContent


extractMarkup :: [ FileWithPath ] -> Mp.Map String [ FileWithPath ]
extractMarkup = foldl (\accum (dirPath, aFile) ->
    case aFile of
      Fs.KnownFile kind aPath ->
        if kind == Fs.Markup then
          Mp.insertWith (<>) dirPath [(dirPath, aFile)] accum
        else
          accum
      _ -> accum
  ) Mp.empty


scanForGlobConfig :: Maybe String -> [ FileWithPath ] -> (Maybe FileWithPath, Maybe FileWithPath, [ FileWithPath ])
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


getHugoEnvironment :: RunOptions -> Maybe String
getHugoEnvironment rtOpts =
  case rtOpts.techOpts of
    HugoOptions opts -> Just opts.environment
    _ -> Nothing

data AnalyzeContext = AnalyzeContext


analyzeConfig :: RunOptions -> (Maybe FileWithPath, Maybe FileWithPath, [ FileWithPath ]) -> IO (Either GenError AnalyzeContext)
analyzeConfig rtOpts (topConfig, defaultConfig, otherConfigs) =
  maybe (pure $ Right AnalyzeContext) (parseTopConfig AnalyzeContext rtOpts.baseDir) topConfig
    >>= (\case
        Left err -> pure $ Left err
        Right ctxt -> maybe (pure $ Right ctxt) (parseDefaultConfig ctxt rtOpts.baseDir) defaultConfig
      )
    >>= (\case
      Left err -> pure $ Left err
      Right ctxt -> parseOtherConfigs ctxt rtOpts.baseDir otherConfigs
      )
  where
  parseTopConfig :: AnalyzeContext -> FilePath -> FileWithPath -> IO (Either GenError AnalyzeContext)
  parseTopConfig context rootDir aFile = do
    putStrLn "Parsing top config..."
    parseConfigFile context rootDir aFile

  parseDefaultConfig :: AnalyzeContext -> FilePath -> FileWithPath -> IO (Either GenError AnalyzeContext)
  parseDefaultConfig context rootDir aFile = do
    putStrLn "Parsing default config..."
    parseConfigFile context rootDir aFile

  parseOtherConfigs :: AnalyzeContext -> FilePath -> [ FileWithPath ] -> IO (Either GenError AnalyzeContext)
  parseOtherConfigs context rootDir otherConfigs = do
    putStrLn "Parsing other configs..."
    parseRez <- foldM (\accum aFile -> case accum of
          Left err -> pure $ Left err
          Right aCtxt -> parseConfigFile aCtxt rootDir aFile
        ) (Right context) otherConfigs
    pure $ case parseRez of
      Left err -> Left err
      Right _ -> Right context


parseConfigFile :: AnalyzeContext -> FilePath -> FileWithPath -> IO (Either GenError AnalyzeContext)
parseConfigFile context rootDir (dirPath, Fs.KnownFile kind filePath) = do
  let
    fullPath = rootDir </> "config" </> dirPath </> filePath
  putStrLn $ "@[parseDefaultConfig] reading: " <> fullPath
  putStrLn $ "@[parseDefaultConfig] analyzing: " <> fullPath
  case kind of
    Fs.Toml -> do
      srcText <- T.readFile fullPath
      case Tm.parse srcText of
        Left err -> do
          putStrLn $ "@[parseDefaultConfig] error parsing TOML: " <> show err
          pure . Left $ SimpleMsg ("TOML err: " <> pack (show err))
        Right tomlVal -> do
          putStrLn $ "@[parseDefaultConfig] parsed TOML: " <> show tomlVal
          pure $ Right AnalyzeContext
    Fs.Yaml -> do
      eiYamlRez <- Ym.decodeFileEither fullPath :: IO (Either Ym.ParseException Ae.Value)
      case eiYamlRez of
        Left err -> do
          putStrLn $ "@[parseDefaultConfig] error parsing Yaml: " <> show err
          pure . Left $ SimpleMsg ("TOML err: " <> pack (show err))
        Right yamlVal -> do
          putStrLn $ "@[parseDefaultConfig] parsed Yaml: " <> show yamlVal
          pure $ Right AnalyzeContext
    Fs.Json -> do
      putStrLn $ "@[parseDefaultConfig] parsing JSON..."
      eiJsonRez <- Ae.eitherDecodeFileStrict' fullPath :: IO (Either String Ae.Object)
      case eiJsonRez of
        Left err -> do
          putStrLn $ "@[parseDefaultConfig] error parsing JSON: " <> err
          pure . Left $ SimpleMsg ("JSON err: " <> pack err)
        Right jsonVal -> do
          putStrLn $ "@[parseDefaultConfig] parsed JSON: " <> show jsonVal
          pure $ Right AnalyzeContext

parseConfigFile context rootDir (dirPath, Fs.MiscFile aPath) = do
  putStrLn "@[parseConfigFile] misc file!"
  pure . Left . SimpleMsg $ "@[parseConfigFile] error, trying to parse misc file " <> pack aPath
