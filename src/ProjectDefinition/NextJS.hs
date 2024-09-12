module ProjectDefinition.NextJS where

import Data.List (isPrefixOf)
import qualified Data.Map.Strict as Mp
import Data.Maybe (fromMaybe)
import Data.Text (pack)

import qualified FileSystem.Types as Fs

import Options.Runtime (RunOptions (..))
import Conclusion (GenError (..))
import Generator.Types (WorkPlan (..))
import ProjectDefinition.Types



defaultNextJS :: NextJSComponents
defaultNextJS = NextJSComponents {
    config = NextJSConfig {
        envConfig = []
        , nextConfig = []
        , packageConfig = []
        , tsConfig = []
      }
    , components = []
    , pages = []
    , api = []
    , lib = []
    , styles = []
    , utils = []
    , hooks = []
    , services = []
    , types = []
    , tests = []
    , stories = []
    , public = []
    , build = []
    , deploy = []
    , miscs = []
  }


analyseNextJsProject :: RunOptions -> Bool -> Fs.PathFiles -> Either GenError WorkPlan
analyseNextJsProject rtOpts isStatic pathFiles =
  {- TODO:
    -- go through all the pathFile and build a ProjectDefinition.
      -> ProjectDefinition rtOpts.baseDir (WebApp (NextJS defaultNextJS)) [] pathFiles
    - pass to buildPlan
     - buildPlan: RunOptions -> ProjectDefinition -> IO (Either GenError WorkPlan)
  -}

  let
    content = classifyContent rtOpts pathFiles
  in
  if isStatic then
    analyseStaticProject rtOpts (ProjectDefinition rtOpts.baseDir (Site NextStatic) [] pathFiles) content
  else
    analyseWebAppProject rtOpts (ProjectDefinition rtOpts.baseDir (WebApp NextJS) [] pathFiles) content


classifyContent :: RunOptions -> Fs.PathFiles -> NextJSComponents
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


organizeFiles :: FileSet -> NextJSComponents
organizeFiles (knownFiles, miscFiles) =
  let
    orgSet = Mp.foldl' (foldl (flip organizeAFile)) Mp.empty knownFiles
  in
  NextJSComponents {
    config = NextJSConfig {
        envConfig = maybe [] (map snd) $ Mp.lookup "envConfig" orgSet
        , nextConfig = maybe [] (map snd) $ Mp.lookup "nextConfig" orgSet
        , packageConfig = maybe [] (map snd) $ Mp.lookup "packageConfig" orgSet
        , tsConfig = maybe [] (map snd) $ Mp.lookup "tsConfig" orgSet
      }
    , components = fromMaybe [] $ Mp.lookup "components" orgSet
    , pages = fromMaybe [] $ Mp.lookup "pages" orgSet
    , api = fromMaybe [] $ Mp.lookup "api" orgSet
    , lib = fromMaybe [] $ Mp.lookup "lib" orgSet
    , styles = fromMaybe [] $ Mp.lookup "styles" orgSet
    , utils = fromMaybe [] $ Mp.lookup "utils" orgSet
    , hooks = fromMaybe [] $ Mp.lookup "hooks" orgSet
    , services = fromMaybe [] $ Mp.lookup "services" orgSet
    , types = fromMaybe [] $ Mp.lookup "types" orgSet
    , tests = fromMaybe [] $ Mp.lookup "tests" orgSet
    , stories = fromMaybe [] $ Mp.lookup "stories" orgSet
    , public = fromMaybe [] $ Mp.lookup "public" orgSet
    , build = fromMaybe [] $ Mp.lookup "build" orgSet
    , deploy = fromMaybe [] $ Mp.lookup "deploy" orgSet
    , miscs = miscFiles <> fromMaybe [] (Mp.lookup "miscs" orgSet)
  }
  where
  organizeAFile :: Fs.FileWithPath -> OrgMap -> OrgMap
  organizeAFile (dirPath, aFile) orgMap
    | dirPath == "" = case aFile of
        Fs.KnownFile aKind aPath ->
            if "next-env.d" `isPrefixOf` aPath then
              Mp.insertWith (<>) "envConfig" [(dirPath, aFile)] orgMap
            else if "next.config" `isPrefixOf` aPath
                  || "tailwind.config" `isPrefixOf` aPath
                  || "postcss.config" `isPrefixOf` aPath
                  || "prettier.config" `isPrefixOf` aPath then
              Mp.insertWith (<>) "nextConfig" [(dirPath, aFile)] orgMap
            else if "package" `isPrefixOf` aPath then
              Mp.insertWith (<>) "packageConfig" [(dirPath, aFile)] orgMap
            else if "tsconfig" `isPrefixOf` aPath then
              Mp.insertWith (<>) "tsConfig" [(dirPath, aFile)] orgMap
            else
              Mp.insertWith (<>) "misc" [(dirPath, aFile)] orgMap
        -- This case isn't supposed to happen.
        _ -> orgMap
    | "src/components" `isPrefixOf` dirPath = Mp.insertWith (<>) "components" [(drop 14 dirPath, aFile)] orgMap
    | "src/pages" `isPrefixOf` dirPath = Mp.insertWith (<>) "pages" [(drop 10 dirPath, aFile)] orgMap
    | "src/api" `isPrefixOf` dirPath = Mp.insertWith (<>) "api" [(drop 8 dirPath, aFile)] orgMap
    | "src/lib" `isPrefixOf` dirPath = Mp.insertWith (<>) "lib" [(drop 8 dirPath, aFile)] orgMap
    | "src/styles" `isPrefixOf` dirPath = Mp.insertWith (<>) "styles" [(drop 11 dirPath, aFile)] orgMap
    | "src/utils" `isPrefixOf` dirPath = Mp.insertWith (<>) "utils" [(drop 10 dirPath, aFile)] orgMap
    | "src/hooks" `isPrefixOf` dirPath = Mp.insertWith (<>) "hooks" [(drop 10 dirPath, aFile)] orgMap
    | "src/services" `isPrefixOf` dirPath = Mp.insertWith (<>) "services" [(drop 13 dirPath, aFile)] orgMap
    | "src/types" `isPrefixOf` dirPath = Mp.insertWith (<>) "types" [(drop 10 dirPath, aFile)] orgMap
    | "src/tests" `isPrefixOf` dirPath = Mp.insertWith (<>) "tests" [(drop 10 dirPath, aFile)] orgMap
    | "src/stories" `isPrefixOf` dirPath = Mp.insertWith (<>) "stories" [(drop 12 dirPath, aFile)] orgMap
    | "public" `isPrefixOf` dirPath = Mp.insertWith (<>) "public" [(drop 7 dirPath, aFile)] orgMap
    | "build" `isPrefixOf` dirPath = Mp.insertWith (<>) "build" [(drop 6 dirPath, aFile)] orgMap
    | "deploy" `isPrefixOf` dirPath = Mp.insertWith (<>) "deploy" [(drop 7 dirPath, aFile)] orgMap
    | otherwise = Mp.insertWith (<>) "miscs" [(dirPath, aFile)] orgMap

analyseStaticProject :: RunOptions -> ProjectDefinition -> NextJSComponents -> Either GenError WorkPlan
analyseStaticProject rtOpts (ProjectDefinition baseDir (Site NextStatic) [] pathFiles) content =
  let
    dbgContent = "NextJS Site project definition: " <> pack (show content)
  in
  Left $ SimpleMsg dbgContent
  -- Right $ WorkPlan { destDir = "", items = [] }


analyseWebAppProject :: RunOptions -> ProjectDefinition -> NextJSComponents -> Either GenError WorkPlan
analyseWebAppProject rtOpts (ProjectDefinition baseDir (WebApp NextJS) [] pathFiles) content =
  let
    dbgContent = "NextJS WebApp project definition: " <> pack (show content)
  in
  Left $ SimpleMsg dbgContent
  -- Right $ WorkPlan { destDir = "", items = [] }
