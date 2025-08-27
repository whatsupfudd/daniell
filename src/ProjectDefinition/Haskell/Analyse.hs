module ProjectDefinition.Haskell.Analyse where

import qualified Data.Map as Mp
import Data.Maybe (fromMaybe)
import Data.Text (Text, pack)

import System.FilePath (takeExtension)

import Conclusion (GenError (..), Conclusion (..))
import Options.Runtime (RunOptions)
import Options.Types (TechKind (..))
import ProjectDefinition.Types (FileSet)

import qualified FileSystem.Types as Fs
import qualified FileSystem.Explore as Fs
import ProjectDefinition.Haskell.Types


analyseProject :: RunOptions -> TechKind -> FilePath -> IO (Either GenError ())
analyseProject rtOpts techKind srcDir = do
  eiPathFiles <- Fs.loadFolderTree srcDir
  case eiPathFiles of
    Left err -> pure . Left . SimpleMsg $ "Error loading folder tree: " <> pack err
    Right pathFiles ->
      let
        content = organizeFiles rtOpts pathFiles 
      in do
      putStrLn $ "buildInfo: " <> show content.buildInfo
      putStrLn $ "modules: " <> show content.modules
      putStrLn $ "otherLogic: " <> show content.otherLogic
      pure $ Right ()


organizeFiles :: RunOptions -> Fs.PathFiles -> HaskellComponents
organizeFiles rtOpts pathFiles =
  let
    orgSet = foldl (\accum (dirPath, files) -> foldr (organizeAFile dirPath) accum files) Mp.empty pathFiles
  in
  HaskellComponents {
    buildInfo = maybe Mp.empty organizeBuildFiles $ Mp.lookup "buildInfo" orgSet
    , modules = fromMaybe [] $ Mp.lookup "modules" orgSet
    , otherLogic = fromMaybe [] $ Mp.lookup "otherLogic" orgSet
  }
  where
  organizeAFile :: FilePath -> Fs.ExtFileItem -> Mp.Map Text [(FilePath, Fs.FileItem)] -> Mp.Map Text [(FilePath, Fs.FileItem)]
  organizeAFile dirPath xFileItem aMap =
    let
      fileItem = case xFileItem of
        Fs.ReferFI fileItem -> fileItem
        Fs.ContentFI fileItem content -> fileItem
    in
    case fileItem of
      Fs.KnownFile aKind aPath -> case aKind of
        Fs.Haskell -> Mp.insertWith (<>) "modules" [(dirPath, fileItem)] aMap
        Fs.Yaml -> case aPath of
          "stack.yaml" -> Mp.insertWith (<>) "buildInfo" [(dirPath, fileItem)] aMap
          "package.yaml" -> Mp.insertWith (<>) "buildInfo" [(dirPath, fileItem)] aMap
          _ -> aMap
        Fs.Cpp -> Mp.insertWith (<>) "otherLogic" [(dirPath, fileItem)] aMap
        Fs.CppHeader -> Mp.insertWith (<>) "otherLogic" [(dirPath, fileItem)] aMap
        _ -> aMap
      Fs.MiscFile aPath ->
        if takeExtension aPath == ".cabal" then
          Mp.insertWith (<>) "buildInfo" [(dirPath, fileItem)] aMap
        else
          aMap
  organizeBuildFiles ::[Fs.FileWithPath] -> Mp.Map FilePath BuildInfo
  organizeBuildFiles =
    foldl (\accum (dirPath, fileItem) ->
        case Mp.lookup dirPath accum of
          Nothing ->
            case fileItem of
              Fs.KnownFile fKind fPath ->
                    case fPath of
                      "stack.yaml" -> Mp.insert dirPath (BuildInfo (Just fPath) Nothing Nothing) accum
                      "package.yaml" -> Mp.insert dirPath (BuildInfo  Nothing Nothing (Just fPath)) accum
                      _ -> accum
              Fs.MiscFile aPath ->
                if takeExtension aPath == ".cabal" then
                  Mp.insert dirPath (BuildInfo Nothing (Just aPath) Nothing) accum
                else
                  accum
          Just aBuildInfo ->
            let
              updBuildInfo = case fileItem of
                Fs.KnownFile fKind fPath ->
                  case fPath of
                    "stack.yaml" -> Just $ aBuildInfo { stack = Just fPath }
                    "package.yaml" -> Just $aBuildInfo { package = Just fPath }
                    _ -> Nothing
                Fs.MiscFile aPath ->
                  if takeExtension aPath == ".cabal" then
                    Just $ aBuildInfo { package = Just aPath }
                  else
                    Nothing
              in
              case updBuildInfo of
                Nothing -> accum
                Just aBuildInfo -> Mp.insert dirPath aBuildInfo accum
      ) Mp.empty
