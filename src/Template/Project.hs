module Template.Project where

import Data.Text (Text, pack)

import System.Directory (doesDirectoryExist)

import qualified FileSystem.Explore as Exp
import qualified Options.Runtime as Rt
import Template.Types


loadTemplate :: Rt.RunOptions -> FilePath -> IO (Either Text ProjectTempl)
loadTemplate rtOpts path = do
  if isSuffix ".dtmpl" path then
      loadProjectFile rtOpts path
  else do
    dirTest <- doesDirectoryExist path
    if dirTest then
      loadProjectDir rtOpts path
    else
      pure . Left $ "Invalid template file or directory: " <> pack path
  where
    isSuffix :: String -> FilePath -> Bool
    isSuffix suffix path = suffix == reverse (take (length suffix) (reverse path))

{- Project template: 
  path :: FilePath
  , description :: Maybe Text
  , structure :: PathFiles
  , parameters :: ParameterMap
  , logic :: Function
-}

loadProjectFile :: Rt.RunOptions -> FilePath -> IO (Either Text ProjectTempl)
loadProjectFile rtOpts path =
  -- TODO: load the template file, and create a ProjectTempl from it.
  pure . Right $ ProjectTempl path Nothing Nothing mempty mempty Noop


loadProjectDir :: Rt.RunOptions -> FilePath -> IO (Either Text ProjectTempl)
loadProjectDir rtOpts path = do
  eiTree <- Exp.loadFolderTree path
  case eiTree of
    Left err -> pure . Left $ pack err
    Right fTree -> do
      -- putStrLn $ "fTree: " <> show fTree
      pure $ Right $ ProjectTempl path Nothing Nothing fTree mempty Noop


mergeTemplates :: [ProjectTempl] -> ProjectTempl
mergeTemplates =
  foldl1 mergeTemplate
  where
    mergeTemplate :: ProjectTempl -> ProjectTempl -> ProjectTempl
    mergeTemplate a b = a

