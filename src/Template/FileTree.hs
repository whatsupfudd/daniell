module Template.FileTree where

import Control.Monad (unless)

import Data.Text (Text, pack)

import qualified System.Directory as SE

import qualified FileSystem.Explore as Exp
import qualified Conclusion as Ccl
import qualified Options.Types as Op
import qualified Options.Runtime as Op
import Template.Types


loadTree :: Op.RunOptions -> FilePath -> IO (Either Text ScaffoldTempl)
loadTree rtOpts path = do
  if isSuffix ".dtmpl" path then
      loadTreeLogic rtOpts path
  else do
    dirTest <- SE.doesDirectoryExist path
    if dirTest then
      loadTreeContent rtOpts path
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

loadTreeLogic :: Op.RunOptions -> FilePath -> IO (Either Text ScaffoldTempl)
loadTreeLogic rtOpts path =
  -- TODO: load the template file, and create a ScaffholdTempl from it.
  pure . Right $ ScaffoldTempl path Nothing Nothing mempty mempty Noop


loadTreeContent :: Op.RunOptions -> FilePath -> IO (Either Text ScaffoldTempl)
loadTreeContent rtOpts path = do
  eiTree <- Exp.loadFolderTree path
  case eiTree of
    Left err -> pure . Left $ pack err
    Right fTree -> do
      -- putStrLn $ "fTree: " <> show fTree
      pure $ Right $ ScaffoldTempl path Nothing Nothing fTree mempty Noop


mergeTemplates :: [ScaffoldTempl] -> ScaffoldTempl
mergeTemplates =
  foldl1 mergeTemplate
  where
    -- TODO: define how two project templates are merged together.
    mergeTemplate :: ScaffoldTempl -> ScaffoldTempl -> ScaffoldTempl
    mergeTemplate a b = a

