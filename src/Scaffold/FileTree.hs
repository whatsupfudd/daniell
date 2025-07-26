module Scaffold.FileTree where

import Control.Monad (unless)

import Data.Text (Text, pack)

import qualified System.Directory as SE

-- import Cannelle.Templog.Types (Function (..))

import qualified FileSystem.Explore as Exp
import qualified Conclusion as Ccl
import qualified Options.Types as Op
import qualified Options.Runtime as Op
import qualified Cannelle.Templog.Types as Tpl
import Scaffold.Types (ScaffoldBundle (..))


loadTree :: Op.RunOptions -> FilePath -> IO (Either Text ScaffoldBundle)
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

loadTreeLogic :: Op.RunOptions -> FilePath -> IO (Either Text ScaffoldBundle)
loadTreeLogic rtOpts path =
  -- TODO: load the template file, and create a ScaffholdTempl from it.
  pure . Right $ ScaffoldBundle path Nothing Nothing mempty mempty Tpl.Noop


loadTreeContent :: Op.RunOptions -> FilePath -> IO (Either Text ScaffoldBundle)
loadTreeContent rtOpts path = do
  eiTree <- Exp.loadFolderTree path
  case eiTree of
    Left err -> pure . Left $ pack err
    Right fTree -> do
      -- putStrLn $ "fTree: " <> show fTree
      pure $ Right $ ScaffoldBundle path Nothing Nothing fTree mempty Tpl.Noop


mergeBundles :: [ScaffoldBundle] -> ScaffoldBundle
mergeBundles =
  foldl1 mergeBundle
  where
    -- TODO: define how two project templates are merged together.
    mergeBundle :: ScaffoldBundle -> ScaffoldBundle -> ScaffoldBundle
    mergeBundle a b = a

