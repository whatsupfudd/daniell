module Commands.Analyse where

import Control.Monad (forM_, forM)

import Data.Text (Text, unpack)
import System.Directory as SE

import qualified Options.Runtime as Rto
import Options.Types (AnalyseOptions (..), TechKind (..))
import qualified Generator.Logic as Gen
import qualified Conclusion as Ccl
import qualified WebServer.Servant as WSrv
import Data.Maybe (fromMaybe)


analyseCmd :: AnalyseOptions -> Rto.RunOptions -> IO Ccl.Conclusion
analyseCmd analyseOpts rtOpts = do
  srcDir <- case analyseOpts.projDir of
    Just "." -> SE.getCurrentDirectory
    Just aStr -> pure aStr
    Nothing -> SE.getCurrentDirectory
  eiRez <- Gen.analyseProject rtOpts analyseOpts.techKind srcDir
  pure Ccl.NilCcl


{-
buildSite :: Rto.RunOptions -> Maybe Text  -> IO Ccl.Conclusion
buildSite rtOpts srcDir siteOpts = do
  srcDir <- case srcDir of
    Just "." -> SE.getCurrentDirectory
    Just aStr -> pure $ unpack aStr
    Nothing -> SE.getCurrentDirectory
  mbSite <- Gen.buildSite (rtOpts { Rto.baseDir = srcDir }) siteOpts
  case mbSite of
    Left genErr ->
      putStrLn $ "@[buildSite] buildCmd: " <> show genErr
    Right aSite -> do
      putStrLn "@[buildSite] build done."
  pure Ccl.NilCcl
-}
