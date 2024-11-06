module Commands.Build where

import Control.Monad (forM_, forM)

import Data.Text (Text, unpack)
import System.Directory as SE

import qualified Options.Runtime as Rto
import Options.Types (BuildOptions (..), BuildKind (..), SiteOptions (..), WebAppOptions (..))
import qualified Generator.Logic as Gen
import qualified Conclusion as Ccl
import qualified WebServer.Servant as WSrv
import Data.Maybe (fromMaybe)


buildCmd :: BuildOptions -> Rto.RunOptions -> IO Ccl.Conclusion
buildCmd buildOpts rtOpts =
  case buildOpts.kind of
    SiteBK siteOpts -> buildSite rtOpts buildOpts.srcDir siteOpts
    WebAppBK waOpts -> buildWebApp rtOpts buildOpts waOpts
    LocalAppBK -> buildLocalApp rtOpts buildOpts


buildLocalApp :: Rto.RunOptions -> BuildOptions -> IO Ccl.Conclusion
buildLocalApp rtOpts buildOpts = do
  putStrLn "@[buildLocalApp] starting."
  pure Ccl.NilCcl


buildWebApp :: Rto.RunOptions -> BuildOptions -> WebAppOptions -> IO Ccl.Conclusion
buildWebApp rtOpts buildOpts waOpts = do
  srcDir <- case buildOpts.srcDir of
    Just "." -> SE.getCurrentDirectory
    Just aStr -> pure $ unpack aStr
    Nothing -> SE.getCurrentDirectory
  mbSite <- Gen.buildWebApp (rtOpts { Rto.baseDir = srcDir }) waOpts
  case mbSite of
    Left genErr ->
      putStrLn $ "@[buildWebApp] buildCmd: " <> show genErr
    Right aSite -> do
      putStrLn "@[buildWebApp] build done."

  pure Ccl.NilCcl


buildSite :: Rto.RunOptions -> Maybe Text -> SiteOptions -> IO Ccl.Conclusion
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
