module Commands.Build where

import Control.Monad (forM_, forM)

import Data.Text (unpack)
import System.Directory as SE

import qualified Options.Runtime as Rto
import Options.Types (BuildOptions (..), ProjectKind (..), SubProjKind (..))
import qualified Generator.Logic as Gen
import qualified Conclusion as Ccl
import qualified WebServer.Servant as WSrv
import Data.Maybe (fromMaybe)


buildCmd :: BuildOptions -> Rto.RunOptions -> IO Ccl.Conclusion
buildCmd buildOpts rtOpts =
  case buildOpts.projKind of
    SitePK -> buildSite rtOpts buildOpts
    WebAppPK -> buildWebApp rtOpts buildOpts
    LocalAppPK -> buildLocalApp rtOpts buildOpts


buildLocalApp :: Rto.RunOptions -> BuildOptions -> IO Ccl.Conclusion
buildLocalApp rtOpts buildOpts = do
  putStrLn "@[buildLocalApp] starting."
  pure Ccl.NilCcl


buildWebApp :: Rto.RunOptions -> BuildOptions -> IO Ccl.Conclusion
buildWebApp rtOpts buildOpts = do
  putStrLn "@[buildWebApp] starting."
  pure Ccl.NilCcl


buildSite :: Rto.RunOptions -> BuildOptions -> IO Ccl.Conclusion
buildSite rtOpts buildOpts =
  let
    mbSubpKind = case buildOpts.subKind of
      Just "fuddle" -> Just FuddleSP
      Just "hugo" -> Just HugoSP
      Just "nextjs" -> Just NextSP
      Just "gatsby" -> Just GatsbySP
      _ -> Nothing
  in
  case mbSubpKind of
    Nothing -> do
      putStrLn $ "@[buildSite] unknown subproject kind: " <> unpack (fromMaybe "<missing>" buildOpts.subKind) <> "."
      pure Ccl.NilCcl
    Just aSubpKind -> do
      srcDir <- case buildOpts.srcDir of
          Just "." -> SE.getCurrentDirectory
          Just aStr -> pure $ unpack aStr
          Nothing -> SE.getCurrentDirectory
      mbSite <- Gen.buildSite (rtOpts { Rto.baseDir = srcDir }) aSubpKind
      case mbSite of
        Left genErr ->
          putStrLn $ "@[buildSite] buildCmd: " <> show genErr
        Right aSite -> do
          putStrLn "@[buildSite] build done."
      pure Ccl.NilCcl
