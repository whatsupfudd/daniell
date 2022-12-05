{-# LANGUAGE TemplateHaskell #-}

module Commands.Version where

import qualified Conclusion as Ccl
import qualified Options.RunOptions as Rto

import Data.Version (showVersion)
import Development.GitRev (gitHash, gitCommitDate)
import Paths_daniell (version)

versionHu :: Rto.RunOptions -> IO Ccl.Conclusion
versionHu rtOpts = do
  putStrLn $ "Version: " <> showVersion version <> ", git: " <> $(gitHash) <> " (" <> $(gitCommitDate) <> ")."
  pure Ccl.NilCcl
