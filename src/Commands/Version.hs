module Commands.Version where

import qualified Conclusion as Ccl
import qualified Options.RunOptions as Rto

versionHu :: Rto.RunOptions -> IO Ccl.Conclusion
versionHu rtOpts =
  putStrLn "@[versionHu] starting." >> pure Ccl.NilCcl
