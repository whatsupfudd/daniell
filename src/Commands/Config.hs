module Commands.Config where

import qualified Conclusion as Ccl
import qualified Options.Runtime as Rto

configHu :: Rto.RunOptions -> IO Ccl.Conclusion
configHu rtOpts =
  putStrLn "@[configHu] starting." >> pure Ccl.NilCcl
