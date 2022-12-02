module Commands.Env where

import qualified Conclusion as Ccl
import qualified Options.RunOptions as Rto

envHu :: Rto.RunOptions -> IO Ccl.Conclusion
envHu rtOpts =
  putStrLn "@[envHu] starting." >> pure Ccl.NilCcl
