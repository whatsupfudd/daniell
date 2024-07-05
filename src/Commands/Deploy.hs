module Commands.Deploy where

import qualified Conclusion as Ccl
import qualified Options.Runtime as Rto

deployHu :: Rto.RunOptions -> IO Ccl.Conclusion
deployHu rtOpts =
  putStrLn "@[deployHu] starting." >> pure Ccl.NilCcl
