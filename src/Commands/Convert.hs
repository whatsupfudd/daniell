module Commands.Convert where

import qualified Conclusion as Ccl
import qualified Options.Runtime as Rto

convertHu :: Rto.RunOptions -> IO Ccl.Conclusion
convertHu rtOpts =
  putStrLn "@[convertHu] starting." >> pure Ccl.NilCcl
