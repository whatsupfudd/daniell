module Commands.Import where

import qualified Conclusion as Ccl
import qualified Options.Runtime as Rto

importHu :: Rto.RunOptions -> IO Ccl.Conclusion
importHu rtOpts =
  putStrLn "@[importHu] starting." >> pure Ccl.NilCcl
