module Commands.Mod where

import qualified Conclusion as Ccl
import qualified Options.Runtime as Rto

modHu :: Rto.RunOptions -> IO Ccl.Conclusion
modHu rtOpts =
  putStrLn "@[modHu] starting." >> pure Ccl.NilCcl
