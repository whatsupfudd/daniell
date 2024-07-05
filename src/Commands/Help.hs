module Commands.Help where

import qualified Conclusion as Ccl
import qualified Options.Runtime as Rto

helpHu :: Rto.RunOptions -> IO Ccl.Conclusion
helpHu rtOpts =
  putStrLn "@[helpHu] starting." >> pure Ccl.NilCcl
