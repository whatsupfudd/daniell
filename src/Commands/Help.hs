module Commands.Help where

import qualified Conclusion as Ccl
import qualified Options.RunOptions as Rto

helpHu :: Rto.RunOptions -> IO Ccl.Conclusion
helpHu rtOpts =
  putStrLn "@[helpHu] starting." >> pure Ccl.NilCcl
