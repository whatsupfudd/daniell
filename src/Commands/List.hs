module Commands.List where

import qualified Conclusion as Ccl
import qualified Options.RunOptions as Rto

listHu :: Rto.RunOptions -> IO Ccl.Conclusion
listHu rtOpts =
  putStrLn "@[listHu] starting." >> pure Ccl.NilCcl
