module Commands.Gen where

import qualified Conclusion as Ccl
import qualified Options.RunOptions as Rto

genHu :: Rto.RunOptions -> IO Ccl.Conclusion
genHu rtOpts =
  putStrLn "@[genHu] starting." >> pure Ccl.NilCcl
