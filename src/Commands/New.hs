module Commands.New where

import qualified Conclusion as Ccl
import qualified Options.RunOptions as Rto

newHu :: Rto.RunOptions -> IO Ccl.Conclusion
newHu rtOpts =
  putStrLn "@[newHu] starting." >> pure Ccl.NilCcl
