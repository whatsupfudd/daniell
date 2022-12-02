module Commands.Publish where

import qualified Conclusion as Ccl
import qualified Options.RunOptions as Rto

publishDan :: Rto.RunOptions -> IO Ccl.Conclusion
publishDan rtOpts =
  putStrLn "@[publishDan] starting." >> pure Ccl.NilCcl
