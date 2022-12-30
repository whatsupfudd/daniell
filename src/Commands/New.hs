module Commands.New where

import qualified Conclusion as Ccl
import qualified Options.RunOptions as Rto

{- | Use archetype/* to create a new document in the content section, based on the
 value passed in the RunOptions
-}

newHu :: Rto.RunOptions -> IO Ccl.Conclusion
newHu rtOpts =
  putStrLn "@[newHu] starting." >> pure Ccl.NilCcl
