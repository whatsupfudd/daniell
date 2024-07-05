module Commands.New where

import qualified Conclusion as Ccl
import qualified Options.Cli as Opt
import qualified Options.Runtime as Rto

{- | Use archetype/* to create a new document in the content section, based on the
 value passed in the RunOptions
-}

newCmd :: Opt.NewOptions -> Rto.RunOptions -> IO Ccl.Conclusion
newCmd options rtOpts =
  putStrLn "@[newCmd] starting." >> pure Ccl.NilCcl
