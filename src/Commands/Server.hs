module Commands.Server where

import Control.Monad (forM_, forM)

import qualified Options.Runtime as Rto
import Generator.Logic (createSite)
import qualified Conclusion as Ccl
import qualified WebServer.Servant as WSrv


serverCmd :: Rto.RunOptions -> IO Ccl.Conclusion
serverCmd rtOpts = do
  mbStaticSite <- createSite rtOpts
  case mbStaticSite of
    Left genErr ->
      putStrLn $ "@[serverHu] err: " <> show genErr
    Right aSite -> do
      putStrLn "@[serverHu] starting listener."
      WSrv.listen rtOpts
  pure Ccl.NilCcl
