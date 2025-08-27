module ProjectDefinition.Haskell.Work where

import Conclusion (GenError (..), Conclusion (..))
import Options.Runtime (RunOptions)
import Options.Types (TechKind (..))


runWorkItem :: RunOptions -> FilePath -> IO (Either GenError ())
runWorkItem rtOpts srcDir = do
  putStrLn $ "@[runWorkItem] starting on: " <> srcDir
  pure $ Right ()