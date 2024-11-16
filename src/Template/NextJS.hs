module Template.NextJS where

import qualified Data.ByteString as Bs
import qualified Data.Map as Mp
import Data.Text (pack)

import Cannelle.React.Parse (parseFromContent)
import Cannelle.React.Print (printReactContext, printContextStats)

import Options.Runtime (RunOptions (..))
import ProjectDefinition.NextJS.Types
import Conclusion (GenError (..))

compileTemplate :: RunOptions -> (NxEngine, NxContext, NxWorkItem) -> (FilePath, Bs.ByteString) -> IO (Either GenError NxContext)
compileTemplate rtOpts (engine, context, wItem) (tmplPath, templSource) = do
  -- For now, the FileUnit is not saved automatically.
  putStrLn $ "@[compileTemplate.Nx] compiling: " <> tmplPath
  fileUnit <- parseFromContent True tmplPath templSource Nothing
  case fileUnit of
    Left errMsg -> do
      putStrLn $ "@[runWorkItem.NxEngine] parseFromContent err: " <> errMsg
      pure . Left . SimpleMsg $ "@[runWorkItem.NxEngine] parseFromContent err: " <> pack errMsg
    Right fileUnit -> do
      -- Testing:
      -- printReactContext templSource fileUnit
      -- printContextStats fileUnit
      pure . Right $ context { compFileUnits = Mp.insert tmplPath fileUnit context.compFileUnits }
  