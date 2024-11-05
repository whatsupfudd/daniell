module Template.Hugo where

import qualified Data.ByteString as Bs
import Data.List (sortOn)
import qualified Data.List.NonEmpty as Ne
import Data.Text (pack)
import qualified Data.Text.Encoding as T
import qualified Data.Vector as V
import qualified Data.Map as Mp

import Options.Runtime (RunOptions (..))
import Conclusion (GenError (..))

import qualified Cannelle.Hugo.Parse as Cnl
import qualified Cannelle.Hugo.AST as Cnl


import qualified Cannelle.Hugo.Parse as Hg
import qualified Cannelle.Hugo.Exec as Hg
import Cannelle.VM.Context (ConstantValue)

import ProjectDefinition.Hugo.Types ( HgWorkItem, HgContext(..), HgEngine )

compileTemplate :: RunOptions -> (HgEngine, HgContext, HgWorkItem) -> (FilePath, Bs.ByteString) -> IO (Either GenError HgContext)
compileTemplate rtOpts (engine, context, wItem) (tmplPath, templSource) = do
  -- For now, the FileUnit is not saved automatically.
  putStrLn $ "@[compileTemplate] compiling: " <> tmplPath
  fileUnit <- Hg.parseFromContent False tmplPath templSource Nothing
  case fileUnit of
    Left errMsg -> do
      putStrLn $ "@[runWorkItem.HgEngine] parseFromContent err: " <> errMsg
      pure . Left . SimpleMsg $ "@[runWorkItem.HgEngine] parseFromContent err: " <> pack errMsg
    Right fileUnit -> do
      -- Testing:
      -- Hg.exec fileUnit
      pure . Right $ context { compFileUnits = Mp.insert tmplPath fileUnit context.compFileUnits }
  