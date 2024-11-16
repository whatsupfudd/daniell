{-# LANGUAGE MultiParamTypeClasses #-}
module ProjectDefinition.NextJS.Work (runWorkItem) where

import Control.Exception (try)

import qualified Data.ByteString as Bs
import Data.Text (pack)

import Conclusion (GenError(..))
import ProjectDefinition.NextJS.Types
import Generator.Types (ExecSystem (..), WorkPlan (..))
import Template.NextJS (compileTemplate)


instance ExecSystem NxEngine NxContext NxWorkItem where
  runWorkItem rtOpts engine context wItem = do
    case wItem of
      ExecTmplForRouteNx dirPath -> do
        putStrLn $ "@[NextJS.runWorkItem] src: " <> show wItem
        pure $ Right context
      ExecTmplForContentNx filePath -> do
        eiTemplSource <- try $ Bs.readFile filePath :: IO (Either IOError Bs.ByteString)
        case eiTemplSource of
          Left err ->
            let
              errMsg =  "@[NextJS.runWorkItem] readFile err: " <> show err
            in do
            putStrLn errMsg
            pure . Left . SimpleMsg $ pack errMsg
          Right templSource ->
            compileTemplate rtOpts (engine, context, wItem) (filePath, templSource)
        pure $ Right context
