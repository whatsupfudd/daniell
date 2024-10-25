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

import Cannelle.Hugo.Types (CompContext (..), CompFunction (..), CompConstant (..))
import Cannelle.Hugo.Assembler (assemble, convertCompCteToTempl)
import Cannelle.Hugo.Compiler (compileStatements, FullCompContext (..))
import Cannelle.Hugo.AST (RawStatement (..))
import qualified Cannelle.Template.Types as Ct
import qualified Cannelle.Hugo.TemplateConv as Cv
import qualified Cannelle.VM.Context as VM
import qualified Cannelle.VM.Engine as VM
import Cannelle.VM.Context (ConstantValue)

import ProjectDefinition.Hugo.Types ( HgWorkItem, HgContext(..), HgEngine )

compileTemplate :: RunOptions -> (HgEngine, HgContext, HgWorkItem) -> (FilePath, Bs.ByteString) -> IO (Either GenError HgContext)
compileTemplate rtOpts (engine, context, wItem) (tmplPath, templSource) = do
  rezB <- Cnl.parseTemplateSource (Just tmplPath) templSource
  case rezB of
    Left errMsg ->
      let
        nErrMsg = "@[runWorkItem.HgEngine] parseTemplateSource err: " <> errMsg
      in do
      putStrLn nErrMsg
      pure . Left . SimpleMsg $ pack nErrMsg
    Right templateElements -> case Cnl.convertElements templateElements of
      Left errMsg ->
        let
          nErrMsg = "@[runWorkItem.HgEngine] convertElements err: " <> errMsg
        in do
        putStrLn nErrMsg
        pure . Left . SimpleMsg $ pack nErrMsg
      Right rezC -> do
        putStrLn "@[runWorkItem.HgEngine] convertElements: "
        Cnl.printStatements rezC
        case compileStatements "$main" rezC of
          Left err -> do
            putStrLn $ "@[runWorkItem.HgEngine] compileCode err: " <> show err
            pure . Left . SimpleMsg $ "@[runWorkItem.HgEngine] compileCode err: " <> pack (show err)
          Right compiledCode -> do
            putStrLn $ "@[runWorkItem.HgEngine] compileCode: " <> show compiledCode
            let
              template = Ct.TemplateDef {
                    name = Just . T.encodeUtf8 . pack $ tmplPath
                    , description = Nothing
                    , constants = V.fromList $ map (Cv.hugoCteToTmpl . fst) . sortOn snd $ Mp.elems compiledCode.constants
                    , definitions = V.singleton (Cv.hugoFctToTmpl (Ne.head compiledCode.curFctDef))
                              <> V.fromList (map (Cv.hugoFctToTmpl . fst) . Mp.elems $ compiledCode.functions)
                    , routing = V.empty
                    , imports = V.empty
                  }

              vmModule = VM.VMModule {
                  functions = V.fromList . map (\(compFct, fID) -> 
                      VM.FunctionDef {
                        moduleID = 0
                        , fname = T.decodeUtf8 $ case compFct.name of
                            "$main" -> compFct.name
                            _ -> "$" <> compFct.name
                        , args = Nothing
                        , returnType = VM.FirstOrderSO VM.IntTO
                        , body = case assemble compFct of
                            Left err -> VM.ByteCode V.empty
                            Right compiled -> VM.ByteCode compiled
                      }
                    ) $ Mp.elems compiledCode.functions
                , constants = V.fromList . map (convertCompCteToTempl . fst) $ Mp.elems compiledCode.constants
                , externModules = Mp.empty
              }
            rezE <- VM.execModule vmModule
            case rezE of
              Left errMsg -> do
                putStrLn $ "@[runWorkItem.HgEngine] execModule err: " <> errMsg
              Right (VM.ExecResult vmCtxt) -> do
                putStrLn $ "@[runWorkItem.HgEngine] exec rez: " <> show vmCtxt
            pure . Right $ context { compTemplates = Mp.insert tmplPath template context.compTemplates }
