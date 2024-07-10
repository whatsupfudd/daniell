module Commands.New where

import Data.Text (Text, unpack)
import Data.List (singleton)

import qualified Conclusion as Ccl
import Options.Runtime (RunOptions (..))
import Options.Types (NewOptions (..))
import FileSystem.Types (PathNode (..), FileItem (..), FileKind (..), getItemPath)
import ProjectDefinition.Defaults (defaultTemplate)
import ProjectDefinition.Logic (createProject)
import Template.Project (loadTemplate)
import Template.Types (ProjectTempl (..))
import Utils (splitResults)

import Markup.Haskell (testTreeSitter)


newCmd :: NewOptions -> RunOptions -> IO Ccl.Conclusion
newCmd options rtOpts = do
  testTreeSitter "/Users/lhugo/.fudd/daniell/templates/fuddapp/src/Options/Cli.hs"

  putStrLn $ "@[newCmd] Options: " <> show options
  rezTemplates <- mapM (parseProjectTempl rtOpts) options.templates
  let
    (errTemplates, userTemplates) = splitResults rezTemplates
  case errTemplates of
    -- No errors, keep going.
    [] -> do
      -- putStrLn $ "Parsed templates: " <> show userTemplates
      -- if there's no 'no-default template' instruction in the specified templates, load the default template.
      -- Right . (<>) userTemplates . singleton <$> 
      rezA <- if False then pure $ Right userTemplates else do
        rezB <- parseProjectTempl rtOpts (defaultTemplate rtOpts options.projKind)
        case rezB of
          Left errMsg -> pure . Left $ show errMsg
          Right defTempl -> pure . Right $ userTemplates <> [ defTempl ]
      case rezA of
        Left errMsg -> pure $ Ccl.ErrorCcl $ "@[newCmd] error loading default template: " <> show errMsg
        Right allTemplates -> do
          rezB <- createProject rtOpts options allTemplates
          case rezB of
            Left errMsg -> pure $ Ccl.ErrorCcl $ "@[newCmd] error creating project: " <> show errMsg
            Right _ -> pure Ccl.NilCcl
    -- Errors while reading templates, abort.
    _ -> do
      putStrLn $ "@[newCmd] Template loading error: " <> show errTemplates
      pure $ Ccl.ErrorCcl $ "@[newCmd] error loading templates: " <> show errTemplates


parseProjectTempl :: RunOptions -> FilePath -> IO (Either Ccl.GenError ProjectTempl)
parseProjectTempl rtOpts tPath =
  let
    fullPath = case tPath of
      '.' : rest -> tPath
      '/' : rest -> tPath
      _ -> rtOpts.templateDir <> "/" <> tPath
  in do
  rezA <- loadTemplate rtOpts fullPath
  case rezA of
    Left errMsg -> pure $ Left $ Ccl.SimpleMsg errMsg
    Right aTempl -> pure $ Right aTempl

