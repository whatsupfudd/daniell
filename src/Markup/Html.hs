module Markup.Html where

import Data.Text (Text, pack)
import qualified Data.Text.IO as T

import System.FilePath ((</>))

import Conclusion (GenError (..))

import FileSystem.Types (FileWithPath, FileItem (..), getItemPath)
import Markup.FrontMatter (parseFrontMatter)
import Markup.Types

parse :: FilePath -> FileWithPath -> IO (Either GenError MarkupPage)
parse srcDir file@(dirPath, fileItem) = do
  let
    fullFilePath = srcDir </> dirPath </> getItemPath fileItem
  {-
      fileName = takeBaseName $ getItemPath filePath
      -- outPath = addExtension (joinPath ["/tmp", fileName]) "html"
  -}
  txt <- T.readFile fullFilePath
  case parseFrontMatter txt of
    Left errMsg -> do
      putStrLn $ "@[parse] parseFrontMatter err: " <> errMsg
      pure . Left . SimpleMsg $ pack errMsg
    Right (mbFrontMatter, rest) -> do
      -- TODO: do real stuff:
      -- putStrLn $ "@[parse] parseFrontMatter: " <> show mbFrontMatter
      pure . Right $ MarkupPage {
                item = file
              , frontMatter = mbFrontMatter
              , content = Content RawHtml (Just rest)
            }
