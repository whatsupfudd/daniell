module Options.FileOptions where

import qualified Data.Int as DI


defaultConfigFilePath = ".daniell.yaml"

data FileOptions = FileOptions {
  debug :: Maybe DI.Int32
 }


parseFileOptions :: FilePath -> IO (Either String FileOptions)
parseFileOptions filePath =
  let
    tmpOptions = FileOptions {
        debug = Nothing
      }
  in
  pure . Right $ tmpOptions
