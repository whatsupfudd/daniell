module Options.Config where

import Control.Monad (forM_)

import qualified Data.ByteString as BS
import qualified Data.HashMap.Strict as Hms
import qualified Data.List.NonEmpty as Nemp
import qualified Data.Text as DT
import qualified Data.Text.Encoding as DT
import qualified Toml.Parser as Tp
import qualified Toml.Type.TOML as Tp
import qualified Toml.Type.Key as Tp


parseToml :: FilePath -> IO (Either Tp.TomlParseError Tp.TOML)
parseToml filePath = do
  fileContent <- BS.readFile filePath
  let
    eiRez = Tp.parse (DT.decodeUtf8 fileContent)
  case eiRez of
    Left err -> putStrLn $ "@[parseToml] err: " <> show err
    Right toml ->
      debugInfo toml
  pure $ eiRez

debugInfo :: Tp.TOML -> IO ()
debugInfo toml =
  forM_ (Hms.keys toml.tomlPairs) (\(Tp.Key k) -> do
            let
              (Tp.Piece p) = Nemp.head k
            putStrLn $ "k: " <> DT.unpack p <> " = " <> (case Hms.lookup (Tp.Key k) toml.tomlPairs of Nothing -> ""; Just aVal -> show aVal)
            )
{-
  toml.tomlPairs :: HashM
  toml.tomlTables
  toml.tomlTableArrays

-}