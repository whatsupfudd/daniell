module Options.CliOptions where

import qualified Data.Int as DI
import Options.Applicative


data CliOptions = CliOptions {
  debug :: Maybe DI.Int32
  , configFile :: Maybe FilePath
 }


parseCliOptions :: IO (Either String CliOptions)
parseCliOptions =
  Right <$> execParser parser

parser :: ParserInfo CliOptions
parser =
  info (arguments <**> helper) $
    fullDesc <> progDesc "Static Site generator." <> header "daniell"


arguments :: Parser CliOptions
arguments =
  pure $ CliOptions {
    debug = Nothing
    , configFile = Nothing
  }
