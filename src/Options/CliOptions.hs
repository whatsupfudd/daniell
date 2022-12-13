{-# LANGUAGE DerivingStrategies #-}

module Options.CliOptions where

import qualified Data.Int as DI
import Options.Applicative


data Command =
  ConfigCmd
  | ConvertCmd
  | DeployCmd
  | EnvCmd
  | GenCmd
  | HelpCmd
  | ImportCmd
  | ListCmd
  | ModCmd
  | NewCmd
  | ServerCmd
  | VersionCmd
  -- Daniell specific:
  | PublishCmd
  deriving stock (Show)

data CliOptions = CliOptions {
  debug :: Maybe DI.Int32
  , configFile :: Maybe FilePath
  , job :: Maybe Command
 }
 deriving stock (Show)


parseCliOptions :: IO (Either String CliOptions)
parseCliOptions =
  Right <$> execParser parser

parser :: ParserInfo CliOptions
parser =
  info (argumentsP <**> helper) $
    fullDesc <> progDesc "Simple and flexible Static Site generator." <> header "daniell"


argumentsP :: Parser CliOptions
argumentsP = do
  buildOptions <$> globConfFileDef <*> (subparser commandsDef)
  where
    buildOptions confPath cmd =
      let
        mbConfPath = case confPath of
          "" -> Nothing
          aValue -> Just aValue
      in
      CliOptions {
        debug = Nothing
        , configFile = mbConfPath
        , job = Just cmd
      }

globConfFileDef :: Parser (FilePath)
globConfFileDef =
  strOption (
    long "danconf"
    <> metavar "DANIELLCONF"
    <> value ""
    <> showDefault
    <> help "Global config file (default is ~/.daniell/config.yaml)."
  )

commandsDef :: Mod CommandFields Command
commandsDef =
  let
    cmdArray = [
        ("config", ConfigCmd, "Print the site configuration.")
      , ("convert", ConvertCmd, "Convert your content to different formats.")
      , ("deploy", DeployCmd, "Deploy your site to a Cloud provider.")
      , ("env", EnvCmd, "Print Hugo version and environment info.")
      , ("gen", GenCmd, "A collection of several useful generators.")
      , ("help", HelpCmd, "Help about any command.")
      , ("import", ImportCmd, "Import your site from others.")
      , ("list", ListCmd, "Listing out various types of content.")
      , ("mod", ModCmd, "Various Hugo Modules helpers.")
      , ("new", NewCmd, "Create new content for your site.")
      , ("server", ServerCmd, "A high performance webserver.")
      , ("version", VersionCmd, "Print the version number of Daniell.")
      , ("publish", PublishCmd, "Deploy the site to the <public> self-sufficient folder.")
      ]
    headArray = head cmdArray
    tailArray = tail cmdArray
  in
    foldl (\accum aCmd -> (cmdBuilder aCmd) <> accum) (cmdBuilder headArray) tailArray
  where
    cmdBuilder (label, cmdType, desc) =
      command label (info (pure cmdType) (progDesc desc))
