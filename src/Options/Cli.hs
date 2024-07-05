{-# LANGUAGE DerivingStrategies #-}

module Options.Cli where

import Data.Text (Text)
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
  | NewCmd NewOptions
  | ServerCmd
  | VersionCmd
  -- Daniell specific:
  | PublishCmd
  deriving stock (Show)

data NewOptions = NewOptions {
    rootDir :: Text
    , template :: Maybe Text
  }
  deriving stock (Show)

data CliOptions = CliOptions {
  debug :: Maybe Int
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
        ("config", pure ConfigCmd, "Print the site configuration.")
      , ("convert", pure ConvertCmd, "Convert your content to different formats.")
      , ("deploy", pure DeployCmd, "Deploy your site to a Cloud provider.")
      , ("env", pure EnvCmd, "Print Hugo version and environment info.")
      , ("gen", pure GenCmd, "A collection of several useful generators.")
      , ("help", pure HelpCmd, "Help about any command.")
      , ("import", pure ImportCmd, "Import your site from others.")
      , ("list", pure ListCmd, "Listing out various types of content.")
      , ("mod", pure ModCmd, "Various Hugo Modules helpers.")
      , ("new", NewCmd <$> newOpts, "Create a new project or add new content in a project.")
      , ("server", pure ServerCmd, "A high performance webserver.")
      , ("version", pure VersionCmd, "Print the version number of Daniell.")
      , ("publish", pure PublishCmd, "Deploy the site to the <public> self-sufficient folder.")
      ]
    headArray = head cmdArray
    tailArray = tail cmdArray
  in
    foldl (\accum aCmd -> cmdBuilder aCmd <> accum) (cmdBuilder headArray) tailArray
  where
    cmdBuilder (label, cmdDef, desc) =
      command label (info cmdDef (progDesc desc))


newOpts :: Parser NewOptions
newOpts =
   NewOptions <$>
      strArgument (metavar "PROJECTROOT" <> help "Name of project's root directory.")
    <*> optional (strOption (
        long "template"
        <> short 't'
        <> help "Template to use for the new item."
      ))

  