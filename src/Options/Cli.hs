{-# LANGUAGE DerivingStrategies #-}

module Options.Cli where

import Data.Text (Text, pack, splitOn)
import Options.Applicative

import Options.Types

data Command =
  BuildCmd BuildOptions
  | ConfigCmd
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
  deriving stock Show


data CliOptions = CliOptions {
  debug :: Maybe Int
  , configFile :: Maybe FilePath
  , job :: Maybe Command
 }
 deriving stock Show


parseCliOptions :: IO (Either String CliOptions)
parseCliOptions =
  Right <$> execParser parser

parser :: ParserInfo CliOptions
parser =
  info (argumentsP <**> helper) $
    fullDesc <> progDesc "Fast and flexible project creator and content generator." <> header "daniell"


argumentsP :: Parser CliOptions
argumentsP = do
  buildOptions <$> globConfFileDef <*> subparser commandsDef
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


globConfFileDef :: Parser FilePath
globConfFileDef =
  strOption (
    long "danconf"
    <> metavar "DANIELLCONF"
    <> value ""
    <> showDefault
    <> help "Global config file (default is ~/fudd/.daniell/config.yaml)."
  )

commandsDef :: Mod CommandFields Command
commandsDef =
  let
    cmdArray = [
        ("build", BuildCmd <$> buildOpts, "Print the site configuration.")
      , ("config", pure ConfigCmd, "Print the site configuration.")
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
  cmdBuilder (label, cmdDef, desc) = command label (info cmdDef (progDesc desc))


newOpts :: Parser NewOptions
newOpts =
   NewOptions <$>
      subparser (sitePK <> webAppPK <> localAppPK)
    <*> strArgument (metavar "PROJECTROOT" <> help "Name of project's root directory.")
    <*> many (strOption (
        long "template"
        <> short 't'
        <> help "Template to use for the new item."
      ))
    <*> optional (strOption (
        long "compat"
        <> short 'c'
        <> help "Project compatibility mode."
      ))
    <*> many (option paramParser (
        long "param"
        <> short 'p'
        <> help "Parameter for the template."
    ))
  where
  sitePK = command "site" (info (pure SitePK) (progDesc "Create a new site project."))
  webAppPK = command "webapp" (info (pure WebAppPK) (progDesc "Create a new webapp project."))
  localAppPK = command "localapp" (info (pure LocalAppPK) (progDesc "Create a new localapp project."))


buildOpts :: Parser BuildOptions
buildOpts =
  BuildOptions <$>
    subparser (sitePK <> webAppPK <> localAppPK)
    <*> optional (strOption (
        long "type"
        <> short 't'
        <> help "technology type (fuddle, hugo, nextjs)."
      ))
    <*> optional (strArgument (metavar "PROJECTROOT" <> help "Name of project's root directory."))
  -- (subparser (hugoSP <> nextSP <> fuddleSP <> gatsbySP)
  where
  sitePK = command "site" (info (pure SitePK) (progDesc "Create a new site project."))
  webAppPK = command "webapp" (info (pure WebAppPK) (progDesc "Create a new webapp project."))
  localAppPK = command "localapp" (info (pure LocalAppPK) (progDesc "Create a new localapp project."))
  {- TODO: get the parser to only accept these keywoards:
  hugoSP = command "hugo" (info (pure HugoSP) (progDesc "Build a Hugo project."))
  nextSP = command "next" (info (pure NextSP) (progDesc "Build a Next project."))
  fuddleSP = command "fuddle" (info (pure FuddleSP) (progDesc "Build a Fuddle project."))
  gatsbySP = command "gatsby" (info (pure GatsbySP) (progDesc "Build a Gatsby project."))
  -}
  {-
   Hugo options:
    --config <filename>[, <filename>]*   config file(s, with left-to-right priority), default is path/{hugo|config}.{yaml|json|toml}.
    --environment <env>                 build environment (development, staging, production, ...), default is 'development'.
    -b / --baseURL <url>                     hostname (and path) to the root, default is taken from baseURL in config file.
    -D / --buildDrafts                    include content marked as draft.
    -E / --buildExpired                   include expired content.
    -F / --buildFuture                    include content with publishdate in the future.
    --cacheDir <directory>            filesystem path to cache directory.
    --cleanDestinationDir            remove files from destination not found in source.
    --clock <string>                  set the clock used by generator, default is the machine's local time, e.g. --clock 2021-11-06T22:30:00.00+09:00
    --contentDir <directory>          filesystem path to content directory, default is path/content.
    --configDir <directory>           filesystem path to configuration directory, default is path/config.
    -c / --contentDir <directory>          filesystem path to content directory, default is path/content.
    --debug                          debug output.
    --destination <directory>         filesystem path to write files to, default is path/public.
  -}

paramParser :: ReadM ParameterTpl
paramParser = eitherReader $ \s ->
  let
    tS = pack s
  in
  case splitOn "=" tS of
    [k, v] -> Right $ AssignmentP (k, v)
    _ -> case s of
      "" -> Left "Invalid parameter format, expected <key>=<value>."
      _ -> Right $ FlagP tS

