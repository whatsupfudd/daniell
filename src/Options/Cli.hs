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
        ("build", BuildCmd <$> buildOpts, "Builds a project into a static website or a dynamic web application.")
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
  cmdBuilder (label, cmdDef, desc) = command label (info (cmdDef <**> helper) (progDesc desc))


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
buildOpts = do
  BuildOptions <$> subparser (sitePK <> webAppPK <> localAppPK)
        <*> optional (strArgument (metavar "PROJECTROOT" <> help "Name of project's root directory."))
  -- (subparser (hugoSP <> nextSP <> fuddleSP <> gatsbySP)
  where
  sitePK = command "site" (info (SiteBK <$> siteOpts <**> helper) (progDesc "Builds a site project."))
  webAppPK = command "webapp" (info (WebAppBK <$> webappOpts <**> helper) (progDesc "Builds a webapp project."))
  localAppPK = command "localapp" (info (pure LocalAppBK <**> helper) (progDesc "Builds a localapp project."))
  {- TODO: get the parser to only accept these keywords:
  nextSP = command "next" (info (pure NextSP) (progDesc "Builds a Next project."))
  fuddleSP = command "fuddle" (info (pure FuddleSP) (progDesc "Builds a Fuddle project."))
  gatsbySP = command "gatsby" (info (pure GatsbySP) (progDesc "Builds a Gatsby project."))
  -}
  {-
   Hugo options:
  -b, --baseURL string             hostname (and path) to the root, e.g. https://spf13.com/
  -D, --buildDrafts                include content marked as draft
  -E, --buildExpired               include expired content
  -F, --buildFuture                include content with publishdate in the future
      --cacheDir string            filesystem path to cache directory
      --cleanDestinationDir        remove files from destination not found in static directories
      --clock string               set the clock used by Hugo, e.g. --clock 2021-11-06T22:30:00.00+09:00
      --config string              config file (default is hugo.yaml|json|toml)
      --configDir string           config dir (default "config")
  -c, --contentDir string          filesystem path to content directory
      --debug                      debug output
  -d, --destination string         filesystem path to write files to
      --disableKinds strings       disable different kind of pages (home, RSS etc.)
      --enableGitInfo              add Git revision, date, author, and CODEOWNERS info to the pages
  -e, --environment string         build environment
      --forceSyncStatic            copy all files when static is changed.
      --gc                         enable to run some cleanup tasks (remove unused cache files) after the build
  -h, --help                       help for hugo
      --ignoreCache                ignores the cache directory
      --ignoreVendorPaths string   ignores any _vendor for module paths matching the given Glob pattern
  -l, --layoutDir string           filesystem path to layout directory
      --logLevel string            log level (debug|info|warn|error)
      --minify                     minify any supported output format (HTML, XML etc.)
      --noBuildLock                don't create .hugo_build.lock file
      --noChmod                    don't sync permission mode of files
      --noTimes                    don't sync modification time of files
      --panicOnWarning             panic on first WARNING log
      --poll string                set this to a poll interval, e.g --poll 700ms, to use a poll based approach to watch for file system changes
      --printI18nWarnings          print missing translations
      --printMemoryUsage           print memory usage to screen at intervals
      --printPathWarnings          print warnings on duplicate target paths etc.
      --printUnusedTemplates       print warnings on unused templates.
      --quiet                      build in quiet mode
      --renderToMemory             render to memory (only useful for benchmark testing)
  -s, --source string              filesystem path to read files relative from
      --templateMetrics            display metrics about template executions
      --templateMetricsHints       calculate some improvement hints when combined with --templateMetrics
  -t, --theme strings              themes to use (located in /themes/THEMENAME/)
      --themesDir string           filesystem path to themes directory
      --trace file                 write trace to file (not useful in general)
  -v, --verbose                    verbose output
  -w, --watch                      watch filesystem for changes and recreate as needed
  -}

siteOpts :: Parser SiteOptions
siteOpts = 
  subparser (
      command "hugo" (info (HugoSS <$> hugoOpts <**> helper) (progDesc "Build a Hugo project."))
    <> command "php" (info (PhpSS <$> phpOpts <**> helper) (progDesc "Builds a PHP project."))
    <> command "gatsby" (info (pure GatsbySS <**> helper) (progDesc "Builds a Gatsby project."))
  )


hugoOpts :: Parser HugoBuildOptions
hugoOpts =
  HugoBuildOptions <$> optional (strOption (long "baseURL" <> short 'b' <> help "(string) hostname (and path) to the root, e.g. https://spf13.com/"))
  <*> optional (switch (long "buildDrafts" <> short 'D' <> help "include content marked as draft"))
  <*> optional (switch (long "buildExpired" <> short 'E' <> help "include expired content"))
  <*> optional (switch (long "buildFuture" <> short 'F' <> help "include content with publishdate in the future"))
  <*> optional (strOption (long "cacheDir" <> help "(string) filesystem path to cache directory"))
  <*> optional (switch (long "cleanDestinationDir" <> help "remove files from destination not found in static directories"))
  <*> optional (strOption (long "clock" <> help "(string) set the clock used by Hugo, e.g. --clock 2021-11-06T22:30:00.00+09:00"))
  <*> optional (strOption (long "config" <> help "(string) config file (default is hugo.yaml|json|toml)"))
  <*> optional (strOption (long "configDir" <> help "(string) config dir (default \"config\")"))
  <*> optional (strOption (long "contentDir" <> short 'c' <> help "(string) filesystem path to content directory"))
  <*> optional (switch (long "debug" <> help "debug output"))
  <*> optional (strOption (long "destination" <> short 'd' <> help "(string) filesystem path to write files to"))
  <*> optional (strOption (long "disableKinds" <> help "(strings) disable different kind of pages (home, RSS etc.)"))
  <*> optional (switch (long "enableGitInfo" <> help "add Git revision, date, author, and CODEOWNERS info to the pages"))
  <*> optional (strOption (long "environment" <> short 'e' <> help "(string) build environment"))
  <*> optional (switch (long "forceSyncStatic" <> help "copy all files when static is changed."))
  <*> optional (switch (long "gc" <> help "enable to run some cleanup tasks (remove unused cache files) after the build"))
  <*> optional (switch (long "ignoreCache" <> help "ignores the cache directory"))
  <*> optional (strOption (long "ignoreVendorPaths" <> help "(string) ignores any _vendor for module paths matching the given Glob pattern"))
  <*> optional (strOption (long "layoutDir" <> short 'l' <> help "(string) filesystem path to layout directory"))
  <*> optional (strOption (long "logLevel" <> help "(string) log level (debug|info|warn|error)"))
  <*> optional (switch (long "minify" <> help "minify any supported output format (HTML, XML etc.)"))
  <*> optional (switch (long "noBuildLock" <> help "don't create .hugo_build.lock file"))
  <*> optional (switch (long "noChmod" <> help "don't sync permission mode of files"))
  <*> optional (switch (long "noTimes" <> help "don't sync modification time of files"))
  <*> optional (switch (long "panicOnWarning" <> help "panic on first WARNING log"))
  <*> optional (strOption (long "poll" <> help "(string) set this to a poll interval, e.g --poll 700ms, to use a poll based approach to watch for file system changes"))
  <*> optional (switch (long "printI18nWarnings" <> help "print missing translations"))
  <*> optional (switch (long "printMemoryUsage" <> help "print memory usage to screen at intervals"))
  <*> optional (switch (long "printPathWarnings" <> help "print warnings on duplicate target paths etc."))
  <*> optional (switch (long "printUnusedTemplates" <> help "print warnings on unused templates."))
  <*> optional (switch (long "quiet" <> help "build in quiet mode"))
  <*> optional (switch (long "renderToMemory" <> help "render to memory (only useful for benchmark testing)"))
  <*> optional (strOption (long "source" <> short 's' <> help "(string) filesystem path to read files relative from"))
  <*> optional (switch (long "templateMetrics" <> help "display metrics about template executions"))
  <*> optional (switch (long "templateMetricsHints" <> help "calculate some improvement hints when combined with --templateMetrics"))
  <*> optional (strOption (long "theme" <> short 't' <> help "(strings) themes to use (located in /themes/THEMENAME/)"))
  <*> optional (strOption (long "themesDir" <> help "(string) filesystem path to themes directory"))
  <*> optional (strOption (long "trace" <> help "(file) write trace to file (not useful in general)"))
  <*> optional (switch (long "verbose" <> short 'v' <> help "verbose output"))
  <*> optional (switch (long "watch" <> short 'w' <> help "watch filesystem for changes and recreate as needed"))


phpOpts :: Parser PhpBuildOptions
phpOpts =
  PhpBuildOptions <$> optional (strOption (long "srcDir" <> help "(string) filesystem path to read files relative from"))



webappOpts :: Parser WebAppOptions
webappOpts =
  subparser (
      command "next" (info (NextWA <$> nextAppOpts <**> helper) (progDesc "Builds a Next project."))
    <> command "fuddle" (info (pure FuddleWA <**> helper) (progDesc "Builds a Fuddle project."))
  )


nextAppOpts :: Parser NextAppBuildOptions
nextAppOpts =
  NextAppBuildOptions <$> optional (strOption (long "package" <> short 'p' <> help "(string) alternative package file for the project."))


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

