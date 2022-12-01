# DANIELL
## A fast static site builder compatible with HUGO.


### Goal: generate static website from markup files, with ability to read HUGO site definitions and templates.

### The general logic is to:
- read the driving configuration for driving static site generation from a list of markup files,
- parse the folders and collect all relevant files,
- apply transformation logic from descriptions to HTML and associated resources,
- provide either a HTTP access for browsing the site, or a stand-alone version of the content ready for publication on CDNs.

### Implementation:
- use Haskell for code base,
- inspire template processing from [Ginger](https://github.com/tdammers/ginger),
- inspire script interpretation from [Co](https://abhinavsarkar.net/posts/implementing-co-2/),
- use Servant for web services,

### Commands:
- *no arguments* : reads the current folder and creates a version of the static site in the *public* sub-folder.
- **new site** <path> : populates a folder *path* with the required basic scaffholding, creating the folder if it doesn't already exist.
- **new** <path> : creates a markup file at *path* with the minimal content to be useful in static site processing.
- **server** : creates an internal representation of the static site and and make it available to browsers on a HTTP channel.
- **mod init** <path> : populates a folder *path> with the required basic scaffholding for a **module**, creates the folder *path* if it doesn't already exist.
- **mod get** : updates the modules from their git repo.

### Site Description Folder Structure:
- archetypes:
- assets: resources used by the scriting logic (pipes) in on-the-fly no-markup content transformation.
- content: the top level container for all markup files that will lead to the generation of pages in the static site. Its subfolders correspond to sub-sections of the site.
- data: files providing structured datasets to the scripting logic for inclusion into the content transformation (eg rows of records).
- layouts: templates of HTML and scripting logic that drive the transformation logic of markup files into static web pages.
- public: publishable version of the static site.
- static: resources that support the rendering of pages in the static site (images, style sheets, javascript, etc). This folder is recreated as is in the public folder when a version of the static site is created.
- themes: reusable version of site definitions that provide a much faster and simpler creation process.
- config.<ext> : the configuration for driving the transformation logic, provided either in yaml, toml or json format.

### Hugo command set:
> Usage:
  hugo [flags]
  hugo [command]
>
> Available Commands:
  config      Print the site configuration
  convert     Convert your content to different formats
  deploy      Deploy your site to a Cloud provider.
  env         Print Hugo version and environment info
  gen         A collection of several useful generators.
  help        Help about any command
  import      Import your site from others.
  list        Listing out various types of content
  mod         Various Hugo Modules helpers.
  new         Create new content for your site
  server      A high performance webserver
  version     Print the version number of Hugo
>
> Flags:
  -b, --baseURL string             hostname (and path) to the root, e.g. http://spf13.com/
  -D, --buildDrafts                include content marked as draft
  -E, --buildExpired               include expired content
  -F, --buildFuture                include content with publishdate in the future
      --cacheDir string            filesystem path to cache directory. Defaults: $TMPDIR/hugo_cache/
      --cleanDestinationDir        remove files from destination not found in static directories
      --config string              config file (default is path/config.yaml|json|toml)
      --configDir string           config dir (default "config")
  -c, --contentDir string          filesystem path to content directory
      --debug                      debug output
  -d, --destination string         filesystem path to write files to
      --disableKinds strings       disable different kind of pages (home, RSS etc.)
      --enableGitInfo              add Git revision, date and author info to the pages
  -e, --environment string         build environment
      --forceSyncStatic            copy all files when static is changed.
      --gc                         enable to run some cleanup tasks (remove unused cache files) after the build
  -h, --help                       help for hugo
      --i18n-warnings              print missing translations
      --ignoreCache                ignores the cache directory
      --ignoreVendor               ignores any _vendor directory
      --ignoreVendorPaths string   ignores any _vendor for module paths matching the given Glob pattern
  -l, --layoutDir string           filesystem path to layout directory
      --log                        enable Logging
      --logFile string             log File path (if set, logging enabled automatically)
      --minify                     minify any supported output format (HTML, XML etc.)
      --noChmod                    don't sync permission mode of files
      --noTimes                    don't sync modification time of files
      --path-warnings              print warnings on duplicate target paths etc.
      --print-mem                  print memory usage to screen at intervals
      --quiet                      build in quiet mode
      --renderToMemory             render to memory (only useful for benchmark testing)
  -s, --source string              filesystem path to read files relative from
      --templateMetrics            display metrics about template executions
      --templateMetricsHints       calculate some improvement hints when combined with --templateMetrics
  -t, --theme strings              themes to use (located in /themes/THEMENAME/)
      --themesDir string           filesystem path to themes directory
      --trace file                 write trace to file (not useful in general)
  -v, --verbose                    verbose output
      --verboseLog                 verbose logging
  -w, --watch                      watch filesystem for changes and recreate as needed

