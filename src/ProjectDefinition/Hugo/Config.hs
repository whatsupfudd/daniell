module ProjectDefinition.Hugo.Config where

import Data.Text (Text)
import qualified Data.Map as Mp

import Options.Runtime (RunOptions (..), TechOptions (..))
import Options.Types (HugoBuildOptions (..))

import ProjectDefinition.Types (DictEntry (..))


data LocaleConfig = LanguageConfig {
    symbol :: Text
    , disabled :: Bool
    , languageCode :: Text
    , languageDirection :: Text
    , languageName :: Text
    , title :: Text
    , weight :: Integer
  }
  deriving Show

-- TODO: assert language code using the iso639 package.
defaultLocale = LanguageConfig {
    symbol = "en"
    , disabled = False
    , languageCode = ""
    , languageDirection = ""
    , languageName = ""
    , title = ""
    , weight = 0
  }


-- ***** Utilities for extracting command line & runtime options *****
getHugoOpts :: TechOptions -> Maybe HugoBuildOptions
getHugoOpts (HugoOptions options) = Just options
getHugoOpts _ = Nothing

setRunOptions :: RunOptions -> HugoBuildOptions -> RunOptions
setRunOptions rtOpts buildOpts =
  rtOpts { techOpts = HugoOptions buildOpts }

data RunFlag =
  BuildF
  | ServerF

contextDefaultOptions :: RunFlag -> Mp.Map Text DictEntry
contextDefaultOptions runFlag =
  Mp.fromList [
    ("archetypeDir", StringDV "archetypes")
    , ("assetDir", StringDV "assets")
    , ("baseURL", StringDV "http://localhost:8080/")
    , ("build", DictDV $ Mp.fromList [
          ("buildStats", DictDV $ Mp.fromList [
              ("disableClasses", BoolDV False)
            , ("disableIDs", BoolDV False)
            , ("disableTags", BoolDV False)
            , ("enable", BoolDV False)
          ])
        , ("cacheBusters", ListDV $ [
            DictDV $ Mp.fromList [
                ("source", StringDV "(postcss|tailwind)\\.config\\.js")
              , ("target", StringDV "(css|styles|scss|sass)")
            ]
          ])
        , ("noJSConfigInAssets", BoolDV False)
        , ("useResourceCacheWhen", StringDV "fallback")
      ])
    , ("buildDrafts", BoolDV False)
    , ("buildExpired", BoolDV False)
    , ("buildFuture", BoolDV False)
    , ("caches", DictDV $ Mp.fromList [
          ("assets", DictDV $ Mp.fromList [
              ("dir", StringDV ":resourceDir/_gen")
              , ("maxAge", IntDV (-1))
            ])
        , ("getcsv", DictDV $ Mp.fromList [
            ("dir", StringDV ":cacheDir/:project")
            , ("maxAge", IntDV (-1))
          ])
        , ("getjson", DictDV $ Mp.fromList [
            ("dir", StringDV ":cacheDir/:project")
            , ("maxAge", IntDV (-1))
          ])
        , ("getresource", DictDV $ Mp.fromList [
            ("dir", StringDV ":cacheDir/:project")
            , ("maxAge", IntDV (-1))
          ])
        , ("images", DictDV $ Mp.fromList [
            ("dir", StringDV ":resourceDir/_gen")
            , ("maxAge", IntDV (-1))
          ])
        , ("misc", DictDV $ Mp.fromList [
            ("dir", StringDV ":cacheDir/:project")
            , ("maxAge", IntDV (-1))
          ])
        , ("modules", DictDV $ Mp.fromList [
            ("dir", StringDV ":cacheDir/modules")
            , ("maxAge", IntDV (-1))
          ])
      ])
    , ("canonifyURLs", BoolDV False)
    , ("capitalizeListTitles", BoolDV True)
    , ("cascade", ListDV $ [])
        {- example of a cascade entry:
          DictDV $ Mp.fromList [
            ("_target", DictDV $ Mp.fromList [
                ("kind", StringDV "page")
              , ("lang", StringDV "en")
              , ("path", "/articles/**")
            ])
          , ("params", DictDV $ Mp.fromList [
              ("background", StringDV "yosemite.jpg")
            ])
          ])
        -}
    , ("cleanDestinationDir", BoolDV False)
    , ("contentDir", StringDV "content")
    , ("copyright", StringDV "")
    , ("dataDir", StringDV "data")
    , ("defaultContentLanguage", StringDV "en")
    , ("defaultContentLanguageInSubdir", BoolDV False)
    , ("disableAliases", BoolDV False)
    , ("disableHugoGeneratorInject", BoolDV False)
    , ("disableKinds", ListDV [])     -- Entries are StringDV, any of: 404, home, page, robotstxt, rss, section, sitemap, taxonomy, or term.
    , ("disableLanguages", ListDV []) -- Entries are StringDV, any of iso639: en, fr, de, etc.
    , ("disableLiveReload", BoolDV False)
    , ("disablePathToLower", BoolDV False)
    , ("enableEmoji", BoolDV False)
    , ("enableGitInfo", BoolDV False)
    , ("enableMissingTranslationPlaceholders", BoolDV False)
    , ("enableRobotsTXT", BoolDV False)
    , ("environment", StringDV (case runFlag of BuildF -> "production"; ServerF -> "development"))
    , ("frontmatter", DictDV $ Mp.fromList [
          ("date", ListDV $ [StringDV "date", StringDV "publishdate", StringDV "pubdate", StringDV "published", StringDV "lastmod", StringDV "modified"])
        , ("expiryDate", ListDV $ [StringDV "expiryDate", StringDV "unpublishdate"])
        , ("lastmod", ListDV $ [StringDV ":git", StringDV "lastmod", StringDV "modified", StringDV "date", StringDV "publishdate", StringDV "pubdate", StringDV "published"])
        , ("publishDate", ListDV $ [StringDV "publishDate", StringDV "pubdate", StringDV "published", StringDV "date"])
      ])
    , ("hasCJKLanguage", BoolDV False)
    , ("ignoreCache", BoolDV False)
    , ("ignoreLogs", ListDV [])   -- Entries are StringDV, matching errorsidf (error-<id>) & warnidf (warn-<id>).
    , ("ignoreVendorPaths", StringDV "")  -- A Glob pattern that will filter out files from the _vendor directory.
    , ("imaging", DictDV $ Mp.fromList [
          ("bgColor", StringDV "#ffffff")
        , ("hint", StringDV "photo")
        , ("quality", IntDV 75)
        , ("resampleFilter", StringDV "box")
        , ("exif", DictDV $ Mp.fromList [
              ("disableDate", BoolDV False)
            , ("disableLatLong", BoolDV False)
            , ("excludeFields", StringDV "")     -- regex pattern.
            , ("includeFields", StringDV "")     -- regex pattern.
        ])
      ])
    , ("languageCode", StringDV "en")
    , ("languages", DictDV $ Mp.fromList [
          ("en", DictDV $ Mp.fromList [
              ("disabled", BoolDV False)
            , ("languageCode", StringDV "")
            , ("languageDirection", StringDV "")
            , ("languageName", StringDV "")
            , ("title", StringDV "")
            , ("weight", IntDV 0)
            -- can also have pagination.
          ])
      ])
    , ("layoutDir", StringDV "layouts")
    , ("markup", DictDV $ Mp.fromList [
        ("defaultMarkdownHandler", StringDV "goldmark")   -- Golang's libs: asciidocext, goldmark, org, pandoc, rst.
      ])
    , ("mediaTypes", DictDV Mp.empty)
        {- example of a mediaTypes entry:
          ("text/enriched", DictDV $ Mp.fromList [
              ("suffixes", ListDV $ [StringDV "enr"])
          ])
        , ("text/html", DictDV $ Mp.fromList [
            ("suffixes", ListDV $ [StringDV "asp"])
          ])
        -}
    , ("menus", DictDV Mp.empty)
        {- example of a menu definition:
          ("main", ListDV $ [
              DictDV $ Mp.fromList [
                  ("name", StringDV "home")
                , ("pageRef", StringDV "/")
                , ("weight", IntDV 10)
              )
            , DictDV $ Mp.fromList [
                  ("name", StringDV "Products")
                , ("pageRef", StringDV "/products")
                , ("weight", IntDV 20)
              )
            , DictDV $ Mp.fromList [
                  ("name", StringDV "Services")
                , ("pageRef", StringDV "/services")
                , ("weight", IntDV 30)
              )
        -}
    , ("minify", DictDV $ Mp.fromList [
          ("disableCSS", BoolDV False)
        , ("disableHTML", BoolDV False)
        , ("disableJS", BoolDV False)
        , ("disableJSON", BoolDV False)
        , ("disableSVG", BoolDV False)
        , ("disableXML", BoolDV False)
        , ("minifyOuput", BoolDV False)
        , ("tdewolff", DictDV $ Mp.fromList [
              ("css", DictDV $ Mp.fromList [
                    ("inline", BoolDV False)
                  , ("keepCSS2", BoolDV False)
                  , ("precision", IntDV 0)
              ])
            , ("html", DictDV $ Mp.fromList [
                  ("keepComments", BoolDV False)
                , ("keepConditionalComments", BoolDV False)
                , ("keepDefaultAttrVals", BoolDV True)
                , ("keepDocumentTags", BoolDV True)
                , ("keepEndTags", BoolDV True)
                , ("keepQuotes", BoolDV False)
                , ("keepSpecialComments", BoolDV True)
                , ("keepWhitespace", BoolDV False)
                , ("templateDelims", ListDV $ [StringDV "", StringDV ""])
              ])
            , ("js", DictDV $ Mp.fromList [
                  ("keepVarNames", BoolDV False)
                , ("precision", IntDV 0)
                , ("version", StringDV "2022")
              ])
            , ("json", DictDV $ Mp.fromList [
                  ("keepNumbers", BoolDV False)
                , ("precision", IntDV 0)
              ])
            , ("svg", DictDV $ Mp.fromList [
                  ("inline", BoolDV False)
                , ("keepComments", BoolDV False)
                ,  ("precision", IntDV 0)
              ])
            , ("xml", DictDV $ Mp.fromList [
                ("keepWhitespace", BoolDV False)
              ])
          ])
      ])
    , ("module", DictDV $ Mp.fromList [
          ("mounts", ListDV [
            DictDV $ Mp.fromList [
                ("source", StringDV "content")
              , ("target", StringDV "content")
            ]
          , DictDV $ Mp.fromList [
                ("source", StringDV "static")
              , ("target", StringDV "static")
            ]
          , DictDV $ Mp.fromList [
                ("source", StringDV "layouts")
              , ("target", StringDV "layouts")
            ]
          , DictDV $ Mp.fromList [
                ("source", StringDV "data")
              , ("target", StringDV "data")
            ]
          , DictDV $ Mp.fromList [
                ("source", StringDV "assets")
              , ("target", StringDV "assets")
            ]
          , DictDV $ Mp.fromList [
                ("source", StringDV "i18n")
              , ("target", StringDV "i18n")
            ]
          , DictDV $ Mp.fromList [
                ("source", StringDV "archetypes")
              , ("target", StringDV "archetypes")
            ]
        ])
        {- example of other sections configuration:
            noProxy: none
            noVendor: ""
            private: '*.*'
            proxy: direct
            replacements: ""
            vendorClosest: false
            workspace: "off"
            hugoVersion:
              extended: false
              max: ""
              min: ""
            imports:
            - disable: false
              ignoreConfig: false
              ignoreImports: false
              path: github.com/gohugoio/hugoTestModules1_linux/modh1_2_1v
            - path: my-shortcodes
        -}
      ])
    , ("newContentEditor", BoolDV False)
    , ("noBuildLock", BoolDV False)
    , ("noChmod", BoolDV False)
    , ("noTimes", BoolDV False)
    , ("outputFormats", DictDV Mp.empty)
        {- example of outputFormats configuration:
            MyEnrichedFormat:
              baseName: myindex
              isPlainText: true
              mediaType: text/enriched
              protocol: bep://
        -}
    , ("page", DictDV $ Mp.fromList [
          ("nextPrevInSectionSortOrder", StringDV "desc")
        , ("nextPrevSortOrder", StringDV "desc")
      ])
    , ("pagination", DictDV $ Mp.fromList [
          ("disableAliases", BoolDV False)
        , ("pagerSize", IntDV 10)
        , ("path", StringDV "page")
      ])
    , ("panicOnWarning", BoolDV False)
    , ("permalinks", DictDV Mp.empty)
      {- example of permalinks configuration:
            page:
              posts: /articles/:year/:month/:slug/
              tutorials: /training/:slug/
            section:
              posts: /articles/
              tutorials: /training/
      -}
    , ("pluralizeListTitles", BoolDV True)
    , ("printI18nWarnings", BoolDV False)
    , ("printPathWarnings", BoolDV False)
    , ("printUnusedTemplates", BoolDV False)
    , ("publishDir", StringDV "public")
    , ("refLinksErrorLevel", StringDV "ERROR")
    , ("refLinksNotFoundURL", StringDV "")
    , ("related", DictDV $ Mp.fromList [
          ("indices", ListDV [
              DictDV $ Mp.fromList [
                  ("applyFilter", BoolDV False)
                , ("cardinalityThreshold", IntDV 0)
                , ("name", StringDV "keywords")
                , ("pattern", StringDV "")
                , ("toLower", BoolDV False)
                , ("type", StringDV "basic")
                , ("weight", IntDV 100)
              ]
            , DictDV $ Mp.fromList [
                  ("applyFilter", BoolDV False)
                , ("cardinalityThreshold", IntDV 0)
                , ("name", StringDV "date")
                , ("pattern", StringDV "")
                , ("toLower", BoolDV False)
                , ("type", StringDV "basic")
                , ("weight", IntDV 100)
              ]
            , DictDV $ Mp.fromList [
                  ("applyFilter", BoolDV False)
                , ("cardinalityThreshold", IntDV 0)
                , ("name", StringDV "tags")
                , ("pattern", StringDV "")
                , ("toLower", BoolDV False)
                , ("type", StringDV "basic")
                , ("weight", IntDV 100)
              ]            
          ])
        , ("threshold", IntDV 80)
        , ("toLower", BoolDV False)
      ])
    , ("relativeURLs", BoolDV False)
    , ("removePathAccents", BoolDV False)
    , ("renderSegments", ListDV [])    -- Segments in the "segments" directory to render.
    , ("sectionPagesMenu", StringDV "")
    , ("security", DictDV $ Mp.fromList [
          ("enableInlineShortCodes", BoolDV False)
        , ("exec", DictDV $ Mp.fromList [
              ("allow", ListDV [
                  StringDV "^(dart-)?sass(-embedded)?$"
                , StringDV "^go$"
                , StringDV "^git$"
                , StringDV "^npx$"
                , StringDV "^postcss$"
                , StringDV "^tailwindcss$"
              ])
            , ("osEnv", ListDV [
                StringDV "(?i)^((HTTPS?|NO)_PROXY|PATH(EXT)?|APPDATA|TE?MP|TERM|GO\\w+|(XDG_CONFIG_)?HOME|USERPROFILE|SSH_AUTH_SOCK|DISPLAY|LANG|SYSTEMDRIVE)$"
              ])
          ])
        , ("funcs", DictDV $ Mp.fromList [
              ("getenv", ListDV [
                  StringDV "^HUGO_"
                , StringDV "^CI$"
              ])
          ])
        , ("http", DictDV $ Mp.fromList [
              ("mediaTypes", StringDV "null")
              , ("methods", ListDV [ StringDV "(?i)GET|POST"])
              , ("urls", ListDV [ StringDV ".*" ])
          ])
      ])
    , ("segments", DictDV Mp.empty)
      {- example of a segment configuration:
          segment1:
            excludes:
            - lang: n*
            - lang: en
              output: rss
            includes:
            - kind: '{home,term,taxonomy}'
            - path: '{/docs,/docs/**}'
      -}
    , ("sitemap", DictDV $ Mp.fromList [
          ("changefreq", StringDV "")
          , ("disable", BoolDV False)
          , ("filename", StringDV "sitemap.xml")
          , ("priority", IntDV (-1))
      ])
    , ("summaryLength", IntDV 70)
    , ("taxonomies", DictDV $ Mp.fromList [
          ("category", StringDV "categories")
        , ("tag", StringDV "tags")
      ])
    , ("templateMetrics", BoolDV False)
    , ("templateMetricsHints", BoolDV False)
    , ("theme", StringDV "")
    , ("themesDir", StringDV "themes")
    , ("timeout", StringDV "30s")
    , ("timeZone", StringDV "")  -- At least UTC, Local, and locations of IANA time zone database.
    , ("title", StringDV "")
    , ("titleCaseStyle", StringDV "ap")
    , ("uglyURLs", BoolDV False)
    , ("watch", BoolDV False)
  ]

ctxtOptionKeys :: [ Text ]
ctxtOptionKeys = [
    "archetypeDir", "assetDir", "baseURL", "build", "buildDrafts", "buildExpired", "buildFuture", "caches"
  , "canonifyURLs", "capitalizeListTitles", "cascade", "cleanDestinationDir", "contentDir", "copyright", "dataDir"
  , "defaultContentLanguage", "defaultContentLanguageInSubdir", "disableAliases", "disableHugoGeneratorInject", "disableKinds"
  , "disableLanguages", "disableLiveReload", "disablePathToLower", "enableEmoji", "enableGitInfo"
  , "enableMissingTranslationPlaceholders", "enableRobotsTXT", "environment", "frontmatter", "hasCJKLanguage", "ignoreCache"
  , "ignoreLogs", "ignoreVendorPaths", "imaging", "languageCode", "languages", "layoutDir", "markup", "mediaTypes", "menus"
  , "minify", "module", "newContentEditor", "noBuildLock", "noChmod", "noTimes", "outputFormats", "page", "pagination"
  , "panicOnWarning", "permalinks", "pluralizeListTitles", "printI18nWarnings", "printPathWarnings", "printUnusedTemplates"
  , "publishDir", "refLinksErrorLevel", "refLinksNotFoundURL", "related", "relativeURLs", "removePathAccents", "renderSegments"
  , "sectionPagesMenu", "security", "segments", "sitemap", "summaryLength", "taxonomies", "templateMetrics", "templateMetricsHints"
  , "theme", "themesDir", "timeout", "timeZone", "title", "titleCaseStyle", "uglyURLs", "watch"
  ]
