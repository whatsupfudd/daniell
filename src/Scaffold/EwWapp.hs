module Scaffold.EwWapp where

import Control.Monad (forM_)

import qualified Data.ByteString as Bs
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Data.UUID.V4 (nextRandom)
import qualified Data.UUID as Uu
import System.Directory (createDirectoryIfMissing)
import System.FilePath ((</>))

import Options.Types (NewOptions (..))
import Conclusion (Conclusion (..))
import qualified Options.Runtime as Rto


data WappFileApps =
  DesktopNav Uu.UUID
  | TranslateEn T.Text
  | NoArg


defaultEwWapp :: Rto.RunOptions -> NewOptions -> IO Conclusion
defaultEwWapp rtOpts newOpts = do

  newAppID <- nextRandom
  putStrLn $ "@[defaultEwWapp] new app ID: " <> T.unpack (Uu.toText newAppID)
  {-
  New project directory structure:
   * project directory:
     * build
       * app
       * config
       * css
       * Data
       * en
       * imgs
       * js
         * app
     * resources
       * js
       * css
     * wapp
       * Components
       * Intl
       * Protected
       * DynRoutes.elm
       * RootLayout.elm
       * BuildPage.elm
     * .gitignore
     * elm.json
  -}

  -- Create the directory structure for a new EwWapp project
  let
    directories = [
        "build" </> "app",
        "build" </> "config",
        "build" </> "css",
        "build" </> "Data",
        "build" </> "en",
        "build" </> "imgs",
        "build" </> "js" </> "app",
        "resources" </> "js",
        "resources" </> "css",
        "wapp" </> "Components",
        "wapp" </> "Intl",
        "wapp" </> "Protected"
      ]

  -- Create all directories
  forM_ directories $ \dir -> do
    putStrLn $ "@[defaultEwWapp] Creating directory: " <> dir
    createDirectoryIfMissing True (newOpts.rootDir </> dir)

  -- Create initial files
  -- Base wapp app:
  defaultWappFile "DynRoutes.elm" (newOpts.rootDir </> "wapp" </> "DynRoutes.elm")
  defaultWappFile "RootLayout.elm" (newOpts.rootDir </> "wapp" </> "RootLayout.elm")
  defaultWappFile "BuildPage.elm" (newOpts.rootDir </> "wapp" </> "BuildPage.elm")
  defaultWappFile "Translation_en.elm" (newOpts.rootDir </> "wapp" </> "Intl" </> "Translation_en.elm")
  defaultWappFile "TranslationHack.elm" (newOpts.rootDir </> "wapp" </> "Intl" </> "TranslationHack.elm")
  defaultWappFileWithArgs "DesktopNav.elm" (newOpts.rootDir </> "wapp" </> "Protected" </> "DesktopNav.elm") (DesktopNav newAppID)
  defaultWappFile "DemoHome.elm" (newOpts.rootDir </> "wapp" </> "Components" </> "DemoHome.elm")
  defaultWappFile "NavBar.elm" (newOpts.rootDir </> "wapp" </> "Components" </> "NavBar.elm")
  defaultWappFile "Icons.elm" (newOpts.rootDir </> "wapp" </> "Components" </> "Icons.elm")
  -- Resources:
  defaultWappFile "all.css" (newOpts.rootDir </> "resources" </> "css" </> "all.css")
  -- Top level files:
  defaultWappFile "elm.json" (newOpts.rootDir </> "elm.json")
  defaultWappFile ".gitignore" (newOpts.rootDir </> ".gitignore")
  defaultWappFile "tailwind.config.js" (newOpts.rootDir </> "tailwind.config.js")
  defaultWappFile "mkBuildPages" (newOpts.rootDir </> "mkBuildPages")

  putStrLn "@[defaultEwWapp] Project structure created successfully."
  return NilCcl


defaultWappFile :: String -> FilePath -> IO (Either String ())
defaultWappFile fileName path =
  defaultWappFileWithArgs fileName path NoArg


defaultWappFileWithArgs :: String -> FilePath -> WappFileApps -> IO (Either String ())
defaultWappFileWithArgs fileName path args =
  let
    fileContent = case fileName of
        "DynRoutes.elm" -> Right dynRoutesVerbatim
        "RootLayout.elm" -> Right rootLayoutVerbatim
        "BuildPage.elm" -> Right buildPageVerbatim
        "Translation_en.elm" -> Right translationEnVerbatim
        "TranslationHack.elm" -> Right translationHackVerbatim
        "DesktopNav.elm" -> Right $ desktopNavVerbatim args
        "NavBar.elm" -> Right navBarVerbatim
        "DemoHome.elm" -> Right demoHomeVerbatim
        "Icons.elm" -> Right iconsVerbatim
        "elm.json" -> Right elmJsonVerbatim
        ".gitignore" -> Right "\
          \# Elm \
          \elm-stuff/ \
          \# Build output \
          \build/ \
          \# Node modules \
          \node_modules/ \
          \# Editor files \
          \.vscode/ \
          \.idea/ \
          \*.swp \
          \*.swo \
          \# OS files \
          \.DS_Store"
        "all.css" -> Right "@import \"tailwindcss\";"
        "tailwind.config.js" -> Right "\
          \module.exports = {\n\
          \  content: [\n\
          \    \"./wapp/**/*.elm\",\n\
          \    \"./resources/html/**/*.html\",\n\
          \  ],\n\
          \  theme: {\n\
          \    extend: {},\n\
          \  },\n\
          \}\n"
        "mkBuildPages" -> Right "\
          \#!/bin/bash\n\
          \n\
          \echo \"building pages:\"\n\
          \locales=(en)\n\
          \for l in ${locales[@]}; do\n\
          \  echo \"- locale: $l\"\n\
          \  sed -i -e \"s/Translation_.*/Translation_$l as Loc/g\" wapp/Intl/TranslationHack.elm\n\
          \  echo \"homeAnon\"\n\
          \  mkTmpl -l \"$l\" homeAnon index\n\
          \done\n\
          \n\
          \echo \"*** Done ***\"\n"
        _ -> Left $ "@[defaultWappFile] unknown default content for file:" ++ fileName
  in
  case fileContent of
    Left err -> pure $ Left err
    Right content -> do
      putStrLn $ "@[defaultWappFile] Writing file: " <> path
      Bs.writeFile path content
      return (Right ())


dynRoutesVerbatim :: Bs.ByteString
dynRoutesVerbatim = "module DynRoutes exposing (main)\n\
\\n\
\import Dict as D\n\
\\n\
\import FunctionRouter exposing (run)\n\
\\n\
\-- Components:\n\
\\n\
\\n\
\-- This must be created on a per-instance of app/UI:\n\
\-- uiFunctions : D.Dict String (ComponentBuilder msg pT)\n\
\dynSpecifications = []\n\
\\n\
\\n\
\main =\n\
\  let\n\
\    dynFunctions = D.fromList <| List.map (\n\
\        \\(label, topLevel, _) -> (label, topLevel)\n\
\      ) dynSpecifications\n\
\    compContinuations = D.fromList <| List.foldl (\n\
\        \\(label, _, conts) accum -> accum ++ (\n\
\            List.map (\\(contLabel, handler) -> (label ++ \".\" ++ contLabel, handler)) conts)\n\
\      ) [] dynSpecifications\n\
\  in\n\
\    run (dynFunctions, compContinuations)"


rootLayoutVerbatim :: Bs.ByteString
rootLayoutVerbatim = "module RootLayout exposing (default)\n\
\\n\
\-- While the Fuddle compiler with template support is not ready.\n\
\import Html.String exposing (Html)\n\
\import Html.String.Attributes as A\n\
\import Html.Extra as Hx\n\
\\n\
\import Html.StaticRender as Sr\n\
\\n\
\import Fuddle.Locales as L\n\
\\n\
\{- Automatically typed as: [ Html.Element msg ] -> Html.Element msg\n\
\ The compiler will deduce [ Html.Element msg ] -> [ Html.Element msg ] automatically if\n\
\ there's an array that is sent back. [...] means List (...).\n\
\-}\n\
\\n\
\-- While using the Elm compiler, we type for convenience:\n\
\default : Sr.RenderOptions -> List (Html msg) -> Html msg\n\
\default rtOpts children =\n\
\  Hx.html [A.lang rtOpts.locale, A.dir (L.directionFor rtOpts.locale) ] [\n\
\    headR\n\
\    , bodyR children\n\
\  ]\n\
\\n\
\\n\
\headR =\n\
\  let\n\
\    appCss = \"/css/base.css\"\n\
\  in\n\
\    Hx.head [] [\n\
\      Hx.title [] <| L.l \"MarketingLayout.title\"\n\
\      , Hx.style [ A.attribute \"data-emotion\" \"cl-internal\", A.attribute \"nonce\" \"\", A.attribute \"data-s\" \"\"] []\n\
\      , Hx.meta [ Hx.charset \"utf-8\" ]\n\
\      , Hx.meta [ A.name \"viewport\", Hx.content \"width=device-width, initial-scale=1\" ]\n\
\      , Hx.link [ A.href \"https://cdn.jsdelivr.net/npm/flowbite@2.5.2/dist/flowbite.min.css\", A.rel \"stylesheet\", Hx.async]\n\
\      , Hx.link [ A.href appCss, A.rel \"stylesheet\", Hx.async]\n\
\      , Hx.script [ A.src \"https://unpkg.com/htmx.org@1.9.12/dist/htmx.min.js\", Hx.crossOrigin \"anonymous\", Hx.defer] \"\"\n\
\      , Hx.script [ A.src \"https://unpkg.com/htmx.org@1.9.12/dist/ext/ws.js\", Hx.crossOrigin \"anonymous\", Hx.defer] \"\"\n\
\      , Hx.script [\n\
\            A.src \"https://clerk.z14learning.com/npm/@clerk/clerk-js@5/dist/clerk.browser.js\"\n\
\            , A.attribute \"data-clerk-js-script\" \"true\"\n\
\            , A.attribute \"async\" \"\"\n\
\            , A.attribute \"crossorigin\" \"anonymous\"\n\
\            , A.attribute \"data-clerk-publishable-key\" \"pk_live_Y2xlcmsuejE0bGVhcm5pbmcuY29tJA\"\n\
\           ] \"\"\n\
\      , Hx.script [ A.type_ \"importmap\"] \"\"\"\n\
\          {\n\
\            \"imports\" : {\n\
\                \"three\" : \"https://cdn.jsdelivr.net/npm/three@0.172.0/build/three.module.js\"\n\
\              , \"three/addons/\" : \"https://cdn.jsdelivr.net/npm/three@0.172.0/examples/jsm/\"\n\
\              , \"three/webgpu\": \"https://threejs.org/build/three.webgpu.js\"\n\
\              , \"three/tsl\": \"https://threejs.org/build/three.tsl.js\"\n\
\              , \"htmx\" : \"https://unpkg.com/htmx.org@1.9.12/dist/htmx.min.js\"\n\
\              , \"flowbite\" : \"https://cdn.jsdelivr.net/npm/flowbite@2.5.2/dist/flowbite.min.js\"\n\
\              , \"d3\" : \"https://esm.run/d3\"\n\
\              , \"d3-hierarchy\" : \"https://esm.run/d3-hierarchy\"\n\
\              , \"d3-shape\" : \"https://esm.run/d3-shape\"\n\
\              , \"d3-array\" : \"https://esm.run/d3-array\"\n\
\              , \"@fullcalendar/core\" : \"https://cdn.skypack.dev/@fullcalendar/core@6.1.15\"\n\
\              , \"@fullcalendar/interaction\" : \"https://cdn.skypack.dev/@fullcalendar/interaction@6.1.15\"\n\
\              , \"@fullcalendar/daygrid\" : \"https://cdn.skypack.dev/@fullcalendar/daygrid@6.1.15\"\n\
\              , \"@fullcalendar/timegrid\" : \"https://cdn.skypack.dev/@fullcalendar/timegrid@6.1.15\"\n\
\              , \"@fullcalendar/list\" : \"https://cdn.skypack.dev/@fullcalendar/list@6.1.15\"\n\
\              , \"webrtc-adapter\" : \"https://cdn.jsdelivr.net/npm/webrtc-adapter@9.0.1/+esm\"\n\
\              , \"sortablejs\" : \"https://unpkg.com/sortablejs@1.15.6/modular/sortable.esm.js\"\n\
\            }\n\
\          }\n\
\        \"\"\"\n\
\    ]\n\
\\n\
\\n\
\bodyR children =\n\
\  Hx.body [] <|\n\
\    [\n\
\        Hx.script [ A.src \"/js/easyWordy.js\", Hx.crossOrigin \"anonymous\", Hx.async] \"\"\n\
\      , Hx.script [ A.type_ \"module\"] <| \"\"\"\n\
\          import \"htmx\"\n\
\          htmx.logAll()\n\
\        \"\"\"\n\
\    ]\n\
\    ++ children\n\
\    ++ [\n\
\      -- Note: @expand automatically inserts the elements in children between the 2 script elements.\n\
\      -- Hx.script [] \"initWapp()\"\n\
\      Hx.script [ A.src \"https://cdn.jsdelivr.net/npm/flowbite@2.5.2/dist/flowbite.min.js\", Hx.async] \"\"\n\
\    ]"


buildPageVerbatim :: Bs.ByteString
buildPageVerbatim = "module BuildPage exposing (main)\n\
\\n\
\import Dict as D\n\
\import Json.Decode as Dec\n\
\import Json.Encode as Enc\n\
\\n\
\import Html.StaticRender exposing (run, RenderOptions, FunctionDef (..))\n\
\import FunctionRouter exposing (DynInvokeFct)\n\
\import Html.String exposing (Html)\n\
\import RootLayout as Root\n\
\\n\
\-- Protected:\n\
\import Protected.DesktopNav as DesktopNav\n\
\import Components.DemoHome as DemoHome\n\
\\n\
\-- Static Component:\n\
\-- ...none...\n\
\-- left side menu:\n\
\-- ...none...\n\
\\n\
\pageDefs = D.fromList [\n\
\    -- Full pages:\n\
\    -- Protected:\n\
\    (\"homeAnon\", FullPage appHome)\n\
\    -- Static Components:\n\
\  ]\n\
\\n\
\\n\
\main =\n\
\  run pageDefs\n\
\\n\
\\n\
\-- Full pages:\n\
\-- ...none...\n\
\\n\
\-- Protected:\n\
\appHome : RenderOptions -> Html msgT\n\
\appHome rtOpts =\n\
\  Root.default rtOpts <| DesktopNav.default rtOpts <| DemoHome.default rtOpts []\n\
\\n\
\\n\
\-- Static Components:\n\
\allSites : DynInvokeFct msgT\n\
\allSites rtOpts =\n\
\  DemoHome.staticPage rtOpts\n\
\\n\
\-- ...none...\n"


translationEnVerbatim :: Bs.ByteString
translationEnVerbatim = "module Intl.Translation_en exposing (globDict)\n\
\\n\
\import Dict as D exposing (fromList)\n\
\\n\
\import LocSupport exposing (Translation, n, t, s)\n\
\\n\
\\n\
\\n\
\globDict = fromList [\n\
\      (\"NavBar\", n [\n\
\        (\"discuss\", t \"Discuss\")\n\
\        , (\"app\", t \"App\")\n\
\        , (\"signin\", t \"Sign In\")\n\
\        , (\"signup\", t \"Sign Up\")\n\
\        , (\"signout\", t \"Sign Out\")\n\
\      ])\n\
\    , (\"Footer\", n [\n\
\        (\"about\", t \"About\")\n\
\        , (\"privacy\", t \"Privacy Policy\")\n\
\        , (\"licensing\", t \"Licensing\")\n\
\        , (\"contact\", t \"Contact\")\n\
\        , (\"emailAddr\", t \"Email address\")\n\
\        , (\"yourEmail\", s \"Your email\")\n\
\        , (\"subscribe\", t \"Subscribe\")\n\
\        , (\"rightsReserved\", t \"All rights reserved\")\n\
\      ])\n\
\    , (\"MarketingLayout\", n [\n\
\        (\"title\", s \"WappApp v1\")\n\
\        , (\"description\", s \"Continously being better\")\n\
\      ])\n\
\  ]\n"


translationHackVerbatim :: Bs.ByteString
translationHackVerbatim = "module Intl.TranslationHack exposing (globDict)\n\
\\n\
\import Intl.Translation_en as Loc\n\
\\n\
\globDict = Loc.globDict\n"


desktopNavVerbatim :: WappFileApps -> Bs.ByteString
desktopNavVerbatim (DesktopNav appID) = "module Protected.DesktopNav exposing (default)\n\
\\n\
\import List as L\n\
\\n\
\import Html.String exposing (..)\n\
\import Html.String as H\n\
\import Html.String.Attributes exposing (..)\n\
\import Html.Extra as Hx\n\
\import Html.Htmx as Mx\n\
\import Html.StaticRender as Sr\n\
\import Html.Flowbite as Fb\n\
\import Sup.Svg as S\n\
\import Sup.Attributes as Sa\n\
\\n\
\import Components.NavBar exposing (navBarProtected)\n\
\\n\
\import Fuddle.Routes as R\n\
\\n\
\\n\
\default : Sr.RenderOptions -> Bool -> List (H.Html msg) -> List (H.Html msg)\n\
\default rtOpts isDynamic children =\n\
\  let\n\
\    hxConnector = if isDynamic then\n\
\        [\n\
\          Mx.hxExt \"ws\"\n\
\        , Mx.wsConnect \"ws://localhost:8885/wbap/stream/" <> T.encodeUtf8 (Uu.toText appID) <> "\"\n\
\        ]\n\
\      else []\n\
\  in [\n\
\  div\n\
\    ([ id \"wsConnector\"\n\
\    , class \"dark antialiased bg-gray-50 dark:bg-gray-900\"\n\
\    ]\n\
\    ++ hxConnector)\n\
\    [\n\
\      navBarProtected\n\
\      , leftAside\n\
\      , main_\n\
\          [ class \"relative h-full w-full overflow-x-scroll bg-gray-50 dark:bg-gray-900 lg:ms-64\"]\n\
\          [ div [ id \"mainContainer\" ] children ]\n\
\    ]\n\
\  , Hx.script [ type_ \"module\" ] \"\"\"\n\
\      import \"flowbite\"\n\
\      htmx.onLoad(() => {\n\
\        initFlowbite()\n\
\      })\n\
\    \"\"\"\n\
\ ]\n\
\\n\
\type alias SimpleItemLA msg = {\n\
\  icon : Maybe (Html msg)\n\
\  , title : String\n\
\  , href : String\n\
\  , mid : String\n\
\  , postIcon : Maybe (Html msg)\n\
\  , params : Maybe String\n\
\ }\n\
\\n\
\\n\
\type alias ComposedItemLA msg= {\n\
\  uid : String\n\
\  , icon : Maybe (Html msg)\n\
\  , title : String\n\
\  , children : List (ItemLA msg)\n\
\ }\n\
\\n\
\type ItemLA msg = Simple (SimpleItemLA msg) | Composed (ComposedItemLA msg)\n\
\\n\
\\n\
\leftPartAItems : List (ItemLA msg)\n\
\leftPartAItems = [\n\
\  Simple {\n\
\      title = \"Overview\"\n\
\      , href = \"overview\"\n\
\      , mid = \"home\"\n\
\      , icon = Just (S.svg [ Sa.class \"w-6 h-6\", Sa.fill \"currentColor\", Sa.viewBox \"0 0 20 20\", Sa.xmlns \"http://www.w3.org/2000/svg\", Hx.ariaHidden \"true\" ] [ S.path [ Sa.d \"M2 10a8 8 0 018-8v8h8a8 8 0 11-16 0z\" ] [] ])\n\
\      , postIcon = Nothing\n\
\      , params = Nothing\n\
\    }\n\
\  , Composed {\n\
\      uid = \"sections\"\n\
\      , icon = Just <|\n\
\            S.svg [\n\
\                Sa.class \"w-6 h-6\", Sa.fill \"currentColor\", Sa.viewBox \"0 0 20 20\", Sa.xmlns \"http://www.w3.org/2000/svg\", Hx.ariaHidden \"true\"\n\
\              ]\n\
\              [\n\
\                S.path [ Sa.d \"M4 4a2 2 0 012-2h4.586A2 2 0 0112 2.586L15.414 6A2 2 0 0116 7.414V16a2 2 0 01-2 2H6a2 2 0 01-2-2V4zm2 6a1 1 0 011-1h6a1 1 0 110 2H7a1 1 0 01-1-1zm1 3a1 1 0 100 2h6a1 1 0 100-2H7z\" ] []\n\
\              ]\n\
\      , title = \"Sections\"\n\
\      , children = [\n\
\        Simple {\n\
\          title = \"Sub-item\"\n\
\          , href = \"\"\n\
\          , mid = \"test.subItem\"\n\
\          , icon = Nothing\n\
\          , postIcon = Nothing\n\
\          , params = Just \"{&quot;itemID&quot;: 3}\"\n\
\        }\n\
\      ]\n\
\    }\n\
\ ]\n\
\\n\
\\n\
\leftPartBItems : List (ItemLA msg)\n\
\leftPartBItems = [\n\
\  Simple {\n\
\        title = \"Help\"\n\
\        , href = \"help\"\n\
\        , mid = \"test.help\"\n\
\        , icon = Just (S.svg [ Sa.class \"w-6 h-6\", Sa.fill \"currentColor\", Sa.viewBox \"0 0 20 20\", Sa.xmlns \"http://www.w3.org/2000/svg\", Hx.ariaHidden \"true\" ]\n\
\                [ S.path [ Sa.fillRule \"evenodd\", Sa.clipRule \"evenodd\", Sa.d \"M18 10a8 8 0 11-16 0 8 8 0 0116 0zm-8-3a1 1 0 00-.867.5 1 1 0 11-1.731-1A3 3 0 0113 8a3.001 3.001 0 01-2 2.83V11a1 1 0 11-2 0v-1a1 1 0 011-1 1 1 0 100-2zm0 8a1 1 0 100 2h.01a1 1 0 100-2H10z\" ] [] ])\n\
\        , postIcon = Nothing\n\
\        , params = Just \"{&quot;name&quot;:&quot;John Smithy&quot;}\"\n\
\      }\n\
\  ]\n\
\\n\
\-- id \"sidebar\"\n\
\leftAside : Html msg\n\
\leftAside =\n\
\  aside\n\
\    [ id \"drawer-navigation\"\n\
\    , class \"fixed top-0 left-0 z-5 w-64 h-screen transition-transform -translate-x-full sm:translate-x-0\"\n\
\    , Hx.ariaLabel \"Sidebar\"\n\
\    ]\n\
\    [ div\n\
\        [ class \"overflow-y-auto py-4 px-3 h-full bg-white border-r border-gray-200 dark:bg-gray-800 dark:border-gray-700\" ]\n\
\        [ a\n\
\            [ href R.home, class \"flex items-center pl-2 mb-5\" ]\n\
\            [ img [ src \"/imgs/logo_1.jpeg\", class \"mr-3 h-6 sm:h-8\", alt \"KnowOps Logo\" ] []\n\
\            , span [ class \"self-center text-2xl font-semibold whitespace-nowrap dark:text-white\" ] [ text \"KnowOps\" ]\n\
\            ]\n\
\        , minimizingSearch\n\
\        , ul [ class \"pt-2 mt-2space-y-2\" ]\n\
\          (List.map (leftAsideItemR Nothing) leftPartAItems)\n\
\\n\
\        , ul [ class \"pt-5 mt-5 space-y-2 border-t border-gray-200 dark:border-gray-700\" ]\n\
\                (List.map (leftAsideItemR Nothing) leftPartBItems)\n\
\        ]\n\
\    , div\n\
\        [ class \"absolute bottom-0 left-0 justify-center p-4 w-full bg-white dark:bg-gray-800 z-20\" ]\n\
\        [ p\n\
\            [ class \"text-sm text-gray-500 dark:text-gray-400\"\n\
\            ]\n\
\            [ text \"Space left\" ]\n\
\        , div\n\
\            [ class \"font-medium text-gray-900 dark:text-white\"\n\
\            ]\n\
\            [ text \"70 of 150 GB\" ]\n\
\        , div\n\
\            [ class \"w-full bg-gray-200 rounded-full h-2.5 mt-2 mb-4 dark:bg-gray-700\" ]\n\
\            [ div [ class \"bg-green-500 h-2.5 rounded-full\", style \"width\" \"50%\" ] [] ]\n\
\        , a\n\
\            [ href \"#\"\n\
\            , class \"inline-flex items-center justify-center w-full py-2.5 px-5 text-sm font-medium text-gray-900 focus:outline-none bg-white rounded-lg border border-gray-200 hover:bg-gray-100 hover:text-blue-700 focus:z-10 focus:ring-4 focus:ring-gray-200 dark:focus:ring-gray-700 dark:bg-gray-800 dark:text-gray-300 dark:border-gray-600 dark:hover:text-white dark:hover:bg-gray-700\"\n\
\            ]\n\
\            [ S.svg\n\
\                [ attribute \"aria-hidden\" \"true\"\n\
\                , Sa.class \"mr-1 w-5 h-5\"\n\
\                , Sa.fill \"none\"\n\
\                , Sa.stroke \"currentColor\"\n\
\                , Sa.viewBox \"0 0 24 24\"\n\
\                ]\n\
\                [ S.path\n\
\                    [ attribute \"strokelinecap\" \"round\"\n\
\                    , attribute \"strokelinejoin\" \"round\"\n\
\                    , attribute \"strokewidth\" \"2\"\n\
\                    , Sa.d \"M17 16l4-4m0 0l-4-4m4 4H7m6 4v1a3 3 0 01-3 3H6a3 3 0 01-3-3V7a3 3 0 013-3h4a3 3 0 013 3v1\"\n\
\                    ]\n\
\                    []\n\
\                ]\n\
\            , text \" Contact Faculty Team \" ]\n\
\        ]\n\
\    ]\n\
\\n\
\\n\
\minimizingSearch : Html msg\n\
\minimizingSearch =\n\
\  H.form\n\
\      [ action \"#\"\n\
\      , method \"GET\"\n\
\      , class \"md:hidden pb-2 border-b border-gray-200 dark:border-gray-700\"\n\
\      ]\n\
\      [ label\n\
\          [ attribute \"htmlfor\" \"sidebar-search\"\n\
\          , class \"sr-only\"\n\
\          ]\n\
\          [ text \"Search\" ]\n\
\      , div\n\
\          [ class \"relative\"\n\
\          ]\n\
\          [ div\n\
\              [ class \"flex absolute inset-y-0 left-0 items-center pl-3 pointer-events-none\"\n\
\              ]\n\
\              [ S.svg\n\
\                  [ Sa.class \"w-5 h-5 text-gray-500 dark:text-gray-400\"\n\
\                  , Sa.fill \"currentColor\"\n\
\                  , Sa.viewBox \"0 0 20 20\"\n\
\                  ]\n\
\                  [ S.path\n\
\                      [ attribute \"fillrule\" \"evenodd\"\n\
\                      , attribute \"cliprule\" \"evenodd\"\n\
\                      , Sa.d \"M8 4a4 4 0 100 8 4 4 0 000-8zM2 8a6 6 0 1110.89 3.476l4.817 4.817a1 1 0 01-1.414 1.414l-4.816-4.816A6 6 0 012 8z\"\n\
\                      ]\n\
\                      []\n\
\                  ]\n\
\              ]\n\
\          , input\n\
\              [ type_ \"text\"\n\
\              , name \"search\"\n\
\              , id \"sidebar-search\"\n\
\              , class \"bg-gray-50 border border-gray-300 text-gray-900 text-sm rounded-lg focus:ring-primary-500 focus:border-primary-500 block w-full pl-10 p-2 dark:bg-gray-700 dark:border-gray-600 dark:placeholder-gray-400 dark:text-white dark:focus:ring-primary-500 dark:focus:border-primary-500\"\n\
\              , placeholder \"Search\"\n\
\              ]\n\
\              []\n\
\          ]\n\
\      ]\n\
\\n\
\\n\
\leftAsideItemR : Maybe String -> ItemLA msg -> Html msg\n\
\leftAsideItemR parentID anItem =\n\
\  case anItem of\n\
\    Simple sItem ->\n\
\      let\n\
\        fullPMid =\n\
\          case parentID of\n\
\            Just anID ->\n\
\              -- anID ++ \"_\" ++ sItem.mid\n\
\              sItem.mid\n\
\            Nothing ->\n\
\              sItem.mid\n\
\        xHeaders = case sItem.params of\n\
\          Nothing -> \"{&quot;mid&quot;:&quot;\" ++ fullPMid ++ \"&quot;}\"\n\
\          Just aValue ->  \"{&quot;mid&quot;:&quot;\" ++ fullPMid ++ \"&quot;,&quot;params&quot;:\" ++ aValue ++ \"}\"\n\
\      in\n\
\        li [] [\n\
\          a\n\
\            [ Mx.wsSend, Mx.hxTarget \"#mainContainer\", Mx.hxSwap \"outerHtml\"\n\
\            , Mx.hxHeaders xHeaders\n\
\            , class \"flex items-center p-2 text-base font-normal text-gray-900 rounded-lg dark:text-white hover:bg-gray-100 dark:hover:bg-gray-700 group\"\n\
\            ]\n\
\            <| L.concat [\n\
\                case sItem.icon of\n\
\                  Just anIcon ->\n\
\                    [ anIcon ]\n\
\                  Nothing ->\n\
\                      []\n\
\              , [ span [ class \"flex-1 ml-3 whitespace-nowrap\" ] [ text sItem.title ] ]\n\
\              , case sItem.postIcon of\n\
\                  Just aPostIcon ->\n\
\                    [ aPostIcon ]\n\
\                  Nothing ->\n\
\                    []\n\
\              ]\n\
\        ]\n\
\    Composed cItem ->\n\
\      let\n\
\        fullPMid =\n\
\          case parentID of\n\
\            Just anID ->\n\
\              anID ++ \"_\" ++ cItem.uid\n\
\            Nothing ->\n\
\              cItem.uid\n\
\      in\n\
\        let\n\
\          collapseTarget = \"menuDropper-\" ++ cItem.uid\n\
\        in\n\
\        li []\n\
\          [ button\n\
\              [ type_ \"button\"\n\
\              , class \"flex items-center p-2 w-full text-base font-normal text-gray-900 rounded-lg transition duration-75 group hover:bg-gray-100 dark:text-white dark:hover:bg-gray-700\"\n\
\              , Hx.ariaControls collapseTarget\n\
\              , Fb.collapseToggle collapseTarget\n\
\              ]\n\
\              <| (case cItem.icon of\n\
\                      Just anIcon ->\n\
\                        [ anIcon ]\n\
\                      Nothing ->\n\
\                        []\n\
\                  )\n\
\                  ++ [\n\
\                      span [ class \"flex-1 ml-3 text-left whitespace-nowrap\" ] [ text cItem.title ]\n\
\                    , S.svg\n\
\                        [ Hx.ariaHidden \"true\"\n\
\                        , Sa.class \"w-6 h-6\"\n\
\                        , Sa.fill \"currentColor\"\n\
\                        , Sa.viewBox \"0 0 20 20\"\n\
\                        ]\n\
\                        [ S.path\n\
\                            [ Sa.fillRule \"evenodd\"\n\
\                            , Sa.d \"M5.293 7.293a1 1 0 011.414 0L10 10.586l3.293-3.293a1 1 0 111.414 1.414l-4 4a1 1 0 01-1.414 0l-4-4a1 1 0 010-1.414z\"\n\
\                            , Sa.clipRule \"evenodd\"\n\
\                            ]\n\
\                            []\n\
\                        ]\n\
\                    ]\n\
\          , ul [ id collapseTarget, class \"hidden py-2 space-y-2\" ]\n\
\              (List.map (leftAsideItemR <| Just fullPMid) cItem.children)\n\
\          ]\n"

navBarVerbatim :: Bs.ByteString
navBarVerbatim = "module Components.NavBar exposing (navBarAnon, navBarProtected)\n\
\\n\
\import List as L\n\
\\n\
\import Html.String as H exposing (..)\n\
\import Html.String.Attributes as A exposing (..)\n\
\import Sup.Svg as S\n\
\import Sup.Attributes as Sa\n\
\import Html.Symbols exposing (nbsp)\n\
\import Html.Extra as Hx\n\
\import Html.Htmx as Ht\n\
\import Html.Flowbite as Fb\n\
\import Html.StaticRender as Sr\n\
\\n\
\import Components.Icons exposing (..)\n\
\\n\
\import ClerkJs exposing (..)\n\
\\n\
\import Fuddle.Routes as R\n\
\import Fuddle.Locales as L exposing (l)\n\
\\n\
\\n\
\type alias TopMenu = {\n\
\  name : String\n\
\  , href : String\n\
\ }\n\
\\n\
\\n\
\topMenu : List TopMenu\n\
\topMenu = [\n\
\    { name = \"features\", href = R.features }\n\
\    , { name = \"products\", href = R.products }\n\
\    , { name = \"about\", href = R.about }\n\
\    , { name = \"contact\", href = R.contact }\n\
\  ]\n\
\\n\
\\n\
\navBarAnon : Sr.RenderOptions -> Html msg\n\
\navBarAnon rtOpts =\n\
\  div [ class \"flex flex-wrap justify-between items-center mx-auto max-w-screen-xl\" ] [\n\
\      a [ href R.home, class \"flex items-center\" ] [\n\
\          img [\n\
\              src \"/imgs/logo_1.jpeg\"\n\
\            , class <| L.pickForDir rtOpts.locale \"mr-3\" \"ml-3\" ++ \" w-auto h-6 sm:h-9\"\n\
\            , alt \"Z1-4L\"\n\
\            , width 40\n\
\            , height 36\n\
\            ] []\n\
\        , span [\n\
\              class \"self-center text-xl font-semibold whitespace-nowrap dark:text-white\"\n\
\            ] [\n\
\              text \" Z1-4L \"\n\
\            ]\n\
\        ]\n\
\    , div [ class \"flex items-center lg:order-2\" ] <|\n\
\          authButtons rtOpts.authenticated ++\n\
\        [ button [\n\
\              attribute \"data-collapse-toggle\" \"mobile-menu-2\"\n\
\            , type_ \"button\"\n\
\            , class \"inline-flex items-center p-2 ml-1 text-sm text-gray-500 rounded-lg lg:hidden hover:bg-gray-100 focus:outline-none focus:ring-2 focus:ring-gray-200 dark:text-gray-400 dark:hover:bg-gray-700 dark:focus:ring-gray-600\"\n\
\            , attribute \"aria-controls\" \"mobile-menu-2\"\n\
\            , attribute \"aria-expanded\" \"false\"\n\
\            ] [\n\
\              span [ class \"sr-only\" ] [ text \"Open main menu\" ]\n\
\            , S.svg [\n\
\                   Sa.class \"w-6 h-6\"\n\
\                , Sa.fill \"currentColor\"\n\
\                , Sa.viewBox \"0 0 20 20\"\n\
\                ] [\n\
\                  S.path [\n\
\                      Sa.fillRule \"evenodd\"\n\
\                    , Sa.d \"M3 5a1 1 0 011-1h12a1 1 0 110 2H4a1 1 0 01-1-1zM3 10a1 1 0 011-1h12a1 1 0 110 2H4a1 1 0 01-1-1zM3 15a1 1 0 011-1h12a1 1 0 110 2H4a1 1 0 01-1-1z\"\n\
\                    , Sa.clipRule \"evenodd\"\n\
\                    ] []\n\
\                ]\n\
\            , S.svg [\n\
\                    class \"hidden w-6 h-6\"\n\
\                  , Sa.fill \"currentColor\"\n\
\                  , Sa.viewBox \"0 0 20 20\"\n\
\                  ] [\n\
\                    S.path [\n\
\                       Sa.fillRule \"evenodd\"\n\
\                      , Sa.d \"M4.293 4.293a1 1 0 011.414 0L10 8.586l4.293-4.293a1 1 0 111.414 1.414L11.414 10l4.293 4.293a1 1 0 01-1.414 1.414L10 11.414l-4.293 4.293a1 1 0 01-1.414-1.414L8.586 10 4.293 5.707a1 1 0 010-1.414z\"\n\
\                      , Sa.clipRule \"evenodd\"\n\
\                      ] []\n\
\                  ]\n\
\            ]\n\
\          ]\n\
\    , div [\n\
\          class \"hidden px-10 justify-between items-center w-full lg:flex lg:w-auto lg:order-1\"\n\
\        , id \"mobile-menu-2\"\n\
\        ] [\n\
\          ul [ class <| L.pickForDir rtOpts.locale \"lg:space-x-8\" \"lg:gap-12\" ++ \" flex flex-col mt-4 font-medium lg:flex-row lg:mt-0\" ]\n\
\            (L.map (topMenuItemR rtOpts) topMenu)\n\
\        ]\n\
\    ]\n\
\\n\
\\n\
\topMenuItemR rtOpts anItem =\n\
\  a [\n\
\      href (\"/\" ++ rtOpts.locale ++ anItem.href)\n\
\    , class <| L.pickForDir rtOpts.locale \"pr-4 pl-3\" \"pl-3 pr-4\" ++ \" block py-2 border-b border-gray-100 text-primary-600 hover:bg-gray-50 lg:hover:bg-transparent lg:border-0 lg:hover:text-primary-600 lg:p-0 dark:text-primary-500 lg:dark:hover:text-primary-500 dark:hover:bg-gray-700 dark:hover:text-primary-500 lg:dark:hover:bg-transparent dark:border-gray-700\"\n\
\    , attribute \"aria-current\" \"page\"\n\
\    ]\n\
\    (l <| \"NavBar.\" ++ anItem.name)\n\
\\n\
\\n\
\-- Html.Element msg (with text)\n\
\authButtons authStatus =\n\
\  if authStatus then [\n\
\    a [ href R.home\n\
\        , class \"mx-10 text-red-900 dark:text-red-300 hover:bg-gray-50 focus:ring-4 focus:ring-gray-300 font-medium rounded-lg text-sm px-4 py-2 lg:px-5 lg:py-2.5 mr-2 dark:hover:bg-gray-700 focus:outline-none dark:focus:ring-gray-800\"\n\
\      ]\n\
\      <| (l \"app\") ++ [ nbsp ]\n\
\  , p [ class \"text-gray-800 dark:text-white hover:bg-gray-50 focus:ring-4 focus:ring-gray-300 font-medium rounded-lg text-sm px-4 py-2 lg:px-5 lg:py-2.5 mr-2\" ] [ nbsp ]\n\
\  , userBtn\n\
\  , p [ class \"text-gray-800 dark:text-white hover:bg-gray-50 focus:ring-4 focus:ring-gray-300 font-medium rounded-lg text-sm px-4 py-2 lg:px-5 lg:py-2.5 mr-2\" ] [ nbsp ]\n\
\  , signOutBtn [] [\n\
\      button [\n\
\          class \"text-gray-800 dark:text-white hover:bg-gray-50 focus:ring-4 focus:ring-gray-300 font-medium rounded-lg text-sm px-4 py-2 lg:px-5 lg:py-2.5 mr-2 dark:hover:bg-gray-700 focus:outline-none dark:focus:ring-gray-800\"\n\
\        ] (l \"NavBar.signout\")\n\
\    ]\n\
\  ]\n\
\  else [\n\
\    signInBtn [] [\n\
\      button [\n\
\          class \"text-gray-800 dark:text-white hover:bg-gray-50 focus:ring-4 focus:ring-gray-300 font-medium rounded-lg text-sm px-4 py-2 lg:px-5 lg:py-2.5 mr-2 dark:hover:bg-gray-700 focus:outline-none dark:focus:ring-gray-800\"\n\
\        ]\n\
\        (l \"NavBar.signin\")\n\
\    ]\n\
\    , signUpBtn [] [\n\
\      button [\n\
\         class \"text-white bg-primary-700 hover:bg-primary-800 focus:ring-4 focus:ring-primary-300 font-medium rounded-lg text-sm px-4 py-2 lg:px-5 lg:py-2.5 mr-2 dark:bg-primary-600 dark:hover:bg-primary-700 focus:outline-none dark:focus:ring-primary-800\"\n\
\       ] (l <| \"NavBar.signup\")\n\
\    ]\n\
\  ]\n\
\\n\
\\n\
\type alias NotificationItem msg = {\n\
\  avatar : String\n\
\  , newFlag : Bool\n\
\  , senders : List String\n\
\  , content : List (Html msg)\n\
\  , time : String\n\
\  , icon : Html msg\n\
\ }\n\
\\n\
\\n\
\type alias AppItem msg = {\n\
\  title : String\n\
\  , icon : Html msg\n\
\  , href : String\n\
\  , mid : Maybe String\n\
\ }\n\
\\n\
\\n\
\type alias UserProfile = {\n\
\  name : String\n\
\  , email : String\n\
\  , avatar : String\n\
\ }\n\
\\n\
\\n\
\navBarProtected : Html msg\n\
\navBarProtected =\n\
\  let\n\
\    aUser = fakeProfile\n\
\  in\n\
\  nav [\n\
\        class \"bg-white border-b border-gray-200 px-4 lg:px-6 py-2.5 dark:bg-gray-800 dark:border-gray-700 fixed left-0 right-0 top-0 z-10\"\n\
\    ]\n\
\    [ div [ class \"flex flex-wrap justify-between items-center\" ]\n\
\        [\n\
\            div [ class \"flex justify-start items-center\" ]\n\
\              [\n\
\                navDrawerButton\n\
\              , a\n\
\                  [ href R.home , class \"flex mr-4 items-center justify-center\" ]\n\
\                  [ img [ src \"/imgs/logo_1.jpeg\", class \"mr-3 h-8\", alt \"KnowOps Logo\" ] []\n\
\                  , span\n\
\                      [ class \"self-center text-2xl font-semibold whitespace-nowrap dark:text-white\" ] [ text \"KnowOps\" ]\n\
\                  ]\n\
\              , topSearchField\n\
\              ]\n\
\          , div [ class \"flex items-center lg:order-2\" ]\n\
\              <| toggleSearchButton\n\
\                :: notifications\n\
\                ++ apps\n\
\                ++ profileMenu aUser\n\
\        ]\n\
\    ]\n\
\\n\
\\n\
\navDrawerButton =\n\
\    button\n\
\        [ Fb.drawerTarget \"drawer-navigation\"\n\
\        , Fb.drawerToggle \"drawer-navigation\"\n\
\        , Hx.ariaControls \"drawer-navigation\"\n\
\        , type_ \"button\"\n\
\        , class \"p-2 mr-2 text-gray-600 rounded-lg cursor-pointer md:hidden hover:text-gray-900 hover:bg-gray-100 focus:bg-gray-100 dark:focus:bg-gray-700 focus:ring-2 focus:ring-gray-100 dark:focus:ring-gray-700 dark:text-gray-400 dark:hover:bg-gray-700 dark:hover:text-white\"\n\
\        ]\n\
\        [ span\n\
\            [ class \"sr-only\"\n\
\            ]\n\
\            [ text \"Toggle sidebar\" ]\n\
\        , S.svg\n\
\            [ Hx.ariaHidden \"true\"\n\
\            , Sa.class \"w-6 h-6\"\n\
\            , Sa.fill \"currentColor\"\n\
\            , Sa.viewBox \"0 0 20 20\"\n\
\            ]\n\
\            [ S.path\n\
\                [ attribute \"fillrule\" \"evenodd\"\n\
\                , Sa.d \"M3 5a1 1 0 011-1h12a1 1 0 110 2H4a1 1 0 01-1-1zM3 10a1 1 0 011-1h6a1 1 0 110 2H4a1 1 0 01-1-1zM3 15a1 1 0 011-1h12a1 1 0 110 2H4a1 1 0 01-1-1z\"\n\
\                , attribute \"cliprule\" \"evenodd\"\n\
\                ]\n\
\                []\n\
\            ]\n\
\        ]\n\
\\n\
\\n\
\topSearchField =\n\
\  H.form\n\
\    [ action \"#\"\n\
\    , id \"search-form\"\n\
\    , method \"GET\"\n\
\    , class \"hidden md:block lg:pl-2\"\n\
\    ]\n\
\    [ label [ for \"topbar-search\", class \"sr-only\" ] [ text \"'discuss'\" ]\n\
\    , div [ class \"relative md:w-64 lg:w-96\" ]\n\
\        [ div\n\
\            [ class \"flex absolute inset-y-0 left-0 items-center pl-3 pointer-events-none\" ]\n\
\            [ S.svg\n\
\                [ Sa.class \"w-5 h-5 text-gray-500 dark:text-gray-400\", Sa.fill \"currentColor\", Sa.viewBox \"0 0 20 20\" ]\n\
\                [ S.path\n\
\                    [ Sa.fillRule \"evenodd\", Sa.clipRule \"evenodd\"\n\
\                    , Sa.d \"M8 4a4 4 0 100 8 4 4 0 000-8zM2 8a6 6 0 1110.89 3.476l4.817 4.817a1 1 0 01-1.414 1.414l-4.816-4.816A6 6 0 012 8z\"\n\
\                    ]\n\
\                    []\n\
\                ]\n\
\            ]\n\
\        , input\n\
\            [ id \"topbar-search\", type_ \"text\", name \"email\", placeholder \"discuss\"\n\
\            , class \"bg-gray-50 border border-gray-300 text-gray-900 text-sm rounded-lg focus:ring-primary-500 focus:border-primary-500 block w-full pl-10 p-2.5 dark:bg-gray-700 dark:border-gray-600 dark:placeholder-gray-400 dark:text-white dark:focus:ring-primary-500 dark:focus:border-primary-500\"\n\
\            ]\n\
\            []\n\
\        ]\n\
\    ]\n\
\\n\
\\n\
\toggleSearchButton =\n\
\  button\n\
\    [ type_ \"button\"\n\
\    , Fb.drawerToggle \"search-form\", Fb.drawerTarget \"search-form\", Hx.ariaControls \"search-form\"\n\
\    , class \"p-2 mr-1 text-gray-500 rounded-lg md:hidden hover:text-gray-900 hover:bg-gray-100 dark:text-gray-400 dark:hover:text-white dark:hover:bg-gray-700 focus:ring-4 focus:ring-gray-300 dark:focus:ring-gray-600\"\n\
\    ]\n\
\    [ span\n\
\        [ class \"sr-only\" ] [ text \"Toggle search\" ]\n\
\    , S.svg\n\
\        [ Hx.ariaHidden \"true\", Sa.class \"w-6 h-6\", Sa.fill \"currentColor\", Sa.viewBox \"0 0 20 20\" ]\n\
\        [ S.path\n\
\            [ Sa.clipRule \"evenodd\", Sa.fillRule \"evenodd\"\n\
\            , Sa.d \"M8 4a4 4 0 100 8 4 4 0 000-8zM2 8a6 6 0 1110.89 3.476l4.817 4.817a1 1 0 01-1.414 1.414l-4.816-4.816A6 6 0 012 8z\"\n\
\            ]\n\
\            []\n\
\        ]\n\
\    ]\n\
\\n\
\\n\
\notificationItemRow : NotificationItem msg -> Html msg\n\
\notificationItemRow item =\n\
\  let\n\
\    tSenders = case item.senders of\n\
\      [sender] ->\n\
\        span [ class \"font-semibold text-gray-900 dark:text-white\" ] [ text sender ]\n\
\      sender :: others ->\n\
\        span [ class \"font-semibold text-gray-900 dark:text-white\" ]\n\
\          [ text <| sender ++ \" and \" ++ String.fromInt (L.length others) ++ \" others\"]\n\
\      _ -> text \"\"\n\
\    firstSender = case item.senders of\n\
\      [sender] -> sender\n\
\      _ -> \"\"\n\
\  in\n\
\  a\n\
\    [ href \"#\"\n\
\    , class \"flex py-3 px-4 border-b hover:bg-gray-100 dark:hover:bg-gray-600 dark:border-gray-600\"\n\
\    ]\n\
\    [ div\n\
\        [ class \"flex-shrink-0\" ]\n\
\        [ img [ class \"w-11 h-11 rounded-full\", src item.avatar, alt (firstSender ++ \" avatar\") ] []\n\
\      , div\n\
\          [ class \"flex absolute justify-center items-center ml-6 -mt-5 w-5 h-5 rounded-full border border-white bg-primary-700 dark:border-gray-700\" ]\n\
\          [ item.icon ]\n\
\    ]\n\
\    , div [ class \"pl-3 w-full\" ]\n\
\        [ div\n\
\            [ class \"text-gray-500 font-normal text-sm mb-1.5 dark:text-gray-400\"\n\
\            ]\n\
\            <|\n\
\              [ text (if item.newFlag then \"New message from \" else \"\")\n\
\              , tSenders\n\
\              , text \" \"\n\
\              ]\n\
\              ++ item.content\n\
\        , div [ class \"text-xs font-medium text-primary-600 dark:text-primary-500\" ]\n\
\            [ text item.time ]\n\
\        ]\n\
\    ]\n\
\\n\
\\n\
\fakeNotifications : List (NotificationItem msg)\n\
\fakeNotifications = [\n\
\  {\n\
\    avatar = \"https://flowbite.s3.amazonaws.com/blocks/marketing-ui/avatars/bonnie-green.png\"\n\
\    , newFlag = True\n\
\    , senders = [\"Bonnie Green\"]\n\
\    , content = [ text \":\\\"Hey, what's up? All set for the presentation?\\\"\" ]\n\
\    , time = \"a few moments ago\"\n\
\    , icon = newIcon\n\
\  }\n\
\  , {\n\
\    avatar = \"https://flowbite.s3.amazonaws.com/blocks/marketing-ui/avatars/jese-leos.png\"\n\
\    , newFlag = False\n\
\    , senders = [\"Jese Leos\", \"Joseph McFall\", \"Roberta Casas\", \"Leslie Livingston\", \"Robert Brown\"]\n\
\    , content = [ text \"started following you.\" ]\n\
\    , time = \"10 minutes ago\"\n\
\    , icon = pplPlus\n\
\  }\n\
\  , {\n\
\    avatar = \"https://flowbite.s3.amazonaws.com/blocks/marketing-ui/avatars/joseph-mcfall.png\"\n\
\    , newFlag = False\n\
\    , senders = [\"Joseph McFall\", \"John Doe\", \"Jane Doe\", \"Alice Doe\", \"Bob Doe\", \"Jane Doe\", \"Alice Doe\", \"Bob Doe\"]\n\
\    , content = [ text \"love your story. See it and view more stories.\" ]\n\
\    , time = \"44 minutes ago\"\n\
\    , icon = redHeart\n\
\  }\n\
\  , {\n\
\    avatar = \"https://flowbite.s3.amazonaws.com/blocks/marketing-ui/avatars/roberta-casas.png\"\n\
\    , newFlag = False\n\
\    , senders = [\"Leslie Livingston\"]\n\
\    , content = [\n\
\            text \"mentioned you in a comment: \"\n\
\            , span [ class \"font-medium text-primary-600 dark:text-primary-500\" ] [ text \"@bonnie.green\" ]\n\
\            , text \" what do you say?\"\n\
\            ]\n\
\    , time = \"1 hour ago\"\n\
\    , icon = greenMessage\n\
\  }\n\
\  , {\n\
\    avatar = \"https://flowbite.s3.amazonaws.com/blocks/marketing-ui/avatars/robert-brown.png\"\n\
\    , newFlag = False\n\
\    , senders = [\"Robert Brown\"]\n\
\    , content = [ text \"posted a new video: Glassmorphism - learn how to implement the new design trend.\" ]\n\
\    , time = \"3 hours ago\"\n\
\    , icon = purpleCamera\n\
\  }\n\
\ ]\n\
\\n\
\notifications : List (Html msg)\n\
\notifications = [\n\
\  button\n\
\      [ type_ \"button\", attribute \"data-dropdown-toggle\" \"notification-dropdown\"\n\
\      , class \"p-2 mr-1 text-gray-500 rounded-lg hover:text-gray-900 hover:bg-gray-100 dark:text-gray-400 dark:hover:text-white dark:hover:bg-gray-700 focus:ring-4 focus:ring-gray-300 dark:focus:ring-gray-600\"\n\
\      ]\n\
\      [\n\
\        span [ class \"sr-only\" ] [ text \"View notifications\" ]\n\
\      , bellIcon\n\
\      ]\n\
\  , div\n\
\    [ id \"notification-dropdown\"\n\
\      , class \"hidden overflow-hidden z-50 my-4 max-w-sm text-base list-none bg-white divide-y divide-gray-100 shadow-lg dark:divide-gray-600 dark:bg-gray-700 rounded-xl\"\n\
\    ]\n\
\    [ div\n\
\        [ class \"block py-2 px-4 text-base font-medium text-center text-gray-700 bg-gray-50 dark:bg-gray-600 dark:text-gray-300\" ]\n\
\        [ text \" Notifications \" ]\n\
\      , div [] (List.map notificationItemRow fakeNotifications)\n\
\      , a\n\
\        [ href \"#\"\n\
\        , class \"block py-2 text-md font-medium text-center text-gray-900 bg-gray-50 hover:bg-gray-100 dark:bg-gray-600 dark:text-white dark:hover:underline\"\n\
\        ]\n\
\        [ div\n\
\            [ class \"inline-flex items-center\"\n\
\            ]\n\
\            [ S.svg\n\
\                [ Hx.ariaHidden \"true\"\n\
\                , Sa.class \"mr-2 w-4 h-4 text-gray-500 dark:text-gray-400\"\n\
\                , Sa.fill \"currentColor\"\n\
\                , Sa.viewBox \"0 0 20 20\"\n\
\                , Sa.xmlns \"http://www.w3.org/2000/svg\"\n\
\                ]\n\
\                [ S.path\n\
\                    [ Sa.d \"M10 12a2 2 0 100-4 2 2 0 000 4z\"\n\
\                    ]\n\
\                    []\n\
\                , S.path\n\
\                    [ Sa.fillRule \"evenodd\"\n\
\                    , Sa.d \"M.458 10C1.732 5.943 5.522 3 10 3s8.268 2.943 9.542 7c-1.274 4.057-5.064 7-9.542 7S1.732 14.057.458 10zM14 10a4 4 0 11-8 0 4 4 0 018 0z\"\n\
\                    , Sa.clipRule \"evenodd\"\n\
\                    ]\n\
\                    []\n\
\                ]\n\
\            , text \" View all \" ]\n\
\        ]\n\
\    ]\n\
\  ]\n\
\\n\
\\n\
\appItems : List (AppItem msg)\n\
\appItems = [\n\
\  {\n\
\    title = \"Things\"\n\
\    , href = \"#\"\n\
\    , mid = Just \"demo_1\"\n\
\    , icon =\n\
\        S.svg\n\
\          [ Hx.ariaHidden \"true\"\n\
\          , class \"mx-auto mb-1 w-7 h-7 text-gray-400 group-hover:text-gray-500 dark:text-gray-400 dark:group-hover:text-gray-400\"\n\
\          , Sa.fill \"currentColor\"\n\
\          , Sa.viewBox \"0 0 20 20\"\n\
\          ]\n\
\          [ S.path [ Sa.d \"M10 2a4 4 0 00-4 4v1H5a1 1 0 00-.994.89l-1 9A1 1 0 004 18h12a1 1 0 00.994-1.11l-1-9A1 1 0 0015 7h-1V6a4 4 0 00-4-4zm2 5V6a2 2 0 10-4 0v1h4zm-6 3a1 1 0 112 0 1 1 0 01-2 0zm7-1a1 1 0 100 2 1 1 0 000-2zm-7 4a1 1 0 112 0 1 1 0 01-2 0z\" ] []]\n\
\  }\n\
\  , {\n\
\    title = \"Collective\"\n\
\    , href = \"#\"\n\
\    , mid = Just \"demo_2\"\n\
\    , icon =\n\
\        S.svg\n\
\          [ Hx.ariaHidden \"true\"\n\
\          , class \"mx-auto mb-1 w-7 h-7 text-gray-400 group-hover:text-gray-500 dark:text-gray-400 dark:group-hover:text-gray-400\"\n\
\          , Sa.fill \"currentColor\"\n\
\          , Sa.viewBox \"0 0 20 20\"\n\
\          ]\n\
\          [ S.path [ Sa.d \"M13 6a3 3 0 11-6 0 3 3 0 016 0zM18 8a2 2 0 11-4 0 2 2 0 014 0zM14 15a4 4 0 00-8 0v3h8v-3zM6 8a2 2 0 11-4 0 2 2 0 014 0zM16 18v-3a5.972 5.972 0 00-.75-2.906A3.005 3.005 0 0119 15v3h-3zM4.75 12.094A5.973 5.973 0 004 15v3H1v-3a3 3 0 013.75-2.906z\" ] []]\n\
\  }\n\
\  , {\n\
\    title = \"Mentor\"\n\
\    , href = \"#\"\n\
\    , mid = Just \"demo_3\"\n\
\    , icon =\n\
\        S.svg\n\
\          [ Hx.ariaHidden \"true\"\n\
\          , class \"mx-auto mb-1 w-7 h-7 text-gray-400 group-hover:text-gray-500 dark:text-gray-400 dark:group-hover:text-gray-400\"\n\
\          , Sa.fill \"currentColor\"\n\
\          , Sa.viewBox \"0 0 20 20\"\n\
\          ]\n\
\          [ S.path [ Sa.d \"M5 3a2 2 0 00-2 2v2a2 2 0 002 2h2a2 2 0 002-2V5a2 2 0 00-2-2H5zM5 11a2 2 0 00-2 2v2a2 2 0 002 2h2a2 2 0 002-2v-2a2 2 0 00-2-2H5zM11 5a2 2 0 012-2h2a2 2 0 012 2v2a2 2 0 01-2 2h-2a2 2 0 01-2-2V5zM11 13a2 2 0 012-2h2a2 2 0 012 2v2a2 2 0 01-2 2h-2a2 2 0 01-2-2v-2z\" ] []]\n\
\  }\n\
\  , {\n\
\    title = \"Scenarios\"\n\
\    , href = \"#\"\n\
\    , mid = Just \"prez.topBrowse\"\n\
\    , icon =\n\
\        S.svg\n\
\          [ Hx.ariaHidden \"true\"\n\
\          , class \"mx-auto mb-1 w-7 h-7 text-gray-400 group-hover:text-gray-500 dark:text-gray-400 dark:group-hover:text-gray-400\"\n\
\          , Sa.fill \"currentColor\"\n\
\          , Sa.viewBox \"0 0 20 20\"\n\
\          ]\n\
\          [ S.path [ Sa.d \"M18 10a8 8 0 11-16 0 8 8 0 0116 0zm-6-3a2 2 0 11-4 0 2 2 0 014 0zm-2 4a5 5 0 00-4.546 2.916A5.986 5.986 0 0010 16a5.986 5.986 0 004.546-2.084A5 5 0 0010 11z\" ] []]\n\
\  }\n\
\  , {\n\
\    title = \"Experience\"\n\
\    , href = \"#\"\n\
\    , mid = Just \"demo_4\"\n\
\    , icon =\n\
\        S.svg\n\
\          [ Hx.ariaHidden \"true\"\n\
\          , class \"mx-auto mb-1 w-7 h-7 text-gray-400 group-hover:text-gray-500 dark:text-gray-400 dark:group-hover:text-gray-400\"\n\
\          , Sa.fill \"currentColor\"\n\
\          , Sa.viewBox \"0 0 20 20\"\n\
\          ]\n\
\          [ S.path [ Sa.d \"M11.49 3.17c-.38-1.56-2.6-1.56-2.98 0a1.532 1.532 0 01-2.286.948c-1.372-.836-2.942.734-2.106 2.106.54.886.061 2.042-.947 2.287-1.561.379-1.561 2.6 0 2.978a1.532 1.532 0 01.947 2.287c-.836 1.372.734 2.942 2.106 2.106a1.532 1.532 0 012.287.947c.379 1.561 2.6 1.561 2.978 0a1.533 1.533 0 012.287-.947c1.372.836 2.942-.734 2.106-2.106a1.533 1.533 0 01.947-2.287c1.561-.379 1.561-2.6 0-2.978a1.532 1.532 0 01-.947-2.287c.836-1.372-.734-2.942-2.106-2.106a1.532 1.532 0 01-2.287-.947zM10 13a3 3 0 100-6 3 3 0 000 6z\" ] []]\n\
\  }\n\
\  , {\n\
\    title = \"Materials\"\n\
\    , href = \"#\"\n\
\    , mid = Nothing\n\
\    , icon =\n\
\        S.svg\n\
\          [ Hx.ariaHidden \"true\"\n\
\          , class \"mx-auto mb-1 w-7 h-7 text-gray-400 group-hover:text-gray-500 dark:text-gray-400 dark:group-hover:text-gray-400\"\n\
\          , Sa.fill \"currentColor\"\n\
\          , Sa.viewBox \"0 0 20 20\"\n\
\          ]\n\
\          [ S.path [ Sa.d \"M3 8h14v7a2 2 0 01-2 2H5a2 2 0 01-2-2V8zm5 3a1 1 0 011-1h2a1 1 0 110 2H9a1 1 0 01-1-1z\" ] []]\n\
\  }\n\
\  , {\n\
\    title = \"Shopping\"\n\
\    , href = \"#\"\n\
\    , mid = Nothing\n\
\    , icon =\n\
\        S.svg\n\
\          [ Hx.ariaHidden \"true\"\n\
\          , class \"mx-auto mb-1 w-7 h-7 text-gray-400 group-hover:text-gray-500 dark:text-gray-400 dark:group-hover:text-gray-400\"\n\
\          , Sa.fill \"currentColor\"\n\
\          , Sa.viewBox \"0 0 20 20\"\n\
\          ]\n\
\          [ S.path [ Sa.d \"M8.433 7.418c.155-.103.346-.196.567-.267v1.698a2.305 2.305 0 01-.567-.267C8.07 8.34 8 8.114 8 8c0-.114.07-.34.433-.582zM11 12.849v-1.698c.22.071.412.164.567.267.364.243.433.468.433.582 0 .114-.07.34-.433.582a2.305 2.305 0 01-.567.267z\" ] []]\n\
\  }\n\
\  , {\n\
\    title = \"Finance\"\n\
\    , href = \"#\"\n\
\    , mid = Nothing\n\
\    , icon =\n\
\        S.svg\n\
\            [ Hx.ariaHidden \"true\"\n\
\            , class \"mx-auto mb-1 w-7 h-7 text-gray-400 group-hover:text-gray-500 dark:text-gray-400 dark:group-hover:text-gray-400\"\n\
\            , Sa.fill \"currentColor\"\n\
\            , Sa.viewBox \"0 0 20 20\"\n\
\            ]\n\
\            [ S.path\n\
\                [ Sa.fillRule \"evenodd\"\n\
\                , Sa.clipRule \"evenodd\"\n\
\                , Sa.d \"M5 2a2 2 0 00-2 2v14l3.5-2 3.5 2 3.5-2 3.5 2V4a2 2 0 00-2-2H5zm2.5 3a1.5 1.5 0 100 3 1.5 1.5 0 000-3zm6.207.293a1 1 0 00-1.414 0l-6 6a1 1 0 101.414 1.414l6-6a1 1 0 000-1.414zM12.5 10a1.5 1.5 0 100 3 1.5 1.5 0 000-3z\"\n\
\                ]\n\
\                []\n\
\            ]\n\
\  }\n\
\  , {\n\
\     title = \"External\"\n\
\     , href = \"#\"\n\
\    , mid = Nothing\n\
\     , icon =\n\
\            S.svg\n\
\        [ Hx.ariaHidden \"true\"\n\
\        , class \"mx-auto mb-1 w-7 h-7 text-gray-400 group-hover:text-gray-500 dark:text-gray-400 dark:group-hover:text-gray-400\"\n\
\        , Sa.fill \"none\"\n\
\        , Sa.stroke \"currentColor\"\n\
\        , Sa.viewBox \"0 0 24 24\"\n\
\        ]\n\
\        [ S.path\n\
\            [ Sa.strokeLinecap \"round\"\n\
\            , Sa.strokeLinejoin \"round\"\n\
\            , Sa.strokeWidth \"2\"\n\
\            , Sa.d \"M11 16l-4-4m0 0l4-4m-4 4h14m-5 4v1a3 3 0 01-3 3H6a3 3 0 01-3-3V7a3 3 0 013-3h7a3 3 0 013 3v1\"\n\
\            ]\n\
\            []\n\
\        ]\n\
\      }\n\
\\n\
\ ]\n\
\\n\
\\n\
\appItemR : AppItem msg -> Html msg\n\
\appItemR aItem =\n\
\  case aItem.mid of\n\
\    Nothing ->\n\
\      a\n\
\        [ href aItem.href\n\
\        , class \"block p-4 text-center rounded-lg hover:bg-gray-100 dark:hover:bg-gray-600 group\"\n\
\        ]\n\
\        [ aItem.icon\n\
\        , div [ class \"text-sm text-gray-900 dark:text-white\" ] [ text aItem.title ]\n\
\        ]\n\
\    Just mid ->\n\
\      a (Ht.invokeWithSwap \"#mainContainer\" (\"ta_\" ++ mid) []\n\
\          [ class \"block p-4 text-center rounded-lg hover:bg-gray-100 dark:hover:bg-gray-600 group\" ]\n\
\        )\n\
\        [ aItem.icon\n\
\        , div [ class \"text-sm text-gray-900 dark:text-white\" ] [ text aItem.title ]\n\
\        ]\n\
\\n\
\\n\
\apps = [\n\
\    button\n\
\        [ type_ \"button\"\n\
\        , attribute \"data-dropdown-toggle\" \"apps-dropdown\"\n\
\        , class \"p-2 text-gray-500 rounded-lg hover:text-gray-900 hover:bg-gray-100 dark:text-gray-400 dark:hover:text-white dark:hover:bg-gray-700 focus:ring-4 focus:ring-gray-300 dark:focus:ring-gray-600\"\n\
\        ]\n\
\        [ span\n\
\            [ class \"sr-only\"\n\
\            ]\n\
\            [ text \"View notifications\" ]\n\
\        , S.svg\n\
\            [ Sa.class \"w-6 h-6\"\n\
\            , Sa.fill \"currentColor\"\n\
\            , Sa.viewBox \"0 0 20 20\"\n\
\            ]\n\
\            [ S.path\n\
\                [ Sa.d \"M5 3a2 2 0 00-2 2v2a2 2 0 002 2h2a2 2 0 002-2V5a2 2 0 00-2-2H5zM5 11a2 2 0 00-2 2v2a2 2 0 002 2h2a2 2 0 002-2v-2a2 2 0 00-2-2H5zM11 5a2 2 0 012-2h2a2 2 0 012 2v2a2 2 0 01-2 2h-2a2 2 0 01-2-2V5zM11 13a2 2 0 012-2h2a2 2 0 012 2v2a2 2 0 01-2 2h-2a2 2 0 01-2-2v-2z\"\n\
\                ]\n\
\                []\n\
\            ]\n\
\        ]\n\
\    , div\n\
\        [ class \"hidden overflow-hidden z-50 my-4 max-w-sm text-base list-none bg-white divide-y divide-gray-100 shadow-lg dark:bg-gray-700 dark:divide-gray-600 rounded-xl\"\n\
\        , id \"apps-dropdown\"\n\
\        ]\n\
\        [ div\n\
\            [ class \"block py-2 px-4 text-base font-medium text-center text-gray-700 bg-gray-50 dark:bg-gray-600 dark:text-gray-300\"\n\
\            ]\n\
\            [ text \" Apps \" ]\n\
\        , div [ class \"grid grid-cols-3 gap-4 p-4\" ]\n\
\            <| L.map appItemR appItems\n\
\        ]\n\
\ ]\n\
\\n\
\fakeProfile : UserProfile\n\
\fakeProfile = {\n\
\  name = \"Neil Sims\"\n\
\  , email = \"name@knowops.com\"\n\
\  , avatar = \"https://flowbite.s3.amazonaws.com/blocks/marketing-ui/avatars/michael-gough.png\"\n\
\ }\n\
\\n\
\\n\
\profileMenu : UserProfile -> List (Html msg)\n\
\profileMenu aProfile = [\n\
\  button\n\
\      [ type_ \"button\"\n\
\      , class \"flex mx-3 text-sm bg-gray-800 rounded-full md:mr-0 focus:ring-4 focus:ring-gray-300 dark:focus:ring-gray-600\"\n\
\      , id \"user-menu-button\"\n\
\      , attribute \"aria-expanded\" \"false\"\n\
\      , attribute \"data-dropdown-toggle\" \"dropdown\"\n\
\      ]\n\
\      [ span\n\
\          [ class \"sr-only\"\n\
\          ]\n\
\          [ text \"Open user menu\" ]\n\
\      , img\n\
\          [ class \"w-8 h-8 rounded-full\"\n\
\          , src aProfile.avatar\n\
\          , alt <| aProfile.name ++ \" avatar\"\n\
\          ]\n\
\          []\n\
\      ]\n\
\  , div\n\
\      [ id \"dropdown\"\n\
\      , class \"hidden z-50 my-4 w-56 text-base list-none bg-white divide-y divide-gray-100 shadow dark:bg-gray-700 dark:divide-gray-600 rounded-xl\"\n\
\      ]\n\
\      [ div\n\
\          [ class \"py-3 px-4\"\n\
\          ]\n\
\          [ span\n\
\              [ class \"block text-sm font-semibold text-gray-900 dark:text-white\"\n\
\              ]\n\
\              [ text \"Jane Doe\" ]\n\
\          , span\n\
\              [ class \"block text-sm text-gray-900 truncate dark:text-white\"\n\
\              ]\n\
\              [ text \"@jane_doe\" ]\n\
\          ]\n\
\      , ul\n\
\          [ class \"py-1 text-gray-700 dark:text-gray-300\"\n\
\          , attribute \"aria-labelledby\" \"dropdown\"\n\
\          ]\n\
\          [ li []\n\
\              [ a\n\
\                  [ href \"#\"\n\
\                  , class \"block py-2 px-4 text-sm hover:bg-gray-100 dark:hover:bg-gray-600 dark:text-gray-400 dark:hover:text-white\"\n\
\                  ]\n\
\                  [ text \"My profile\" ]\n\
\              ]\n\
\          , li []\n\
\              [ a\n\
\                  [ href \"#\"\n\
\                  , class \"block py-2 px-4 text-sm hover:bg-gray-100 dark:hover:bg-gray-600 dark:text-gray-400 dark:hover:text-white\"\n\
\                  ]\n\
\                  [ text \"Account settings\" ]\n\
\              ]\n\
\          ]\n\
\      , ul\n\
\          [ class \"py-1 text-gray-700 dark:text-gray-300\"\n\
\          , attribute \"aria-labelledby\" \"dropdown\"\n\
\          ]\n\
\          [ li []\n\
\              [ a\n\
\                  [ href \"#\"\n\
\                  , class \"flex items-center py-2 px-4 text-sm hover:bg-gray-100 dark:hover:bg-gray-600 dark:hover:text-white\"\n\
\                  ]\n\
\                  [ S.svg\n\
\                      [ Sa.class \"mr-2 w-5 h-5 text-gray-400\"\n\
\                      , Sa.fill \"currentColor\"\n\
\                      , Sa.viewBox \"0 0 20 20\"\n\
\                      ]\n\
\                      [ S.path\n\
\                          [ attribute \"fillrule\" \"evenodd\"\n\
\                          , Sa.d \"M3.172 5.172a4 4 0 015.656 0L10 6.343l1.172-1.171a4 4 0 115.656 5.656L10 17.657l-6.828-6.829a4 4 0 010-5.656z\"\n\
\                          , attribute \"cliprule\" \"evenodd\"\n\
\                          ]\n\
\                          []\n\
\                      ]\n\
\                  , text \" Preferences\" ]\n\
\              ]\n\
\          , li []\n\
\              [ a\n\
\                  [ href \"#\"\n\
\                  , class \"flex items-center py-2 px-4 text-sm hover:bg-gray-100 dark:hover:bg-gray-600 dark:hover:text-white\"\n\
\                  ]\n\
\                  [ S.svg\n\
\                      [ Sa.class \"mr-2 w-5 h-5 text-gray-400\"\n\
\                      , Sa.fill \"currentColor\"\n\
\                      , Sa.viewBox \"0 0 20 20\"\n\
\                      ]\n\
\                      [ S.path\n\
\                          [ Sa.d \"M7 3a1 1 0 000 2h6a1 1 0 100-2H7zM4 7a1 1 0 011-1h10a1 1 0 110 2H5a1 1 0 01-1-1zM2 11a2 2 0 012-2h12a2 2 0 012 2v4a2 2 0 01-2 2H4a2 2 0 01-2-2v-4z\"\n\
\                          ]\n\
\                          []\n\
\                      ]\n\
\                  , text \" Collections\" ]\n\
\              ]\n\
\          , li []\n\
\              [ a\n\
\                  [ href \"#\"\n\
\                  , class \"flex justify-between items-center py-2 px-4 text-sm hover:bg-gray-100 dark:hover:bg-gray-600 dark:hover:text-white\"\n\
\                  ]\n\
\                  [ span\n\
\                      [ class \"flex items-center\"\n\
\                      ]\n\
\                      [ S.svg\n\
\                          [ attribute \"aria-hidden\" \"true\"\n\
\                          , Sa.class \"mr-2 w-5 h-5 text-primary-600 dark:text-primary-500\"\n\
\                          , Sa.fill \"currentColor\"\n\
\                          , Sa.viewBox \"0 0 20 20\"\n\
\                          ]\n\
\                          [ S.path\n\
\                              [ attribute \"fillrule\" \"evenodd\"\n\
\                              , Sa.d \"M12.395 2.553a1 1 0 00-1.45-.385c-.345.23-.614.558-.822.88-.214.33-.403.713-.57 1.116-.334.804-.614 1.768-.84 2.734a31.365 31.365 0 00-.613 3.58 2.64 2.64 0 01-.945-1.067c-.328-.68-.398-1.534-.398-2.654A1 1 0 005.05 6.05 6.981 6.981 0 003 11a7 7 0 1011.95-4.95c-.592-.591-.98-.985-1.348-1.467-.363-.476-.724-1.063-1.207-2.03zM12.12 15.12A3 3 0 017 13s.879.5 2.5.5c0-1 .5-4 1.25-4.5.5 1 .786 1.293 1.371 1.879A2.99 2.99 0 0113 13a2.99 2.99 0 01-.879 2.121z\"\n\
\                              , attribute \"cliprule\" \"evenodd\"\n\
\                              ]\n\
\                              []\n\
\                          ]\n\
\                      , text \" Faculty Mode \" ]\n\
\                  , S.svg\n\
\                      [ attribute \"aria-hidden\" \"true\"\n\
\                      , Sa.class \"w-5 h-5 text-gray-400\"\n\
\                      , Sa.fill \"currentColor\"\n\
\                      , Sa.viewBox \"0 0 20 20\"\n\
\                      ]\n\
\                      [ S.path\n\
\                          [ attribute \"fillrule\" \"evenodd\"\n\
\                          , Sa.d \"M7.293 14.707a1 1 0 010-1.414L10.586 10 7.293 6.707a1 1 0 011.414-1.414l4 4a1 1 0 010 1.414l-4 4a1 1 0 01-1.414 0z\"\n\
\                          , attribute \"cliprule\" \"evenodd\"\n\
\                          ]\n\
\                          []\n\
\                      ]\n\
\                  ]\n\
\              ]\n\
\          ]\n\
\      , ul\n\
\          [ class \"py-1 text-gray-700 dark:text-gray-300\"\n\
\          , attribute \"aria-labelledby\" \"dropdown\"\n\
\          ]\n\
\          [ li []\n\
\              [ a\n\
\                  [ href \"#\"\n\
\                  , class \"block py-2 px-4 text-sm hover:bg-gray-100 dark:hover:bg-gray-600 dark:hover:text-white\"\n\
\                  ]\n\
\                  [ text \"Sign out\" ]\n\
\              ]\n\
\          ]\n\
\      ]\n\
\ ]\n"

demoHomeVerbatim :: Bs.ByteString
demoHomeVerbatim = "module Components.DemoHome exposing (default, staticPage)\n\
\\n\
\import Html.String as H exposing (..)\n\
\import Html.String.Attributes as Ha exposing (..)\n\
\import Sup.Svg as S\n\
\import Sup.Attributes as Sa\n\
\\n\
\import Html.Htmx as Ht\n\
\\n\
\import Html.StaticRender as Sr\n\
\\n\
\import FunctionRouter exposing (RenderOptions, DynInvokeFct, InvokeResult (..))\n\
\\n\
\\n\
\default : Sr.RenderOptions -> List (Html msg) -> List (Html msg)\n\
\default rtOpts children = [fakeClients]\n\
\\n\
\staticPage : DynInvokeFct msgT\n\
\staticPage rtOpts jsonParams = Forward [ fakeClients ]\n\
\\n\
\fakeClients : Html msg\n\
\fakeClients =\n\
\  div [ class \"p-8\" ] [\n\
\    div [class \"bg-gray-50 dark:bg-gray-900 p-4 md:ml-64 lg:mr-16 min-h-full pt-20\"] [\n\
\      div [ class \"text-red-500\"] [\n\
\        text \"ALLO!!\"\n\
\      ]\n\
\    ]\n\
\  ]\n\
\\n"

iconsVerbatim :: Bs.ByteString
iconsVerbatim = "module Components.Icons exposing (..)\n\
\\n\
\import Html.String exposing (..)\n\
\import Html.String.Attributes exposing (..)\n\
\import Sup.Svg as S\n\
\import Sup.Attributes as Sa\n\
\import Html.Extra as Hx\n\
\\n\
\bellIcon =\n\
\  S.svg\n\
\    [ attribute \"aria-hidden\" \"true\"\n\
\    , Sa.class \"w-6 h-6\"\n\
\    , Sa.fill \"currentColor\"\n\
\    , Sa.viewBox \"0 0 20 20\"\n\
\    ]\n\
\    [ S.path\n\
\        [ Sa.d \"M10 2a6 6 0 00-6 6v3.586l-.707.707A1 1 0 004 14h12a1 1 0 00.707-1.707L16 11.586V8a6 6 0 00-6-6zM10 18a3 3 0 01-3-3h6a3 3 0 01-3 3z\"\n\
\        ]\n\
\        [] \n\
\    ]\n\
\\n\
\\n\
\newIcon =\n\
\  div\n\
\    [ class \"flex absolute justify-center items-center ml-6 -mt-5 w-5 h-5 rounded-full border border-white bg-primary-700 dark:border-gray-700\"\n\
\    ]\n\
\    [ S.svg\n\
\        [ Hx.ariaHidden \"true\", Sa.class \"w-3 h-3 text-white\", Sa.fill \"currentColor\", Sa.viewBox \"0 0 20 20\" ]\n\
\            [ S.path\n\
\                [ Sa.d \"M8.707 7.293a1 1 0 00-1.414 1.414l2 2a1 1 0 001.414 0l2-2a1 1 0 00-1.414-1.414L11 7.586V3a1 1 0 10-2 0v4.586l-.293-.293z\"\n\
\                ]\n\
\                []\n\
\            , S.path\n\
\                [ Sa.d \"M3 5a2 2 0 012-2h1a1 1 0 010 2H5v7h2l1 2h4l1-2h2V5h-1a1 1 0 110-2h1a2 2 0 012 2v10a2 2 0 01-2 2H5a2 2 0 01-2-2V5z\"\n\
\                ]\n\
\                []\n\
\            ]\n\
\    ]\n\
\\n\
\pplPlus =\n\
\  div\n\
\    [ class \"flex absolute justify-center items-center ml-6 -mt-5 w-5 h-5 bg-gray-900 rounded-full border border-white dark:border-gray-700\"\n\
\    ]\n\
\    [ S.svg\n\
\        [ Hx.ariaHidden \"true\", Sa.class \"w-3 h-3 text-white\", Sa.fill \"currentColor\", Sa.viewBox \"0 0 20 20\" ]\n\
\        [ S.path\n\
\            [ Sa.d \"M8 9a3 3 0 100-6 3 3 0 000 6zM8 11a6 6 0 016 6H2a6 6 0 016-6zM16 7a1 1 0 10-2 0v1h-1a1 1 0 100 2h1v1a1 1 0 102 0v-1h1a1 1 0 100-2h-1V7z\"\n\
\            ]\n\
\            []\n\
\        ]\n\
\    ]\n\
\\n\
\redHeart =\n\
\  div\n\
\    [ class \"flex absolute justify-center items-center ml-6 -mt-5 w-5 h-5 bg-red-600 rounded-full border border-white dark:border-gray-700\"\n\
\    ]\n\
\    [ S.svg\n\
\        [ Hx.ariaHidden \"true\", Sa.class \"w-3 h-3 text-white\", Sa.fill \"currentColor\", Sa.viewBox \"0 0 20 20\" ]\n\
\        [ S.path\n\
\            [ Sa.fillRule \"evenodd\", Sa.clipRule \"evenodd\"\n\
\            , Sa.d \"M3.172 5.172a4 4 0 015.656 0L10 6.343l1.172-1.171a4 4 0 115.656 5.656L10 17.657l-6.828-6.829a4 4 0 010-5.656z\"\n\
\            ]\n\
\            []\n\
\        ]\n\
\    ]\n\
\\n\
\greenMessage =\n\
\  div\n\
\    [ class \"flex absolute justify-center items-center ml-6 -mt-5 w-5 h-5 bg-green-400 rounded-full border border-white dark:border-gray-700\"\n\
\    ]\n\
\    [ S.svg\n\
\        [ Hx.ariaHidden \"true\", Sa.class \"w-3 h-3 text-white\", Sa.fill \"currentColor\", Sa.viewBox \"0 0 20 20\" ]\n\
\        [ S.path\n\
\            [ Sa.fillRule \"evenodd\", Sa.clipRule \"evenodd\"\n\
\            , Sa.d \"M18 13V5a2 2 0 00-2-2H4a2 2 0 00-2 2v8a2 2 0 002 2h3l3 3 3-3h3a2 2 0 002-2zM5 7a1 1 0 011-1h8a1 1 0 110 2H6a1 1 0 01-1-1zm1 3a1 1 0 100 2h3a1 1 0 100-2H6z\"\n\
\            ]\n\
\            []\n\
\        ]\n\
\    ]\n\
\\n\
\purpleCamera =\n\
\  div\n\
\    [ class \"flex absolute justify-center items-center ml-6 -mt-5 w-5 h-5 bg-purple-500 rounded-full border border-white dark:border-gray-700\"\n\
\    ]\n\
\    [ S.svg\n\
\        [ Hx.ariaHidden \"true\", Sa.class \"w-3 h-3 text-white\", Sa.fill \"currentColor\", Sa.viewBox \"0 0 20 20\" ]\n\
\        [ S.path\n\
\            [ Sa.d \"M2 6a2 2 0 012-2h6a2 2 0 012 2v8a2 2 0 01-2 2H4a2 2 0 01-2-2V6zM14.553 7.106A1 1 0 0014 8v4a1 1 0 00.553.894l2 1A1 1 0 0018 13V7a1 1 0 00-1.447-.894l-2 1z\"\n\
\            ]\n\
\            []\n\
\        ]\n\
\    ]\n\
\\n"

-- TODO: use the WAPP_LIB env variable to set the source-directories prefixes.
elmJsonVerbatim :: Bs.ByteString
elmJsonVerbatim = 
  let
    libPrefix = "/Volumes/ledna/Projets/Fudd/EasyWordy/"
  in
    "{\n\
\  \"type\": \"application\",\n\
\  \"source-directories\": [\"" <> libPrefix <> "Wapp/Lib/lib\", \"" <> libPrefix <> "Lib/elm-html-string/src\", \"wapp\"],\n\
\  \"elm-version\": \"0.19.1\",\n\
\  \"dependencies\": {\n\
\    \"direct\": {\n\
\      \"NoRedInk/elm-string-conversions\": \"1.0.1\",\n\
\      \"elm/browser\": \"1.0.2\",\n\
\      \"elm/core\": \"1.0.5\",\n\
\      \"elm/html\": \"1.0.0\",\n\
\      \"elm/json\": \"1.1.3\",\n\
\      \"elm/svg\": \"1.0.1\",\n\
\      \"elm/time\": \"1.0.0\"\n\
\    },\n\
\    \"indirect\": {\n\
\      \"elm/bytes\": \"1.0.8\",\n\
\      \"elm/file\": \"1.0.5\",\n\
\      \"elm/http\": \"2.0.0\",\n\
\      \"elm/url\": \"1.0.0\",\n\
\      \"elm/virtual-dom\": \"1.0.3\"\n\
\    }\n\
\  },\n\
\  \"test-dependencies\": {\n\
\    \"direct\": {},\n\
\    \"indirect\": {}\n\
\  }\n\
\}\n"

