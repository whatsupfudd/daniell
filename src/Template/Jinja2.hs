module Template.Jinja2 where

import qualified Data.Text as DT
import qualified Data.Text.IO as DT

import Control.Monad.Identity (Identity, runIdentity)
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import qualified Text.Ginger as Gngr

import qualified System.IO as Sio
import qualified System.IO.Error as Serr


demoContext :: HashMap DT.Text DT.Text
demoContext = HashMap.fromList [
    ("name", "Alice")
  , ("location", "Wonderland")
  ]

nullResolver :: Gngr.IncludeResolver Identity
nullResolver = const $ return Nothing

demoTemplate :: Gngr.Template Gngr.SourcePos
demoTemplate =
  either (error . show) id . runIdentity $
      Gngr.parseGinger nullResolver Nothing "Hello, {{ name }}, welcome in {{ location }}!"


parse rtOpts filePath = do
  let
    output = Gngr.easyRender demoContext demoTemplate
  DT.putStrLn $ "@[parse] rez: " <> output

--  Gngr.runGingerT (Gngr.makeContextHtmlM scopeLookup (putStr . DT.unpack . htmlSource)) tpl

loadFile fn = Sio.openFile fn Sio.ReadMode >>= Sio.hGetContents

loadFileMay fn =
  Serr.tryIOError (loadFile fn) >>= \e ->
    case e of
      Right contents ->
        return (Just contents)
      Left err -> do
        print err -- remove this line if you want to fail silently
        return Nothing
