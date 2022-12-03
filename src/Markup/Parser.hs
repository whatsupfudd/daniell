module Markup.Parser where


import qualified Data.Text.IO as T
import qualified Data.Text.Lazy.IO as TL
import qualified Text.MMark as Mmkp
import qualified Text.Megaparsec as M
import qualified Lucid as L
import qualified System.FilePath as Sfp


parseMarkdown :: FilePath -> IO ()
parseMarkdown filePath = do
  let
    fileName = Sfp.takeBaseName filePath
    outPath = Sfp.addExtension (Sfp.joinPath ["/tmp", fileName]) "html"
  txt <- T.readFile filePath
  case Mmkp.parse filePath txt of -- (2)
      Left bundle -> putStrLn (M.errorBundlePretty bundle) -- (3)
      Right r -> TL.writeFile outPath -- (6)
        . L.renderText -- (5)
        . Mmkp.render -- (4)
        $ r