module Markup.Markdown where


import qualified Data.Text.IO as DT
import qualified Data.Text.Lazy.IO as TL
import qualified Text.MMark as Mmkp
import qualified Text.Megaparsec as Prsc
import qualified Lucid as Lcd
import qualified System.FilePath as Sfp


parse :: FilePath -> IO ()
parse filePath = do
  let
    fileName = Sfp.takeBaseName filePath
    outPath = Sfp.addExtension (Sfp.joinPath ["/tmp", fileName]) "html"
  txt <- DT.readFile filePath
  case Mmkp.parse filePath txt of
      Left bundle -> putStrLn (Prsc.errorBundlePretty bundle)
      Right r -> TL.writeFile outPath . Lcd.renderText . Mmkp.render $ r