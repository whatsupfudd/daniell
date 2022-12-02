import qualified Data.Char as Chr
:{
genModuleText label =
  let
    fctLabel = ((Chr.toLower . head $ label) : tail label) <> "Hu"
    moduleText = "module Commands." <> label <> " where\n\n\
      \import qualified Conclusion as Ccl\n\
      \import qualified Options.RunOptions as Rto\n\n\
      \" <> fctLabel <> " :: Rto.RunOptions -> IO Ccl.Conclusion\n\
      \" <> fctLabel <> " rtOpts =\n\
      \  putStrLn \"@[" <> fctLabel <> "] starting.\" >> pure Ccl.NilCcl\n"
  in
  writeFile (label <> ".hs") moduleText
:}

test = mapM genModuleText [ "Config", "Convert", "Deploy", "Env", "Gen", "Help", "Import", "List", "Mod", "New", "Server", "Version", "Publish" ]