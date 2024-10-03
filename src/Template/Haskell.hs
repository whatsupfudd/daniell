module Template.Haskell where

import TreeSitter.Parser ( ts_parser_new, ts_parser_parse_string, ts_parser_set_language, Parser )
import TreeSitter.Tree ( ts_tree_root_node_p )
import TreeSitter.Haskell ( tree_sitter_haskell )

import Conclusion (GenError (..))
import Template.Types ( FileTempl )
import Template.TSitterParser ( tsParseFile )


tsParseHaskell :: FilePath -> IO (Either GenError FileTempl)
tsParseHaskell path = do
  putStrLn $ "@[tsParseHaskell] parsing: " ++ path

  parser <- ts_parser_new
  ts_parser_set_language parser tree_sitter_haskell
  tsParseFile parser path
