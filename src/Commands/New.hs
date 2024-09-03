module Commands.New where

import Data.Text (Text, unpack)
import Data.List (singleton)

import qualified Conclusion as Ccl
import Options.Runtime (RunOptions (..))
import Options.Types (NewOptions (..))

import Generator.Logic (createProject)


newCmd :: NewOptions -> RunOptions -> IO Ccl.Conclusion
newCmd options rtOpts = do
  putStrLn $ "@[newCmd] Options: " <> show options
  createProject rtOpts options

