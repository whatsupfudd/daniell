module Conclusion where

import Data.Text (Text)


data GenError =
  SimpleMsg Text

data Conclusion = 
  NilCcl
  | ErrorCcl String

conclude :: Conclusion -> IO ()
conclude c =
  case c of
    NilCcl -> pure ()
    _ -> putStrLn "@[conclude] ended."