module Conclusion where

import Data.Text (Text)


newtype GenError =
    SimpleMsg Text
  deriving Show


data Conclusion = 
    NilCcl
    | ErrorCcl String
  deriving Show

conclude :: Conclusion -> IO ()
conclude c =
  case c of
    NilCcl -> pure ()
    _ -> putStrLn "@[conclude] ended."