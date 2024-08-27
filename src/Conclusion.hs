module Conclusion where

import Data.Text (Text, unpack)


newtype GenError =
    SimpleMsg Text

instance Show GenError where
  show (SimpleMsg msg) = "Error: " <> unpack msg


data Conclusion = 
    NilCcl
    | ErrorCcl String
  deriving Show

conclude :: Conclusion -> IO ()
conclude c =
  case c of
    NilCcl -> pure ()
    _ -> putStrLn "@[conclude] ended."