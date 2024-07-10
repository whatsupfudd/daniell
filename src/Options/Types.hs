module Options.Types where

import Data.Text (Text)

data NewProjectKind =
  SitePK
  | WebAppPK
  | LocalAppPK
  deriving Show

data NewOptions = NewOptions {
    projKind :: NewProjectKind
    , rootDir :: FilePath
    , templates :: [ FilePath ]
    , compatMode :: Maybe Text
    , params :: [ ParameterTpl ]
  }
  deriving Show

data ParameterTpl =
  AssignmentP (Text, Text)
  | FlagP Text
  deriving Show

