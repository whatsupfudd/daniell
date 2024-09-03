module Options.Types where

import Data.Text (Text)
import Data.Vector.Generic.New (New)


data ProjectKind =
  SitePK
  | WebAppPK
  | LocalAppPK
  deriving Show


data NewOptions = NewOptions {
    projKind :: ProjectKind
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


data BuildOptions = BuildOptions {
    projKind :: ProjectKind
    , subKind :: Maybe Text
    , srcDir :: Maybe Text
  }
  deriving Show


data SubProjKind =
  HugoSP
  | NextSP
  | FuddleSP
  | GatsbySP
  deriving Show
