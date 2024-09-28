{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE FunctionalDependencies #-}

module Template.PHP.State where

import Data.Monoid
import Data.Semigroup
import Control.DeepSeq (NFData (rnf))


import Data.Typeable (Typeable)
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE
import Data.Set (Set)
import qualified Data.Set as E

import GHC.Generics

import Data.Data
import Data.Vector (Vector)
import qualified Data.Vector as V

import Template.PHP.Types
import Template.PHP.Error


data ScanState errT = ScanState {
    inputs :: [ NodeEntry ]
    , parsed :: Int
    , errors :: [ ScanError errT ]
    , contextDemands :: V.Vector SegmentPos
  }
  deriving (Typeable, Generic)

deriving instance (Ord errT, Show (ScanError errT)) => Show (ScanState errT)
deriving instance (Ord errT, Eq (ScanError errT)) => Eq (ScanState errT)
deriving instance (Ord errT, Data errT, Data (ScanError errT)) => Data (ScanState errT)
instance (NFData (ScanError errT)) => NFData (ScanState errT)


initState :: (Ord errT) => [NodeEntry] -> ScanState errT
initState inputs = ScanState {
    inputs = inputs
    , parsed = 0
    , errors = []
    , contextDemands = V.empty
  }

