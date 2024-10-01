{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StandaloneDeriving #-}

module Template.PHP.Error where


import Control.Applicative
import Control.Monad
import Control.Monad.Cont.Class
import Control.Monad.Error.Class
import qualified Control.Monad.Fail as Fail
import Control.Monad.Fix
import Control.Monad.IO.Class
import Control.Monad.Reader.Class
import Control.Monad.State.Class
import Control.Monad.Trans
import Control.Monad.Writer.Class


import Data.Monoid
import Data.Semigroup
import Control.DeepSeq (NFData (rnf))

import Data.Data (Data)
import Data.Typeable (Typeable)
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE
import Data.Set (Set)
import qualified Data.Set as E

import GHC.Generics

import Template.PHP.Types
import Data.Void (Void, absurd)
import Data.Maybe (isNothing)
import Data.List


data ErrorItem t =
     -- | Non-empty stream of tokens
    Tokens (NonEmpty t)
    -- | Label (cannot be empty)
  | Label (NonEmpty Char)
    -- | End of input
  | EndOfInput
  deriving (Show, Read, Eq, Ord, Data, Typeable, Generic, Functor)

instance (NFData t) => NFData (ErrorItem t)


data ErrorFancy errT = 
    -- | 'fail' has been used in parser monad
    ErrorFail String
    -- | Incorrect indentation error: desired ordering between reference
    -- level and actual level, reference indentation level, actual
    -- indentation level
  | ErrorIndentation Ordering Int Int
    -- | Custom error data
  | ErrorCustom errT
  deriving (Show, Read, Eq, Ord, Data, Typeable, Generic, Functor)


instance (NFData errT) => NFData (ErrorFancy errT) where
  rnf (ErrorFail str) = rnf str
  rnf (ErrorIndentation ord ref act) = ord `seq` rnf ref `seq` rnf act
  rnf (ErrorCustom a) = rnf a

data ScanError errT = 
  TrivialError {
    position :: Int
    , message :: String
    , mbErrors :: Maybe (ErrorItem NodeEntry)
    , symbols :: Set (ErrorItem NodeEntry)
    }
  | FancyError {
      position :: Int
      , errors :: Set (ErrorFancy errT)
    }
  deriving (Typeable, Generic, Ord)

deriving instance (Show e) => Show (ScanError e)
deriving instance (Data e, Ord e) => Eq (ScanError e)
deriving instance (Data e, Ord e) => Data (ScanError e)
instance (NFData e) => NFData (ScanError e)
instance (Ord e) => Semigroup (ScanError e) where
  (<>) = mergeError

instance (Ord e) => Monoid (ScanError e) where
  mempty = TrivialError 0 "" Nothing mempty
  mappend = (<>)

mergeError :: (Ord e) => ScanError e -> ScanError e -> ScanError e
mergeError e1 e2 =
  case e1.position `compare` e2.position of
    LT -> e2
    EQ -> case (e1, e2) of
      (TrivialError a1 a2 a3 a4, TrivialError b1 b2 b3 b4) ->
        TrivialError a1 (a2 <> " - " <> b2) (n a3 b3) (E.union a4 b4)
      (FancyError {}, TrivialError {}) -> e1
      (TrivialError {}, FancyError {}) -> e2
      (FancyError a1 a2, FancyError b1 b2) ->
        FancyError a1 (E.union a2 b2)
    GT -> e1
  where
  n Nothing Nothing = Nothing
  n (Just x) Nothing = Just x
  n Nothing (Just y) = Just y
  n (Just x) (Just y) = Just (max x y)

data ScanErrBundle errT = ScanErrBundle {
  errors :: NonEmpty (ScanError errT)
  , endPos :: PosState
  }
  deriving (Generic)

deriving instance (Show errT) => Show (ScanErrBundle errT)
deriving instance (Ord errT, Data errT, Eq errT) => Eq (ScanErrBundle errT)
deriving instance (Typeable errT) => Typeable (ScanErrBundle errT)
deriving instance (Ord errT, Data errT) => Data (ScanErrBundle errT)
instance NFData errT => NFData (ScanErrBundle errT)
data PosState = PosState {
  input :: [NodeEntry]
  , parsed :: Int
  }
  deriving (Show, Eq, Data, Typeable, Generic)
instance NFData PosState

showScanErrorBundle :: (ShowErrorComponent e) => ScanErrBundle e -> String
showScanErrorBundle scanErrBundle =
  let
    errors = scanErrBundle.errors
    endPos = scanErrBundle.endPos
  in
  unlines $ parseErrorPretty <$> NE.toList errors

class (Ord a) => ShowErrorComponent a where
  -- | Pretty-print a component of 'ParseError'.
  showErrorComponent :: a -> String

  -- | Length of the error component in characters, used for highlighting of
  -- parse errors in input string.
  --
  -- @since 7.0.0
  errorComponentLen :: a -> Int
  errorComponentLen _ = 1

instance ShowErrorComponent Void where
  showErrorComponent = absurd

parseErrorPretty :: (ShowErrorComponent e) =>
  -- | Parse error to render
  ScanError e ->
  -- | Result of rendering
  String
parseErrorPretty err =
  "offset=" <> show err.position <> ":\n" <> parseErrorTextPretty err

parseErrorTextPretty :: forall e. ( ShowErrorComponent e) =>
  -- | Parse error to render
  ScanError e ->
  -- | Result of rendering
  String
parseErrorTextPretty (TrivialError _ msg mbErrors ps) =
  if isNothing mbErrors && E.null ps
    then "unknown parse error.\n"
    else
      messageItemsPretty "unexpected " (showErrorItem `E.map` maybe E.empty E.singleton mbErrors)
        <> messageItemsPretty "expecting " (showErrorItem `E.map` ps)

parseErrorTextPretty (FancyError _ xs) =
  if E.null xs
    then "unknown fancy parse error.\n"
    else unlines (showErrorFancy <$> E.toAscList xs)


showErrorItem :: ErrorItem NodeEntry -> String
showErrorItem = \case
  Tokens ts -> intercalate ", " (NE.toList $ (.name) <$> ts)
  Label label -> NE.toList label
  EndOfInput -> "end of input"


showErrorFancy :: (ShowErrorComponent e) => ErrorFancy e -> String
showErrorFancy = \case
  ErrorFail msg -> msg
  ErrorIndentation ord ref actual ->
    "incorrect indentation (got "
      <> show actual
      <> ", should be "
      <> p
      <> show ref
      <> ")"
    where
      p = case ord of
        LT -> "less than "
        EQ -> "equal to "
        GT -> "greater than "
  ErrorCustom a -> showErrorComponent a


-- | Transform a list of error messages into their textual representation.
messageItemsPretty :: String -> Set String -> String
  -- | String prefix: Prefix to prepend
  -- | Set String ts: Collection of messages
  -- | String: Result of rendering
messageItemsPretty prefix messages
  | E.null messages = ""
  | otherwise =
      prefix <> (orList . NE.fromList . E.toAscList) messages <> "\n"

-- | Print a pretty list where items are separated with commas and the word
-- “or” according to the rules of English punctuation.
orList :: NonEmpty String -> String
orList (x :| []) = x
orList (x :| [y]) = x <> " or " <> y
orList xs = intercalate ", " (NE.init xs) <> ", or " <> NE.last xs