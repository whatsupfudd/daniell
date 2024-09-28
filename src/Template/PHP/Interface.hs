{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DeriveFunctor #-}
module Template.PHP.Interface where

import Data.Set (Set)
import qualified Data.Set as E


import Template.PHP.Types
import Template.PHP.Error
import Template.PHP.State


newtype Hints = Hints (Set (ErrorItem NodeEntry))

instance Semigroup Hints where
  Hints xs <> Hints ys = Hints $ xs <> ys

instance Monoid Hints where
  mempty = Hints mempty

toHints :: Int -> ScanError errT -> Hints
toHints scanPos = \case
  TrivialError pos _ mbErrs syms ->
    if scanPos == pos then
      Hints (if E.null syms then E.empty else syms)
    else
      mempty
  FancyError _ _ -> mempty


withHints ::
  -- | Hints to use
  Hints
  -- | Continuation to influence
  -> (ScanError errT -> ScanState errT -> m b)
  -- | First argument of resulting continuation
  -> ScanError errT
  -- | Second argument of resulting continuation
  -> ScanState errT
  -> m b
withHints (Hints errSet) contFct err =
  contFct (TrivialError err.position err.message err.mbErrors (E.union err.symbols errSet))

-- | @'accHints' hs c@ results in “OK” continuation that will add given
-- hints @hs@ to third argument of original continuation @c@.
accHints ::
     -- | 'Hints' to add
     Hints
     -- | An “OK” continuation to alter
  -> (a -> ScanState errT -> Hints -> m b)
     -- | Altered “OK” continuation
  -> (a -> ScanState errT -> Hints -> m b)
accHints hints1 contFct x s hints2 =
  contFct x s (hints1 <> hints2)

refreshHints :: Hints -> Maybe (ErrorItem NodeEntry) -> Hints
refreshHints (Hints _) Nothing = Hints E.empty
refreshHints (Hints hints) (Just eItem) =
  if E.null hints then
    Hints hints
  else
    Hints (E.singleton eItem)

-- | All information available after parsing. This includes consumption of
-- input, success (with the returned value) or failure (with the parse
-- error), and parser state at the end of parsing. 'Reply' can also be used
-- to resume parsing.
data Reply errT a = Reply (ScanState errT) Consumption (Result errT a)
  deriving (Functor)

data Consumption =
    -- | Some part of input stream was consumed
    Consumed
     -- | No input was consumed
  | NotConsumed


-- | Whether the parser has failed or not. On success we include the
-- resulting value, on failure we include a 'ParseError'.
--
-- See also: 'Consumption', 'Reply'.
data Result errT a =
  -- | Parser succeeded (includes hints)
    OK Hints a
  | Error (ScanError errT)
  deriving (Functor)
   -- | Parser failed

