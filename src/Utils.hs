{-# LANGUAGE LambdaCase #-}
module Utils where

import qualified Data.Sequence as Seq


(>>=?) :: Monad monadT => monadT (Either errM aT) -> (aT -> monadT (Either errM bT)) -> monadT (Either errM bT)
eiRezFct >>=? f = eiRezFct >>= \case
    Left err -> pure $ Left err
    Right rez -> f rez


seqPartitionEithers :: Seq.Seq (Either a b) -> ([a], [b])
seqPartitionEithers =
  foldr (\anEither (leftsAccum, rightsAccum) -> case anEither of
      Left a -> (leftsAccum <> [a], rightsAccum)
      Right b -> (leftsAccum, rightsAccum <> [b])
    ) ([], [])

