{-# LANGUAGE LambdaCase #-}
module Utils where

splitResults :: [Either a b] -> ([a], [b])
splitResults = foldl (\(lefts, rights) eiR -> either (\l -> (lefts <> [l], rights)) (\r -> (lefts, rights <> [r])) eiR) ([], [])


(>>=?) :: Monad monadT => monadT (Either errM aT) -> (aT -> monadT (Either errM bT)) -> monadT (Either errM bT)
eiRezFct >>=? f = eiRezFct >>= \case
    Left err -> pure $ Left err
    Right rez -> f rez

