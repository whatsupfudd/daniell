{-# LANGUAGE LambdaCase #-}
module Utils where

splitResults :: [Either a b] -> ([a], [b])
splitResults = foldl (\(lefts, rights) eiR -> either (\l -> (lefts <> [l], rights)) (\r -> (lefts, rights <> [r])) eiR) ([], [])


(>>=?) :: IO (Either errM aT) -> (aT -> IO (Either errM bT)) -> IO (Either errM bT)
eiRezFct >>=? f = eiRezFct >>= \case
    Left err -> pure $ Left err
    Right rez -> f rez

