module Utils where

splitResults :: [Either a b] -> ([a], [b])
splitResults = foldl (\(lefts, rights) eiR -> either (\l -> (lefts <> [l], rights)) (\r -> (lefts, rights <> [r])) eiR) ([], [])

