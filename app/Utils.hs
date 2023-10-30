module Utils where

mapWithIndex :: (Int -> a -> b) -> [a] -> [b]
mapWithIndex = mwi 0
    where
        mwi idx f (x : xs) = f idx x : mwi (idx + 1) f xs
        mwi _ _ [] = []