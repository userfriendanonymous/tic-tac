module Row where

import qualified Cell as Item
import Data.List (intercalate)
import Data.Maybe (isJust)

data Value = Value Item.Value Item.Value Item.Value

empty :: Value
empty = Value Nothing Nothing Nothing

display :: Int -> Value -> [Char]
display idx (Value x0 x1 x2) = "|"
    ++ intercalate "|" [item 0 x0, item 1 x1, item 2 x2]
    ++ "|"
    where
        item itemIdx value = Item.display ((idx * 3) + itemIdx) value

item :: Int -> Value -> Item.Value
item idx (Value x0 x1 x2) = case idx of
    0 -> x0
    1 -> x1
    _ -> x2

take :: Item.Taken -> Int -> Value -> Maybe Value
take taken idx (Value x0 x1 x2) = 
    let x = Just taken
    in if isJust (item idx (Value x0 x1 x2)) then
        Nothing
    else Just $ case idx of
        0 -> Value x x1 x2 
        1 -> Value x0 x x2
        _ -> Value x0 x1 x

check :: Value -> Item.Value
check (Value x0 x1 x2) = Item.check3 x0 x1 x2