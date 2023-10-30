module Table where

import qualified Cell
import qualified Row
import Data.List (intercalate, find)
import Data.Maybe (catMaybes)

data Value = Value Row.Value Row.Value Row.Value

empty :: Value
empty = Value Row.empty Row.empty Row.empty

instance Show Value where
    show (Value x0 x1 x2) = intercalate "\n" [Row.display 0 x0, Row.display 1 x1, Row.display 2 x2]

take :: Cell.Taken -> Int -> Value -> Maybe Value
take taken idx (Value x0 x1 x2) =
    let row = Row.take taken $ idx `mod` 3
    in case idx `div` 3 of
        0 -> (\x -> Value x x1 x2) <$> row x0
        1 -> (\x -> Value x0 x x2) <$> row x1
        _ -> (\x -> Value x0 x1 x) <$> row x2

-- rowByIdx :: Int -> Value -> Row.Value
-- rowByIdx idx (Value x0 x1 x2) = case idx of
--     0 -> Eq

-- cellByIdx :: Int -> Cell.Value
-- cellByIdx idx =
--     let rowIdx = idx `mod` 3
--     in case Rowidx `div` 3 of
--         0 -> Row.item row 
--         1 -> (\x -> Value x0 x x2) <$> row x1
--         _ -> (\x -> Value x0 x1 x) <$> row x2

check :: Value -> [Cell.Taken]
check (Value (Row.Value x00 x01 x02) (Row.Value x10 x11 x12) (Row.Value x20 x21 x22)) = do
    catMaybes
        [ Cell.check3 x00 x01 x02
        , Cell.check3 x10 x11 x12
        , Cell.check3 x20 x21 x22
        , Cell.check3 x00 x10 x20
        , Cell.check3 x01 x11 x21
        , Cell.check3 x02 x12 x22
        , Cell.check3 x00 x11 x22
        , Cell.check3 x02 x11 x20
        ]
