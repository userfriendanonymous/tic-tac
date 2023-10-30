module Cell where

type Value = Maybe Taken

data Taken
    = X
    | O
    deriving (Eq)

instance Show Taken where
    show X = "X"
    show O = "O"

display :: Int -> Value -> String
display _ (Just taken) = " " ++ show taken ++ " "
display idx Nothing = "[" ++ show idx ++ "]"

next :: Taken -> Taken
next X = O
next O = X

check3 :: Value -> Value -> Value -> Value
check3 (Just x0) (Just x1) (Just x2) = if x0 == x1 && x1 == x2 then Just x0 else Nothing
check3 _ _ _ = Nothing
