module Main (main) where

import qualified Table
import qualified Cell
import Control.Monad (when)
import Text.Read (readMaybe)

main :: IO ()
main = run

run :: IO ()
run = do
    let table = Table.empty
    game table
    putStrLn "Play again? (y/n)"
    again <- getLine
    if again == "y" then run else do
        putStrLn "Game stopped."

game :: Table.Value -> IO ()
game table = do
    table' <- prompt Cell.X table
    continue <- check table'
    when continue $ do
        table'' <- prompt Cell.O table'
        continue' <- check table''
        when continue' $ game table''

check :: Table.Value -> IO Bool
check table = do
    let
        wins = Table.check table
    if null wins then
        if Table.isFull table then do
            putStrLn "Draw!"
            pure False
        else do
            putStrLn "Okay!"
            pure True
    else do
        putStrLn ""
        print table
        putStrLn $ show (head wins) ++ " won the game!\n"
        pure False

prompt :: Cell.Taken -> Table.Value -> IO Table.Value
prompt cell table = do
    putStrLn ""
    putStrLn $ show cell ++ " player, your order now!"
    print table
    putStrLn "Enter a number where you want to put: "
    askIdx cell table

askIdx :: Cell.Taken -> Table.Value -> IO Table.Value
askIdx cell table = do
    str <- getLine
    let idxResult = readMaybe str
    case idxResult of
        Just idx -> case Table.take cell idx table of
            Just table' -> pure table'
            Nothing -> do
                putStrLn "That index is already taken! Enter another one:"
                askIdx cell table
        Nothing -> do
            putStrLn "Invalid index! Enter a valid one:"
            askIdx cell table
