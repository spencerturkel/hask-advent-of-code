module Main where

import Data.Maybe (mapMaybe)
import Data.List.NonEmpty (NonEmpty((:|)))

import FindRoot (findRootName)
import Parser (parseInput)
import Paths_Day7 (getDataFileName)

main :: IO ()
main = do
    putStrLn "Running..."
    print =<< (run <$> (readFile =<< getDataFileName "input.txt"))

run :: String -> String
run =
    findRootName .
    (\(x:xs) -> x :| xs) .
    mapMaybe parseInput .
    lines