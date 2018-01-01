module Main where

import Data.List.NonEmpty (NonEmpty((:|)))

import FindRoot (findRootName)
import Parser (parseLinesNonEmpty)
import Paths_Day7 (getDataFileName)

main :: IO ()
main = do
    putStrLn "Running..."
    print =<< (run <$> (readFile =<< getDataFileName "input.txt"))

run :: String -> Maybe String
run =
    findRootName .
    parseLinesNonEmpty