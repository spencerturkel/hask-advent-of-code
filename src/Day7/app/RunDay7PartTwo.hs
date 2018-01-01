module Main where

import Data.List.NonEmpty (NonEmpty((:|)))

import FindBalancingWeight (findBalancingWeight)
import Parser (parseLinesNonEmpty)
import Paths_Day7 (getDataFileName)
import ProgramInfo (ProgramInfo)

main :: IO ()
main = do
  putStrLn "Running test input..."
  putStrLn =<< (run <$> (readFile =<< getDataFileName "test.txt"))
  putStrLn "Press enter to continue..."
  _ <- getLine
  putStrLn "Running real input..."
  putStrLn =<< (run <$> (readFile =<< getDataFileName "input.txt"))

run :: String -> String
run = show . fmap findBalancingWeight . parseLinesNonEmpty