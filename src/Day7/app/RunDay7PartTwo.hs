module Main where

import FindBalancingWeight (findBalancingWeight)
import Parser (parseLines)
import Paths_Day7 (getDataFileName)

main :: IO ()
main = do
  putStrLn "Running test input..."
  putStrLn =<< (run <$> (readFile =<< getDataFileName "test.txt"))
  putStrLn "Press enter to continue..."
  _ <- getLine
  putStrLn "Running real input..."
  putStrLn =<< (run <$> (readFile =<< getDataFileName "input.txt"))

run :: String -> String
run = show . findBalancingWeight . parseLines
