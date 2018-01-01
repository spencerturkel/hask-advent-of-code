module Main where

import FindRoot (findRootName)
import Parser (parseLines)
import Paths_Day7 (getDataFileName)

main :: IO ()
main = do
  putStrLn "Running..."
  print =<< (run <$> (readFile =<< getDataFileName "input.txt"))

run :: String -> Maybe String
run = findRootName . parseLines
