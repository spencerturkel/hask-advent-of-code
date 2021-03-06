module Main where

import Data.List (maximum)
import Data.Map.Lazy (toList)
import Paths_Day8 (getDataFileName)

import Interpreter (runProgram)
import Parser (parseProgram)

main :: IO ()
main = do
    putStrLn "Running..."
    Just program <- parseProgram . drop 3 <$> (readFile =<< getDataFileName "input.txt")
    print . maximum . fmap snd . toList . last . runProgram $ program

