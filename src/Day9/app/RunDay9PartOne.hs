module Main where

import Control.Applicative (Alternative((<|>)), Applicative(pure))
import Debug.Trace
import Text.ParserCombinators.ReadP
  ( ReadP
  , char
  , get
  , readP_to_S
  , sepBy
  )

import Paths_Day9 (getDataFileName)

main :: IO ()
main = do
  putStrLn "Running..."
  input <- readFile =<< getDataFileName "input.txt"
  putStrLn "Read input, solving..."
  print . solve $ input

solve :: String -> Int
solve =
  (\xs ->
     case xs of
       [] -> 0
       ((n, _):_) -> n) .
  readP_to_S (pGroup 1)

pGroup :: Int -> ReadP Int
pGroup n
  | flip trace False $ "pGroup " ++ show n = undefined
  | otherwise =
    char '{' *>
    ((+ n) . sum <$> ((pGroup (n + 1) <|> (0 <$ pGarbage)) `sepBy` char ',') <|>
    pure n) <*
    char '}'

pGarbage :: ReadP ()
pGarbage = char '<' *> pInnerGarbage

pInnerGarbage :: ReadP ()
pInnerGarbage = do
  c <- get
  case c of
    '!' -> get *> pInnerGarbage
    '>' -> pure ()
    _ -> pInnerGarbage
