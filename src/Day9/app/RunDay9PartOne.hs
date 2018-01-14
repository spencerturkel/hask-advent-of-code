module Main where

import Control.Applicative (Alternative((<|>)), Applicative(pure))
import Text.ParserCombinators.ReadP
  ( ReadP
  , char
  , get
  , manyTill
  , readP_to_S
  , sepBy
  )

main :: IO ()
main = do
  putStrLn "Hello world"

solve :: String -> Int
solve =
  (\xs ->
     case xs of
       [] -> 0
       ((n, _):_) -> n) .
  readP_to_S (pGroup 1)

pGroup :: Int -> ReadP Int
pGroup n =
  char '{' *>
  ((+ n) . sum <$> ((pGroup (n + 1) <|> (0 <$ pGarbage)) `sepBy` char ',') <|> pure n) <*
  char '}'

pGarbage :: ReadP ()
pGarbage = char '<' *> get `manyTill` char '>' *> pure ()
