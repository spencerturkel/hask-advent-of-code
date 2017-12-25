module Main where

import qualified Data.Stream as S

main :: IO ()
main = do
  putStrLn "hello world"
  print . S.take 10 $ (S.repeat 2 :: S.Stream Int)
