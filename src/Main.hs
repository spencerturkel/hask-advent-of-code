module Main where

import qualified Data.Stream as S

main :: IO ()
main = do
  putStrLn "Calculating..."
  let point = (spiral :: S.Stream (Point Int)) S.!! (289326 - 1)
  putStrLn $ "The point is " ++ show point
  let distanceFromOrigin = abs (x point) + abs (y point)
  putStrLn $ "The distance from the origin is " ++ show distanceFromOrigin

data Point a = Point { x :: a, y :: a } deriving (Show)

spiral :: (Ord a, Num a) => S.Stream (Point a)
spiral = S.iterate nextSpiralStep (Point 0 0)

nextSpiralStep :: (Ord a, Num a) => Point a -> Point a
nextSpiralStep (Point{x, y})
  | x == 0 && y == 0 = Point {x = 1, y = 0}
  | x > 0 && x == y = Point {x = x - 1, y}
  | x < 0 && negate x == y = Point {x, y = y - 1}
  | x < 0 && x == y = Point {x = x + 1, y}
  | x > 0 && negate x == y = Point {x = x + 1, y}
  | x > 0 && y < x && y > negate x = Point {x, y = y + 1}
  | x < 0 && y > x && y < negate x = Point {x, y = y - 1}
  | y > 0 && x < y && x > negate y = Point {x = x - 1, y}
  | y < 0 && x > y && x < negate y = Point {x = x + 1, y}
  | otherwise = undefined
