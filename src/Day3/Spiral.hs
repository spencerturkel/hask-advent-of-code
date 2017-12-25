module Spiral (spiral) where

import Data.Stream (Stream, iterate)
import Prelude hiding (iterate)

import Point (Point(Point, x, y))

spiral :: (Ord a, Num a) => Stream (Point a)
spiral = iterate nextSpiralStep (Point 0 0)

nextSpiralStep :: (Ord a, Num a) => Point a -> Point a
nextSpiralStep (Point {x, y})
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