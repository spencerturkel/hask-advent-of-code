module Day3 where

import Control.Comonad.Trans.Env (EnvT)
import Data.Functor.Foldable (gpara, para)
import Data.List (elemIndex)
import Data.Maybe (maybe)
import qualified Data.Stream as S (Stream, (!!), iterate)

import StreamRecursion()

runPartOne :: IO ()
runPartOne = do
  putStrLn "Calculating..."
  let point = (spiral :: S.Stream (Point Int)) S.!! (289326 - 1)
  point `seq` (putStrLn $ "The point is " ++ show point)
  let distanceFromOrigin = abs (x point) + abs (y point)
      message = "The distance from the origin is " ++ show distanceFromOrigin
  distanceFromOrigin `seq` (putStrLn message)

data Point a = Point
  { x :: a
  , y :: a
  } deriving (Eq, Show)

spiralSum :: forall a. (Num a) => S.Stream a
spiralSum = undefined
  where
    -- transform :: (a, (w b)) -> w (a, b)
    -- transform = _
    -- step :: (a, (EnvT (S.Stream a) w (S.Stream a))) -> S.Stream a
    -- step = _
    transform :: (a, (S.Stream a, b)) -> b
    transform (current, (soFar, next)) = _
    go :: S.Stream a -> S.Stream a
    -- go = gpara transform step
    go = para transform

spiralSumStep ::
     forall a. (Eq a, Num a)
  => Point a
  -> [Point a]
  -> a
spiralSumStep (Point {x, y}) ps =
  sum $ map index [(x + 1, y), (x - 1, y), (x, y + 1), (x, y - 1)]
  where
    index :: (a, a) -> a
    index (px, py) =
      maybe 0 fromIntegral $ elemIndex (Point {x = px, y = py}) ps

spiral :: (Ord a, Num a) => S.Stream (Point a)
spiral = S.iterate nextSpiralStep (Point 0 0)

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
