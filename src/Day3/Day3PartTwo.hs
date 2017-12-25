module Day3PartTwo where

import Control.Comonad.Trans.Env (EnvT)
import Data.Functor.Foldable (gpara, para)
import Data.List (elemIndex)
import Data.Maybe (maybe)
import qualified Data.Stream as S (Stream, (!!), iterate)

import Spiral (spiral)
import Point (Point(Point, x, y))
import StreamRecursion ()

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