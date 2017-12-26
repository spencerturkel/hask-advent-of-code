module Day3PartTwo where

import Control.Comonad.Trans.Env (EnvT)
import Data.Functor.Foldable (ana)
import Data.List (elemIndex)
import Data.Maybe (fromMaybe)
import Data.Monoid (Sum(Sum, getSum))
import Data.Stream (Stream, filter, head, repeat, zip)
import Prelude hiding (Left, Right, (!!), filter, head, repeat, zip)

import Point (Point(Point, x, y))
import qualified Spiral (spiral)
import StreamRecursion ()

runDayThreePartTwo :: IO ()
runDayThreePartTwo = do
  putStrLn "Calculating..."
  let spiral = spiralSum (Spiral.spiral :: Stream (Point Int))
      result = find (\x -> x > 289326) . fmap getSum $ spiral
      message = "The result is " ++ show result
  message `seq` putStrLn message

spiralSum ::
     forall a m. (Eq a, Num a, Monoid m)
  => Stream (Point a)
  -> Stream m
spiralSum =
  let getNext :: Stream (Point a, m) -> Point a -> m
      getNext xs = sumAtPoint (fmap (findPointValue xs) . offsetPoint)

  in fmap snd . go getNext (Point 0 0) . flip zip (repeat (mempty :: m))
  where
    go :: (Stream (Point a, m) -> Point a -> m) -> Point a -> Stream (Point a, m) -> Stream (Point a, m)
    go next p xs = _
    sumAtPoint :: (Direction -> Point a -> m) -> Point a -> m
    sumAtPoint adjacentSum =
      mconcat . fmap adjacentSum $ [Left, Right, Up, Down]
    offsetPoint :: Direction -> Point a -> Point a
    offsetPoint Left Point {x, y} = Point {x = x - 1, y}
    offsetPoint Right Point {x, y} = Point {x = x + 1, y}
    offsetPoint Up Point {x, y} = Point {x, y = y + 1}
    offsetPoint Down Point {x, y} = Point {x, y = y - 1}
    findPointValue :: Stream (Point a, b) -> Point a -> b
    findPointValue ps p = snd $ find (\(q, _) -> q == p) ps

find :: (a -> Bool) -> Stream a -> a
find p = head . filter p

data Direction
  = Left
  | Right
  | Up
  | Down
