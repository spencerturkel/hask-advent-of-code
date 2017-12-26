module Day3PartTwo where

import Data.Foldable (find)
import qualified Data.Map.Lazy as Map (Map, fromList, lookup)
import Data.Monoid (Sum(getSum))
import Data.Stream (Stream, filter, fromList, head, repeat, toList, zip)
import Prelude hiding (Left, Right, (!!), filter, head, repeat, zip)

import Point (Point(Point, x, y))
import qualified Spiral (spiral)

runDayThreePartTwo :: IO ()
runDayThreePartTwo = do
  putStrLn "Calculating..."
  let spiral = spiralSum (Spiral.spiral :: Stream (Point Int))
      result = findS (\x -> x > 289326) . fmap getSum $ spiral
      message = "The result is " ++ show result
  message `seq` putStrLn message

spiralSum ::
     forall a m. (Eq a, Num a, Monoid m)
  => Stream (Point a)
  -> Stream m
spiralSum =
  let updatePoint :: Point a -> (Point a -> m) -> (Point a -> m -> b) -> b
      updatePoint =
        let getNext :: (Point a -> m) -> Point a -> m
            getNext f = sumAtPoint (fmap f . offsetPoint)
        in updatePointSum getNext
      go :: Point a -> Stream (Point a, m) -> Stream (Point a, m)
      go p xs =
        let ps = Map.fromList $ toList xs
        in _
  in fmap snd . go (Point 0 0) . flip zip (repeat (mempty :: m))
  where
    updatePointSum ::
         ((Point a -> m) -> Point a -> m)
      -> Point a
      -> (Point a -> m)
      -> (Point a -> m -> b)
      -> b
    updatePointSum findNewPointSumGivenExistingSums p findPoint updatePointMap =
      updatePointMap p . findNewPointSumGivenExistingSums findPoint $ p
    sumAtPoint :: (Direction -> Point a -> m) -> Point a -> m
    sumAtPoint adjacentSum =
      mconcat . fmap adjacentSum $ [Left, Right, Up, Down]
    offsetPoint :: Direction -> Point a -> Point a
    offsetPoint Left Point {x, y} = Point {x = x - 1, y}
    offsetPoint Right Point {x, y} = Point {x = x + 1, y}
    offsetPoint Up Point {x, y} = Point {x, y = y + 1}
    offsetPoint Down Point {x, y} = Point {x, y = y - 1}

findS :: (a -> Bool) -> Stream a -> a
findS p = head . filter p

data Direction
  = Left
  | Right
  | Up
  | Down
