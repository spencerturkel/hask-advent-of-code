module Day3PartTwo where

import Data.Functor.Identity (Identity(Identity, runIdentity))
import Data.List (lookup, zip)
import Data.Maybe (fromJust)
import Data.Monoid (Sum(Sum), (<>))
import Data.Stream (Stream)
import qualified Data.Stream as Stream (toList)
import Prelude hiding (Left, Right, (!!), filter, head, repeat, zip)

import Point (Point(Point, x, y))
import qualified Spiral (spiral)

runDayThreePartTwo :: IO ()
runDayThreePartTwo = do
  putStrLn "Calculating..."
  let Sum result =
        spiralSum (Spiral.spiral :: Stream (Point Int)) (Sum (289326 - 1 :: Int))
      message = "The result is " ++ show result
  message `seq` putStrLn message

mappingFromList :: (Eq a) => [(a, b)] -> a -> Maybe b
mappingFromList = flip lookup

updateMapping :: (Applicative f, Eq a) => a -> b -> (a -> f b) -> a -> f b
updateMapping x y f a = if a == x then pure y else f a

spiralSum ::
     forall a m. (Ord a, Num a, Monoid m, Ord m)
  => Stream (Point a)
  -> m
  -> m
spiralSum (Stream.toList -> spiral) =
  let pointValues =
        fmap fromJust . mappingFromList . fmap (flip (,) mempty) $ spiral
      nextPoint = fmap fromJust . mappingFromList $ zip spiral (tail spiral)
  in go nextPoint pointValues (Point 0 0)
  where
    go :: (Point a -> Point a) -> (Point a -> m) -> Point a -> m -> m
    go nextPoint pointValues currentPoint valueToFind
      | valueToFind > pointValues currentPoint = valueToFind
      | otherwise =
        go
          nextPoint
          (runIdentity . updateMapping currentPoint (calculatePointValue pointValues currentPoint) (Identity .pointValues))
          (nextPoint currentPoint)
          valueToFind
      where
        calculatePointValue :: (Point a -> m) -> Point a -> m
        calculatePointValue ps (Point {x, y}) =
          let leftValue = ps $ Point {x = x - 1, y}
              rightValue = ps $ Point {x = x + 1, y}
              aboveValue = ps $ Point {x, y = y + 1}
              belowValue = ps $ Point {x, y = y - 1}
          in leftValue <> rightValue <> aboveValue <> belowValue
