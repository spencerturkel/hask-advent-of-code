module Day3PartTwo where

import Data.List (zip)
import Data.Map.Lazy (Map, (!), insert)
import qualified Data.Map.Lazy as Map (fromList)
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
        spiralSum (Spiral.spiral :: Stream (Point Int)) (Sum 289326 :: Sum Int)
      message = "The result is " ++ show result
  message `seq` putStrLn message

spiralSum ::
     forall a m. (Ord a, Num a, Monoid m, Ord m)
  => Stream (Point a)
  -> m
  -> m
spiralSum (Stream.toList -> spiral) =
  let pointValues = Map.fromList . fmap (flip (,) mempty) $ spiral
      nextPoint = (!) . Map.fromList $ zip spiral (tail spiral)
  in go nextPoint pointValues (Point 0 0)
  where
    go :: (Point a -> Point a) -> Map (Point a) m -> Point a -> m -> m
    go nextPoint pointValues currentPoint valueToFind
      | valueToFind >= pointValues ! currentPoint = valueToFind
      | otherwise =
        go
          nextPoint
          (updatePointValues currentPoint pointValues)
          (nextPoint currentPoint)
          valueToFind
      where
        updatePointValues :: Point a -> Map (Point a) m -> Map (Point a) m
        updatePointValues p ps = insert p (calculatePointValue ps p) ps
        calculatePointValue :: Map (Point a) m -> Point a -> m
        calculatePointValue ps (Point {x, y}) =
          let leftValue = ps ! (Point {x = x - 1, y})
              rightValue = ps ! Point {x = x + 1, y}
              aboveValue = ps ! Point {x, y = y + 1}
              belowValue = ps ! Point {x, y = y - 1}
          in leftValue <> rightValue <> aboveValue <> belowValue
