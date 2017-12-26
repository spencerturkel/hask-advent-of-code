module Day3PartTwo where

import Data.Hashable (Hashable)
import Data.List (zip)
import Data.HashMap.Lazy (HashMap, (!), insert)
import qualified Data.HashMap.Lazy as HashMap (fromList)
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
     forall a m. (Hashable a, Ord a, Num a, Monoid m, Ord m)
  => Stream (Point a)
  -> m
  -> m
spiralSum (Stream.toList -> spiral) =
  let pointValues = HashMap.fromList . fmap (flip (,) mempty) $ spiral
      nextPoint = (!) . HashMap.fromList $ zip spiral (tail spiral)
  in go nextPoint pointValues (Point 0 0)
  where
    go :: (Point a -> Point a) -> HashMap (Point a) m -> Point a -> m -> m
    go nextPoint pointValues currentPoint valueToFind
      | valueToFind >= pointValues ! currentPoint = valueToFind
      | otherwise =
        go
          nextPoint
          (updatePointValues currentPoint pointValues)
          (nextPoint currentPoint)
          valueToFind
      where
        updatePointValues :: Point a -> HashMap (Point a) m -> HashMap (Point a) m
        updatePointValues p ps = insert p (calculatePointValue ps p) ps
        calculatePointValue :: HashMap (Point a) m -> Point a -> m
        calculatePointValue ps (Point {x, y}) =
          let leftValue = ps ! (Point {x = x - 1, y})
              rightValue = ps ! Point {x = x + 1, y}
              aboveValue = ps ! Point {x, y = y + 1}
              belowValue = ps ! Point {x, y = y - 1}
          in leftValue <> rightValue <> aboveValue <> belowValue
