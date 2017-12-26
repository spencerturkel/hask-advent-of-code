module Day3PartTwo where

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
        spiralSum (Spiral.spiral :: Stream (Point Int)) (Sum 289326 :: Sum Int)
      message = "The result is " ++ show result
  message `seq` putStrLn message

data Mapping a b = Mapping
  { at :: a -> b
  , insert :: a -> b -> Mapping a b
  }

mappingFromList :: (Eq a) => [(a, b)] -> Mapping a (Maybe b)
mappingFromList xs =
  let at = endoMapFromList xs
      insert x y = mappingFromList ((x, y) : xs)
  in Mapping {at, insert}

endoMapFromList :: (Eq a) => [(a, b)] -> a -> Maybe b
endoMapFromList = flip lookup

spiralSum ::
     forall a m. (Ord a, Num a, Monoid m, Ord m)
  => Stream (Point a)
  -> m
  -> m
spiralSum (Stream.toList -> spiral) =
  let pointValues =
        fmap fromJust . mappingFromList . fmap (flip (,) mempty) $ spiral
      nextPoint = fmap fromJust . endoMapFromList $ zip spiral (tail spiral)
  in go nextPoint pointValues (Point 0 0)
  where
    go :: (Point a -> Point a) -> Mapping (Point a) m -> Point a -> m -> m
    go nextPoint pointValues currentPoint valueToFind
      | valueToFind >= pointValues `at` currentPoint = valueToFind
      | otherwise =
        go
          nextPoint
          (updatePointValues currentPoint pointValues)
          (nextPoint currentPoint)
          valueToFind
      where
        updatePointValues ::
             Point a -> Mapping (Point a) m -> Mapping (Point a) m
        updatePointValues p ps = insert ps p (calculatePointValue ps p)
        calculatePointValue :: Mapping (Point a) m -> Point a -> m
        calculatePointValue ps (Point {x, y}) =
          let leftValue = ps `at` (Point {x = x - 1, y})
              rightValue = ps `at` Point {x = x + 1, y}
              aboveValue = ps `at` Point {x, y = y + 1}
              belowValue = ps `at` Point {x, y = y - 1}
          in leftValue <> rightValue <> aboveValue <> belowValue
