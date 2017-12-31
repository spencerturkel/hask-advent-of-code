module Day3PartTwo where

import Data.Functor.Identity (Identity(Identity, runIdentity))
import Data.List (lookup, zip)
import Data.Maybe (fromJust)
import Data.Monoid (Sum(Sum))
import Data.Stream (Stream)
import qualified Data.Stream as Stream (toList)
import Prelude hiding (Left, Right, (!!), filter, head, repeat, zip)

import Point (Point(Point, x, y))
import qualified Spiral (spiral)

runDayThreePartTwo :: IO ()
runDayThreePartTwo = do
  putStrLn "Calculating..."
  let Sum result =
        spiralSum
          (Spiral.spiral :: Stream (Point Int))
          (Sum (289326 - 1 :: Int))
      message = "The result is " ++ show result
  message `seq` putStrLn message

mappingFromList :: (Eq a) => [(a, b)] -> a -> Maybe b
mappingFromList = flip lookup

updateMapping :: (Applicative f, Eq a) => a -> b -> (a -> f b) -> a -> f b
updateMapping x y f a =
  if a == x
    then pure y
    else f a

spiralSum ::
     forall a m. (Ord a, Num a, Monoid m, Num m, Ord m)
  => Stream (Point a)
  -> m
  -> m
spiralSum (Stream.toList -> spiral) =
  let pointValues :: Point a -> m
      pointValues =
        fmap fromJust .
        updateMapping (Point 0 0) 1 . mappingFromList . fmap (flip (,) mempty) $
        spiral
      nextPoint :: Point a -> Point a
      nextPoint = fmap fromJust . mappingFromList $ zip spiral (tail spiral)
  in go nextPoint pointValues (Point {x = 1, y = 0}) 1
  where
    go :: (Point a -> Point a) -> (Point a -> m) -> Point a -> m -> m -> m
    go nextPoint pointValues currentPoint previousValue valueToFind
      | previousValue > valueToFind = previousValue
      | otherwise =
        let newValue = calculatePointValue pointValues currentPoint
        in go
             nextPoint
             (runIdentity .
              updateMapping currentPoint newValue (Identity . pointValues))
             (nextPoint currentPoint)
             newValue
             valueToFind
      where
        calculatePointValue :: (Point a -> m) -> Point a -> m
        calculatePointValue ps (Point {x, y}) =
          foldMap
            getValue
            [ (x - 1, y)
            , (x - 1, y + 1)
            , (x, y + 1)
            , (x + 1, y + 1)
            , (x + 1, y)
            , (x + 1, y - 1)
            , (x, y - 1)
            , (x - 1, y - 1)
            ]
          where
            getValue (x', y') = ps $ Point {x = x', y = y'}
