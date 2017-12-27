module Day5 where

import Control.Monad ((>=>))
import Data.List (zip)
import Data.Map (Map, (!), adjust, fromList, size)
import Debug.Trace (trace)

import Paths_HaskAdventOfCode (getDataFileName)

runDay5PartOne :: IO ()
runDay5PartOne = runDay5 . OffsetAdjuster $ fmap (+ 1)

runDay5PartTwo :: IO ()
runDay5PartTwo =
  runDay5 . OffsetAdjuster . fmap $ \x ->
    trace ("adjust " ++ show x) $ if x >= 3
      then x - 1
      else x + 1

runDay5 :: OffsetAdjuster Int -> IO ()
runDay5 adjuster =
  getDataFileName >=>
  (readInput :: FilePath -> IO [Offset Int]) >=> printStepsToExit adjuster $
  "input/day5.txt"

readInput :: (Read a) => FilePath -> IO [Offset a]
readInput path = fmap (Offset . read) . lines <$> readFile path

printStepsToExit ::
     (Enum a, Ord a, Num a, Show a) => OffsetAdjuster a -> [Offset a] -> IO ()
printStepsToExit adjuster = print . findStepsToExit adjuster

newtype Offset a = Offset
  { _offset :: a
  } deriving (Eq, Functor, Num, Ord, Show)

newtype OffsetAdjuster a = OffsetAdjuster
  { _offsetAdjuster :: Offset a -> Offset a
  }

findStepsToExit ::
     forall a. (Enum a, Ord a, Num a)
  => OffsetAdjuster a
  -> [Offset a]
  -> a
findStepsToExit OffsetAdjuster {_offsetAdjuster = adjuster} =
  go 0 . fromList . zip [0 ..]
  where
    go :: a -> Map a (Offset a) -> a
    go index program
      | index < 0 || index >= fromIntegral (size program) = 0
      | otherwise = 1 + go nextIndex newProgram
      where
        nextIndex = index + _offset (program ! index)
        newProgram = adjust adjuster index program
