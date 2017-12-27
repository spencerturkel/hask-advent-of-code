module Day5 where

import Control.Monad ((<=<))
import Control.Monad.ST (ST, runST)
import Data.Vector.Unboxed (Vector, fromList, thaw)
import qualified Data.Vector.Unboxed.Mutable as V
import Debug.Trace (trace)

import Paths_HaskAdventOfCode (getDataFileName)

runDay5PartOne :: IO ()
runDay5PartOne = runDay5 . OffsetAdjuster $ fmap (+ 1)

runDay5PartTwo :: IO ()
runDay5PartTwo =
  runDay5 . OffsetAdjuster . fmap $ \x ->
    if x >= 3
      then x - 1
      else x + 1

runDay5 :: OffsetAdjuster Int -> IO ()
runDay5 adjuster = do
  putStrLn "Test input... "
  printStepsToExit adjuster $ fmap Offset $ [0, 3, 0, 1, -3]
  putStrLn "Real input..."
  printStepsToExit adjuster <=<
    (readInput :: FilePath -> IO [Offset Int]) <=< getDataFileName $
    "input/day5.txt"

readInput :: (Read a) => FilePath -> IO [Offset a]
readInput path = fmap (Offset . read) . lines <$> readFile path

printStepsToExit :: (Step a, Show a) => OffsetAdjuster a -> [Offset a] -> IO ()
printStepsToExit adjuster = print . findStepsToExit adjuster

newtype Offset a = Offset
  { _offset :: a
  } deriving (Eq, Functor, Num, Ord, Show)

newtype OffsetAdjuster a = OffsetAdjuster
  { _offsetAdjuster :: Offset a -> Offset a
  }

type Step a = a ~ Int

findStepsToExit ::
     forall a. Step a
  => OffsetAdjuster a
  -> [Offset a]
  -> a
findStepsToExit OffsetAdjuster {_offsetAdjuster = adjuster} =
  execute . fromList . fmap _offset
  where
    execute :: Vector a -> a
    execute program = runST $ thaw program >>= go 0
    go :: a -> V.MVector s a -> ST s a
    go index program
      | trace ("index " ++ show index) False = undefined
      | index < 0 || index >= fromIntegral (V.length program) = pure 0
      | otherwise = do
        nextIndex <- (index +) <$> V.read program index
        V.modify program adjustment index
        (+ 1) <$> go nextIndex program
      where
        adjustment = _offset . adjuster . Offset
