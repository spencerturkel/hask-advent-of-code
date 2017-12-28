module Day5 where

import Control.Monad ((<=<), unless)
import Control.Monad.ST (ST, runST)
import Data.Vector.Unboxed (Vector, fromList, thaw)
import qualified Data.Vector.Unboxed.Mutable as V

import Paths_HaskAdventOfCode (getDataFileName)

runDay5PartOne :: IO ()
runDay5PartOne = runDay5WithAdjuster adjustPartOne

runDay5PartTwo :: IO ()
runDay5PartTwo = runDay5WithAdjuster adjustPartTwo

adjustPartOne :: Int -> Int
adjustPartOne = (+ 1)

adjustPartTwo :: Int -> Int
adjustPartTwo x =
  if x >= 3
    then x - 1
    else x + 1

runDay5WithAdjuster :: (Read a, Step a) => (a -> a) -> IO ()
runDay5WithAdjuster = runDay5 . OffsetAdjuster . fmap

runDay5 :: (Read a, Step a) => OffsetAdjuster a -> IO ()
runDay5 adjuster = do
  putStrLn "Test input... "
  printStepsToExit adjuster $ fmap Offset $ [0, 3, 0, 1, -3]
  putStrLn "Enter q to exit..."
  line <- getLine
  unless (line == "q") $ do
    putStrLn "Real input..."
    printStepsToExit adjuster <=< readInput <=< getDataFileName $ "input/day5.txt"

readInput :: (Read a) => FilePath -> IO [Offset a]
readInput path = fmap (Offset . read) . lines <$> readFile path

printStepsToExit :: Step a => OffsetAdjuster a -> [Offset a] -> IO ()
printStepsToExit adjuster = print . findStepsToExit adjuster

newtype Offset a = Offset
  { _offset :: a
  } deriving (Eq, Functor, Num, Ord, Show)

newtype OffsetAdjuster a = OffsetAdjuster
  { _offsetAdjuster :: Offset a -> Offset a
  }

type Step a = (Eq a, Integral a, Show a, V.Unbox a)

findStepsToExit ::
     forall a. Step a
  => OffsetAdjuster a
  -> [Offset a]
  -> a
findStepsToExit OffsetAdjuster {_offsetAdjuster = adjuster} =
  execute . fromList . fmap _offset
  where
    execute :: Vector a -> a
    execute program = runST $ do
      mutProgram <- thaw program 
      go 0 0 (V.length mutProgram) mutProgram
    go :: Int -> a -> Int -> V.MVector s a -> ST s a
    go index resultSoFar programLength program
      | index < 0 || index >= programLength = pure resultSoFar
      | otherwise = do
        nextIndex <- (\x -> fromIntegral x + index) <$> V.read program index
        V.modify program adjustment index
        go nextIndex (resultSoFar + 1) programLength program
      where
        adjustment = _offset . adjuster . Offset
