module Day6
  ( runDay6
  ) where

import Control.Monad.ST (ST, stToIO)
import Data.Function (on)
import Data.List (maximumBy)
import Data.Monoid (Sum(Sum))
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Vector.Unboxed (Vector, freeze, fromList, thaw, toList)
import Data.Vector.Unboxed.Mutable (MVector, Unbox, length, modify, read, write)
import Prelude hiding (cycle, length, read)

runDay6 :: IO ()
runDay6 = do
  putStrLn "Running Day 6..."
  vec <-
    thaw . fromList $ [2 :: Int, 8, 8, 5, 4, 2, 3, 1, 5, 5, 1, 2, 15, 13, 5, 14]
  Sum count <- stToIO $ countRedistributionCycles vec 12 (Sum (1 :: Int))
  print count

countRedistributionCycles ::
     forall a b s. (Num a, Ord a, Unbox a, Monoid b)
  => MVector s a
  -> Int
  -> b
  -> ST s b
countRedistributionCycles vec initialIndex cycle = do
  initialVec <- freeze vec
  go initialIndex (Set.singleton initialVec)
  where
    len :: Int
    len = length vec
    go :: Int -> Set (Vector a) -> ST s b
    go index seen = do
      valueToDistribute <- read vec index
      write vec index 0
      distribute valueToDistribute ((index + 1) `mod` len)
      frozen <- freeze vec
      if Set.member frozen seen
        then pure mempty
        else fmap (mappend cycle) $ do
               let nextIndex =
                     fst . maximumBy (compare `on` snd) . zip [0 ..] $
                     toList frozen
                   nextSeen = Set.union seen $ Set.singleton frozen
               go nextIndex nextSeen
    distribute :: a -> Int -> ST s ()
    distribute value startIndex
      | value <= 0 = pure ()
      | otherwise = do
        modify vec (+ value) startIndex
        distribute (value - 1) ((startIndex + 1) `mod` len)
