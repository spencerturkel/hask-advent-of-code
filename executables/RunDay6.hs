module Main where

import Day6
  ( actualInput
  , countUniqueDistributionCycles
  , lengthOfDistributionCycles
  , testInput
  )

main :: IO ()
main = do
  print $ countUniqueDistributionCycles testInput
  print $ countUniqueDistributionCycles actualInput
  print $ lengthOfDistributionCycles testInput
  print $ lengthOfDistributionCycles actualInput
