module Main where

import Day6 (countUniqueDistributionCycles, actualInput, testInput)

main :: IO ()
main = do
    print $ countUniqueDistributionCycles testInput
    print $ countUniqueDistributionCycles actualInput