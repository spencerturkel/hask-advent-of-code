module Day4PartOne where

import Data.List ((\\), nub)

import Paths_HaskAdventOfCode (getDataFileName)

runDay4PartOne :: IO ()
runDay4PartOne = do
    input <- getDataFileName "input/day4.txt" >>= readFile
    print . length . filter (null) . fmap (unwords . (\xs -> xs \\ nub xs) . words) . lines $ input