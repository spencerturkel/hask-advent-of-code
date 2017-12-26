module Day4 where

import Data.List ((\\), sort, nub)

import Paths_HaskAdventOfCode (getDataFileName)

runDay4PartOne :: IO ()
runDay4PartOne = do
  input <- getDataFileName "input/day4.txt" >>= readFile
  print .
    length .
    filter (null) . fmap (unwords . (\xs -> xs \\ nub xs) . words) . lines $
    input

runDay4PartTwo :: IO ()
runDay4PartTwo = do
  input <- getDataFileName "input/day4.txt" >>= readFile
  print .
    length .
    filter (null) . fmap (unwords . (\xs -> xs \\ nub xs) . fmap sort . words) . lines $
    input
