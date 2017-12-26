module Day4PartOne where

import Paths_HaskAdventOfCode (getDataFileName)

runDay4PartOne :: IO ()
runDay4PartOne = do
    fileName <- getDataFileName "input/day4.txt"
    putStrLn fileName
    input <- readFile fileName
    print $ take 2 $ lines input
    return ()