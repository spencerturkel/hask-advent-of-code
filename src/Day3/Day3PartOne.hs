module Day3PartOne where

import Data.Stream (Stream, (!!))
import Prelude hiding ((!!))

import Spiral (spiral)
import Point (Point(Point, x, y))
import StreamRecursion ()

runPartOne :: IO ()
runPartOne = do
  putStrLn "Calculating..."
  let point = (spiral :: Stream (Point Int)) !! (289326 - 1)
  point `seq` (putStrLn $ "The point is " ++ show point)
  let distanceFromOrigin = abs (x point) + abs (y point)
      message = "The distance from the origin is " ++ show distanceFromOrigin
  distanceFromOrigin `seq` (putStrLn message)