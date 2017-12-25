module Point where

data Point a = Point
  { x :: a
  , y :: a
  } deriving (Eq, Show)