module Point where

import GHC.Generics (Generic)

data Point a = Point
  { x :: a
  , y :: a
  } deriving (Eq, Generic, Ord, Show)