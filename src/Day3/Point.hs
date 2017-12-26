module Point where

import Data.Hashable (Hashable)
import GHC.Generics (Generic)

data Point a = Point
  { x :: a
  , y :: a
  } deriving (Eq, Generic, Hashable, Ord, Show)