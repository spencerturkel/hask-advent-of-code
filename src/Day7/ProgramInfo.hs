module ProgramInfo where

import Data.Set (Set)

data ProgramInfo = ProgramInfo
  { _name :: String
  , _childProgramNames :: Set String
  , _weight :: Int
  } deriving (Show, Read, Eq, Ord)
