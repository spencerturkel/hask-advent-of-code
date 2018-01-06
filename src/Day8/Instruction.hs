module Instruction where

data Operation
  = Increment
  | Decrement
  deriving (Show)

data Comparison
  = LessThan
  | LessThanOrEqual
  | NotEqual
  | Equal
  | GreaterThan
  | GreaterThanOrEqual
  deriving (Show)

data Instruction = Instruction
  { _target :: String
  , _operation :: Operation
  , _operand :: Int
  , _source :: String
  , _comparison :: Comparison
  , _comparator :: Int
  } deriving (Show)
