module Interpreter (runProgram, runInstruction) where

import Data.List (scanl')
import Data.Map.Lazy (Map, alter, empty, findWithDefault)
import Data.Maybe (fromMaybe)

import Instruction
  ( Comparison(Equal, GreaterThan, GreaterThanOrEqual, LessThan,
           LessThanOrEqual, NotEqual)
  , Instruction(Instruction, _comparator, _comparison, _operand,
            _operation, _source, _target)
  , Operation(Decrement, Increment)
  )

runProgram :: [Instruction] -> [Map String Int]
runProgram = scanl' (flip runInstruction) empty

runInstruction :: Instruction -> Map String Int -> Map String Int
runInstruction Instruction { _target
                           , _operation
                           , _operand
                           , _source
                           , _comparison
                           , _comparator
                           } state =
  if runComparison _comparison (findWithDefault 0 _source state) _comparator
    then alter
           (Just . (flip (runOperation _operation) _operand) . fromMaybe 0)
           _target
           state
    else state

runOperation :: Num a => Operation -> a -> a -> a
runOperation Increment = (+)
runOperation Decrement = flip subtract

runComparison :: Ord a => Comparison -> a -> a -> Bool
runComparison LessThan = (<)
runComparison LessThanOrEqual = (<=)
runComparison NotEqual = (/=)
runComparison Equal = (==)
runComparison GreaterThan = (>)
runComparison GreaterThanOrEqual = (>=)
