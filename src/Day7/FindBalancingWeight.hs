module FindBalancingWeight where

import Control.Monad ((<=<))
import Data.Either (lefts, rights)
import Data.Function (on)
import Data.List (group, maximumBy, nub, sort)
import Data.List.NonEmpty (NonEmpty)
import Data.Tree (Tree, foldTree)

import ProgramInfo (ProgramInfo(_weight))
import ToTree (programInfoToTree)

findBalancingWeight :: NonEmpty ProgramInfo -> Maybe Int
findBalancingWeight =
  (balancingWeightFromWeightedTree . fmap _weight) <=< programInfoToTree

balancingWeightFromWeightedTree :: Tree Int -> Maybe Int
balancingWeightFromWeightedTree = either Just (const Nothing) . foldTree acc
  where
    acc :: Int -> [Either Int Int] -> Either Int Int
    acc _ (lefts -> x:_) = Left x
    acc weight (nub . rights -> _:[]) = Right weight
    acc weight (rights -> []) = Right weight
    acc _ (rights -> xs) =
      Left . head . maximumBy (compare `on` length) . group . sort $ xs
