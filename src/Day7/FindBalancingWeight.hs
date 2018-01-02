module FindBalancingWeight where

import Control.Monad ((<=<))
import Data.Either (lefts, rights)
import Data.Function (on)
import Data.List (group, minimumBy, nub, sort)
import Data.Tree (Tree, foldTree)

import ProgramInfo (ProgramInfo(_weight))
import ToTree (programInfoToTree)

findBalancingWeight :: [ProgramInfo] -> Maybe Int
findBalancingWeight =
  (balancingWeightFromWeightedTree . fmap _weight) <=< programInfoToTree

balancingWeightFromWeightedTree :: Tree Int -> Maybe Int
balancingWeightFromWeightedTree = either Just (const Nothing) . foldTree acc
  where
    acc :: Int -> [Either Int Int] -> Either Int Int
    acc _ (lefts -> x:_) = Left x
    acc weight (rights -> xs)
      | length (nub xs) == 1 = Right $ weight + sum xs
      | xs == [] = Right weight
      | otherwise =
        Left . head . minimumBy (compare `on` length) . group . sort $ xs
