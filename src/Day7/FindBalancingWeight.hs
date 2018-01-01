module FindBalancingWeight where

import Control.Arrow (Arrow((***)))
import Data.Either (lefts, rights)
import Data.Function (on)
import Data.Graph (Vertex, components)
import Data.List (group, maximumBy, nub, sort)
import Data.List.NonEmpty (NonEmpty)
import Data.Tree (Tree, foldTree)

import ProgramInfo (ProgramInfo)
import ToGraph (programInfoToGraph)

findBalancingWeight :: NonEmpty ProgramInfo -> Maybe Int
findBalancingWeight =
  uncurry balancingWeightFromTree .
  (fmap (\(x, _, _) -> x) *** (head . components)) .
  (\(g, f, _) -> (f, g)) . programInfoToGraph

balancingWeightFromTree :: (Vertex -> Int) -> Tree Vertex -> Maybe Int
balancingWeightFromTree weightFromVertex =
  either Just (const Nothing) . foldTree acc
  where
    acc :: Vertex -> [Either Int Int] -> Either Int Int
    acc _ (lefts -> x:_) = Left x
    acc (weightFromVertex -> weight) (nub . rights -> _:[]) = Right weight
    acc (weightFromVertex -> weight) (rights -> []) = Right weight
    acc _ (rights -> xs) =
      Right . head . maximumBy (compare `on` length) . group . sort $ xs
