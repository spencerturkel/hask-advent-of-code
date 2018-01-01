module FindRoot where

import Control.Arrow (Arrow(first))
import Data.Graph (topSort)
import Data.List.NonEmpty (NonEmpty)

import ProgramInfo (ProgramInfo)
import ToGraph (programInfoToGraph)

findRootName :: NonEmpty ProgramInfo -> String
findRootName =
  (\(_, x, _) -> x) .
  (\((v, f)) -> f v) .
  first (head . topSort) . (\(g, f, _) -> (g, f)) . programInfoToGraph
