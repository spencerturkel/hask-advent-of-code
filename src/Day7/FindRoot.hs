module FindRoot where

import Control.Arrow (Arrow(first))
import Data.Graph (Graph, Vertex, graphFromEdges, topSort)
import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty as NonEmpty (toList)
import qualified Data.Set as Set (toList)

import ProgramInfo (ProgramInfo(ProgramInfo, _childProgramNames, _name))

findRootName :: NonEmpty ProgramInfo -> String
findRootName =
  (\(_, x, _) -> x) .
  (\((v, f)) -> f v) .
  first (head . topSort) . (\(g, f, _) -> (g, f)) . programInfoToGraph

programInfoToGraph ::
     NonEmpty ProgramInfo
  -> (Graph, Vertex -> ((), String, [String]), String -> Maybe Vertex)
programInfoToGraph =
  graphFromEdges .
  fmap
    (\(ProgramInfo {_name, _childProgramNames}) ->
       ((), _name, Set.toList _childProgramNames)) .
  NonEmpty.toList
