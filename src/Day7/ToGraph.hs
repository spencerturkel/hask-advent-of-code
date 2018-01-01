module ToGraph where

import Data.Graph (Graph, Vertex, graphFromEdges)
import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty as NonEmpty (toList)
import qualified Data.Set as Set (toList)

import ProgramInfo (ProgramInfo(ProgramInfo, _childProgramNames, _name, _weight))

programInfoToGraph ::
     NonEmpty ProgramInfo
  -> (Graph, Vertex -> (Int, String, [String]), String -> Maybe Vertex)
programInfoToGraph =
  graphFromEdges .
  fmap
    (\(ProgramInfo {_name, _childProgramNames, _weight}) ->
       (_weight, _name, Set.toList _childProgramNames)) .
  NonEmpty.toList