module ToTree where

import Data.Graph (Graph, Vertex, dfs, graphFromEdges, topSort)
import Data.List (uncons)
import qualified Data.Set as Set (fromList, toList)
import Data.Tree (Tree)

import ProgramInfo
  ( ProgramInfo(ProgramInfo, _childProgramNames, _name, _weight)
  )

programInfoToTree :: [ProgramInfo] -> Maybe (Tree ProgramInfo)
programInfoToTree =
  uncurry fromGraph .
  fmap
    (fmap
       (\(_weight, _name, Set.fromList -> _childProgramNames) ->
          ProgramInfo {_name, _weight, _childProgramNames})) .
  (\(g, f, _) -> (g, f)) .
  graphFromEdges .
  fmap
    (\(ProgramInfo {_name, _childProgramNames, _weight}) ->
       (_weight, _name, Set.toList _childProgramNames))

fromGraph :: Graph -> (Vertex -> ProgramInfo) -> Maybe (Tree ProgramInfo)
fromGraph graph f = do
  (top, _) <- uncons (topSort graph)
  fmap (fmap f . fst) (uncons (dfs graph [top]))
