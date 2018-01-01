module ToTree where

import Control.Arrow (Arrow((***)))
import Data.Graph (graphFromEdges, scc)
import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty as NonEmpty (toList)
import Data.Maybe (listToMaybe)
import qualified Data.Set as Set (fromList, toList)
import Data.Tree (Tree)

import ProgramInfo (ProgramInfo(ProgramInfo, _childProgramNames, _name, _weight))

programInfoToTree ::
     NonEmpty ProgramInfo
  -> Maybe (Tree ProgramInfo)
programInfoToTree =
  (\(mt, f) -> fmap (fmap f) mt) .
  ((listToMaybe . scc) *** fmap (\(_weight, _name, Set.fromList -> _childProgramNames) -> ProgramInfo {_name, _weight, _childProgramNames})) .
  (\(g, f , _) -> (g, f)) .
  graphFromEdges .
  fmap
    (\(ProgramInfo {_name, _childProgramNames, _weight}) ->
       (_weight, _name, Set.toList _childProgramNames)) .
  NonEmpty.toList