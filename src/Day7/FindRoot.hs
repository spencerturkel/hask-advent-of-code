module FindRoot where

import Control.Arrow (Arrow((&&&), first))
import Data.Graph (graphFromEdges)
import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty as NonEmpty (toList)
import Data.Set (fromList)
import qualified Data.Set as Set (toList)

import ProgramInfo (ProgramInfo(ProgramInfo, _childProgramNames, _name))

findRootName :: NonEmpty ProgramInfo -> String
findRootName =
  fst .
  fmap fromList .
  (\(f, len) -> f (len - 1)) .
  first
    ((fmap (\((), z, a) -> (z, a)) .
      (\(_, f, _) -> f) .
      graphFromEdges .
      fmap
        (\(ProgramInfo {_name, _childProgramNames}) ->
           ((), _name, Set.toList _childProgramNames)))) .
  (id &&& length) . NonEmpty.toList
