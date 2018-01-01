module FindRoot where

import Data.Tree (rootLabel)
import Data.List.NonEmpty (NonEmpty)

import ProgramInfo (ProgramInfo(_name))
import ToTree (programInfoToTree)

findRootName :: NonEmpty ProgramInfo -> Maybe String
findRootName =
  fmap (_name . rootLabel) . programInfoToTree
