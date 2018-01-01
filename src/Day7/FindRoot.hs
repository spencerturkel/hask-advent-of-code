module FindRoot where

import Data.Tree (rootLabel)

import ProgramInfo (ProgramInfo(_name))
import ToTree (programInfoToTree)

findRootName :: [ProgramInfo] -> Maybe String
findRootName = fmap (_name . rootLabel) . programInfoToTree
