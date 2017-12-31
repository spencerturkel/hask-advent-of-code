module Parser
  ( parseInput
  ) where

import Control.Applicative (Alternative(many, some))
import Data.Set (Set, empty, fromList, member, singleton, union)
import Text.ParserCombinators.ReadP
  ( ReadP
  , ReadS
  , char
  , readP_to_S
  , satisfy
  , skipMany
  , skipSpaces
  )

import ProgramInfo (ProgramInfo(ProgramInfo, _childProgramNames, _name))

parseInput :: ReadS [ProgramInfo]
parseInput = readP_to_S $ many pProgramInfo

pProgramInfo :: ReadP ProgramInfo
pProgramInfo = do
    skipSpaces
    _name <- pName
    skipSpaces
    weight
    skipSpaces
    _childProgramNames <- pChildProgramNames
    pure $ ProgramInfo {_name, _childProgramNames}

pName :: ReadP String
pName = some (satisfy (/= ' '))

weight :: ReadP ()
weight =
  char '(' *> skipMany (satisfy . flip member . fromList $ "0123456789") <*
   char ')'

pChildProgramNames :: ReadP (Set String)
pChildProgramNames = go empty
  where
    go :: Set String -> ReadP (Set String)
    go names = go . union names . singleton =<< pChildName

pChildName :: ReadP String
pChildName = some (satisfy (/= ','))
