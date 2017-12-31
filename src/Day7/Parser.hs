module Parser where

import Control.Applicative (Alternative(some, (<|>)))
import Control.Monad ((<=<))
import Data.Char (isAlpha, isDigit)
import Data.List (foldl')
import Data.Set (Set, empty, fromList)
import Text.ParserCombinators.ReadP
  ( ReadP
  , char
  , option
  , readP_to_S
  , satisfy
  , sepBy
  , skipMany
  , skipSpaces
  , string
  )

import ProgramInfo (ProgramInfo(ProgramInfo, _childProgramNames, _name))

parseInput :: String -> Maybe ProgramInfo
parseInput = toBestParse . allInputParses

toBestParse :: forall a b. [(a, [b])] -> Maybe a
toBestParse = fmap fst <$> foldl' acc Nothing
  where
    acc :: Maybe (a, Int) -> (a, [b]) -> Maybe (a, Int)
    acc ((<|> Just (undefined, 0)) -> Just (x, xLen)) (y, length -> yLen) =
      Just $ case compare xLen yLen of
        LT -> (x, xLen)
        _ -> (y, yLen)

allInputParses :: String -> [(ProgramInfo, String)]
allInputParses = (readP_to_S pProgramInfo) <=< lines

pProgramInfo :: ReadP ProgramInfo
pProgramInfo = do
  skipSpaces
  _name <- pName
  skipSpaces
  weight
  skipSpaces
  _childProgramNames <-
    option empty $ do
      string "->"
      skipSpaces
      pChildProgramNames
  pure $ ProgramInfo {_name, _childProgramNames}

pName :: ReadP String
pName = some (satisfy isAlpha)

weight :: ReadP ()
weight = char '(' *> skipMany (satisfy isDigit) <* char ')'

pChildProgramNames :: ReadP (Set String)
pChildProgramNames = fromList <$> pChildName `sepBy` (char ',' *> skipSpaces)

pChildName :: ReadP String
pChildName = some (satisfy isAlpha)
