module Parser where

import Control.Applicative (Alternative((<|>), many))
import Text.Regex.Applicative (RE, match, psym, string, sym)

import Instruction
  ( Comparison(Equal, GreaterThan, GreaterThanOrEqual, LessThan,
           LessThanOrEqual, NotEqual)
  , Instruction(Instruction)
  , Operation(Decrement, Increment)
  )

parseProgram :: String -> Maybe [Instruction]
parseProgram = traverse (match parseInstruction) . lines

parseInstruction :: RE Char Instruction
parseInstruction =
  Instruction <$> pTarget <* space <*> pOperation <* space <*> pOperand <* space <*>
  pSource <*
  space <*>
  pComparison <*
  space <*>
  pComparator

pTarget :: RE Char String
pTarget = many nonSpace

pOperation :: RE Char Operation
pOperation = (Increment <$ string "inc") <|> (Decrement <$ string "dec")

pOperand :: RE Char Int
pOperand = digit

pSource :: RE Char String
pSource = word

pComparison :: RE Char Comparison
pComparison =
  (LessThan <$ sym '<') <|> (LessThanOrEqual <$ string "<=") <|>
  (NotEqual <$ string "!=") <|>
  (Equal <$ string "==") <|>
  (GreaterThan <$ sym '>') <|>
  (GreaterThanOrEqual <$ string ">=")

pComparator :: RE Char Int
pComparator = digit

digit :: RE Char Int
digit = read <$> many (psym (`elem` ['0','1' .. '9']))

word :: RE Char String
word = many nonSpace

nonSpace :: RE Char Char
nonSpace = psym (/= ' ')

space :: RE Char Char
space = sym ' '
