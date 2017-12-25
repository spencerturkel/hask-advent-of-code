module StreamRecursion where

import Data.Stream ((<:>), Stream)

data StreamF a f = Cons a f

type instance Base (Stream a) = StreamF a

instance Corecursive (Stream a) where
  embed :: StreamF a (Stream a) -> Stream a
  embed (Cons x xs) = x <:> xs