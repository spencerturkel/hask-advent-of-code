module StreamRecursion where

import Data.Functor.Foldable (Base, Corecursive(embed), Recursive(project))
import Data.Stream (Stream(Cons))

type instance Base (Stream a) = (,) a

instance Corecursive (Stream a) where
  embed :: (a, Stream a) -> Stream a
  embed (x, xs) = Cons x xs

instance Recursive (Stream a) where
  project :: Stream a -> (a, Stream a)
  project (Cons x xs) = (x, xs)