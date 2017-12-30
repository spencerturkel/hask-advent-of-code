module Day6 where

import Data.Function (on)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Vector.Unboxed
  ( Unbox
  , Vector
  , foldl'
  , fromList
  , indexed
  , length
  , modify
  )
import qualified Data.Vector.Unboxed.Mutable as M (modify, write)
import Prelude hiding (length)

testInput :: [Int]
testInput = [0, 2, 7, 0]

actualInput :: [Int]
actualInput = [2, 8, 8, 5, 4, 2, 3, 1, 5, 5, 1, 2, 15, 13, 5, 14]

uniqueDistributionCycles ::
     forall a. (Num a, Ord a, Unbox a)
  => Vector a
  -> Set (Vector a)
uniqueDistributionCycles = foldEither acc Set.empty . iterate redistribute
  where acc :: Vector a -> Set (Vector a) -> Either (Set (Vector a)) (Set (Vector a))
        acc x xs = if Set.member x xs then Left xs else Right $ Set.union (Set.singleton x) xs

foldEither :: (a -> b -> Either c b) -> b -> [a] -> c
foldEither _ _ [] = undefined
foldEither f y (x:xs) = case f x y of
  Left z -> z
  Right y' -> foldEither f y' xs

redistributeTestInput :: Bool
redistributeTestInput =
  and $
  fmap
    (\(x, y) -> redistribute (fromList x) == fromList y)
    [ ([0 :: Int, 2, 7, 0], [2, 4, 1, 2])
    , ([2, 4, 1, 2], [3, 1, 2, 3])
    , ([3, 1, 2, 3], [0, 2, 3, 4])
    , ([0, 2, 3, 4], [1, 3, 4, 1])
    , ([1, 3, 4, 1], [2, 4, 1, 2])
    ]

redistribute ::
     forall a. (Num a, Ord a, Unbox a)
  => Vector a
  -> Vector a
redistribute xs =
  case leftMaximum (indexed xs) (compare `on` snd) of
    Nothing -> xs
    Just (index, value) ->
      let zeroedAtIndex = modify (\v -> M.write v index 0) xs
      in distribute zeroedAtIndex value ((index + 1) `mod` length zeroedAtIndex)
  where
    distribute vec 0 _ = vec
    distribute vec val ix =
      distribute
        (modify (\v -> M.modify v (+ 1) ix) vec)
        (val - 1)
        ((ix + 1) `mod` length vec)

leftMaximum ::
     forall a. (Unbox a)
  => Vector a
  -> (a -> a -> Ordering)
  -> Maybe a
leftMaximum xs comp = foldl' acc Nothing xs
  where
    acc :: Maybe a -> a -> Maybe a
    acc Nothing x = Just x
    acc (Just y) x =
      case comp y x of
        LT -> Just x
        _ -> Just y
