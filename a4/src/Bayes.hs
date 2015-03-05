{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE DataKinds, KindSignatures, GADTs, TypeOperators, ScopedTypeVariables, StandaloneDeriving #-}
module Bayes where

import Control.Applicative
import Data.Array (Array, Ix)
import qualified Data.Array as Array
import Data.Maybe (mapMaybe)
import Data.List (elemIndex)

import Debug.Trace (trace)

type Var = Int
type Val = Bool
type Prob = Float
data Normalized = Normalized | Unnormalized
  deriving (Show, Eq)

allVals :: [Val]
allVals = [minBound .. maxBound]

makeValRange :: Int -> ([Val], [Val])
makeValRange i = (replicate i minBound, replicate i maxBound)

instance (Ix a, Enum a) => Ix [a] where
  range ([], []) = [[]]
  range (s:ss, e:es) = (:) <$> [s..e] <*> Array.range (ss, es)
  range _ = undefined

  index ([s], [e]) [i]
    | s <= i && i <= e = fromEnum i - fromEnum s
    | otherwise        = undefined
  index (s:ss, e:es) (i:is) = let this_idx = Array.index ([s], [e]) [i]
                                  rest_idx = Array.index (ss, es) is
                                  rest_total = Array.rangeSize (ss, es)
                              in this_idx * rest_total + rest_idx
  index _ _ = undefined

  inRange ([], []) [] = True
  inRange (s:ss, e:es) (i:is) = s <= i && i <= e && Array.inRange (ss, es) is
  inRange _ _ = False

  rangeSize ([], []) = 1
  rangeSize (s:ss, e:es) = (fromEnum e - fromEnum s + 1) * Array.rangeSize (ss, es)
  rangeSize _ = undefined

data Factor :: * -> Normalized -> * where
  Factor :: [Var] -> Array [Val] t -> Factor t 'Unnormalized
deriving instance Show t => Show (Factor t n)
deriving instance Eq t => Eq (Factor t n)

deleteAt :: Int -> [a] -> [a]
deleteAt _ [] = []
deleteAt 0 (_:as) = as
deleteAt i (a:as) = a:deleteAt (i-1) as

restrict :: Factor t n
         -> Var
         -> Val
         -> Factor t 'Unnormalized
restrict (Factor vars arr) var val = case var `elemIndex` vars of
  Nothing -> trace "Restricting variable that doesn't exist!" undefined
  Just i -> let new_vars = deleteAt i vars
                restrict_f (idx, p)
                  | (idx!!i) == val = Just (deleteAt i idx, p)
                  | otherwise         = Nothing
                new_assocs = mapMaybe restrict_f $ Array.assocs arr
            in Factor new_vars $ Array.array (makeValRange $ length new_vars) new_assocs




