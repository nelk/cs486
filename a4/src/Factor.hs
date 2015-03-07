{-# OPTIONS_GHC -fno-warn-orphans #-}
module Factor where

import Prelude hiding (sum)
import Control.Applicative
import Data.Array (Array, Ix, (!))
import qualified Data.Array as Array
import Data.Maybe (mapMaybe)
import Data.List (elemIndex)
import Data.Foldable (sum)

import Debug.Trace (trace)

debugOn :: Bool
debugOn = False
traceShow :: Show v => String -> v -> v
traceShow prefix v
  | debugOn   = trace (prefix ++ show v) v
  | otherwise = v

type Var = Int
type Val = Bool
type Prob = Double
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

  index ([], []) [] = 0
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
  NormalizedFactor :: [Var] -> Array [Val] t -> Factor t 'Normalized
deriving instance Show t => Show (Factor t n)
deriving instance Eq t => Eq (Factor t n)

deleteAt :: Int -> [a] -> [a]
deleteAt _ [] = []
deleteAt 0 (_:as) = as
deleteAt i (a:as) = a:deleteAt (i-1) as

insertAt :: Int -> a -> [a] -> [a]
insertAt 0 v as = v:as
insertAt i v (a:as) = a:insertAt (i-1) v as
insertAt _ _ [] = []

unionSorted :: Ord a => [a] -> [a] -> [a]
unionSorted [] [] = []
unionSorted as [] = as
unionSorted [] bs = bs
unionSorted (a:as) (b:bs)
  | a == b    = a:unionSorted as bs
  | a < b     = a:unionSorted as (b:bs)
  | otherwise = b:unionSorted (a:as) bs

restrict :: Factor t 'Unnormalized
         -> Var
         -> Val
         -> Factor t 'Unnormalized
restrict f@(Factor vars arr) var val = case var `elemIndex` vars of
  Nothing -> f
  Just i -> let new_vars = deleteAt i vars
                restrict_f (idx, p)
                  | (idx!!i) == val = Just (deleteAt i idx, p)
                  | otherwise         = Nothing
                new_assocs = mapMaybe restrict_f $ Array.assocs arr
            in Factor new_vars $ Array.array (makeValRange $ length new_vars) new_assocs

{-
unionFactorAssocs :: [Var] -> [([Val], Prob)]
                  -> [Var] -> [([Var], Prob)]
                  -> ([Var], [([Var], Prob)])
unionFactorAssocs (avar:avars) () (bvar:bvars) () =
-}

-- |Given two lists of variables, returns a list with
-- an entry for each variable in the union of the two lists in order.
-- The entry contains the variable name, the source it came from, and the
-- index in its source list.
data SourceLoc = LeftSource | RightSource | BothSources deriving (Show, Eq)
unionSortedVars :: [Var] -> [Var] -> [(Var, SourceLoc, Int)]
unionSortedVars as' bs' = unionSortedVars_ as' 0 bs' 0
  where unionSortedVars_ :: [Var] -> Int -> [Var] -> Int -> [(Var, SourceLoc, Int)]
        unionSortedVars_ [] _ [] _ = []
        unionSortedVars_ [] _ (b:bs) bi = (b, RightSource, bi):unionSortedVars_ [] 0 bs (bi+1)
        unionSortedVars_ (a:as) ai [] _ = (a, LeftSource, ai):unionSortedVars_ as (ai+1) [] 0
        unionSortedVars_ (a:as) ai (b:bs) bi
          | a == b    = (a, BothSources, ai):unionSortedVars_ as (ai+1) bs (bi+1)
          | a < b     = (a, LeftSource, ai):unionSortedVars_ as (ai+1) (b:bs) bi
          | otherwise = (b, RightSource, bi):unionSortedVars_ (a:as) ai bs (bi+1)

elementwiseMult :: forall t. Num t
                => Array [Val] t
                -> Array [Val] t
                -> [(Var, SourceLoc, Int)]
                -> Array [Val] t
elementwiseMult arr_a arr_b info =
  let new_bounds = makeValRange $ length info
      new_range = Array.range new_bounds
      new_assocs = map doMult new_range

      doMult :: [Val] -> ([Val], t)
      doMult vals = (vals, doMult_ (zip info vals) [] [])

      doMult_ :: [((Var, SourceLoc, Int), Val)] -> [Val] -> [Val] -> t
      doMult_ [] idxa idxb = arr_a!idxa * arr_b!idxb
      doMult_ (((_, source, _), val):rest) idxa idxb
        | source == BothSources = doMult_ rest (idxa++[val]) (idxb++[val])
        | source == LeftSource  = doMult_ rest (idxa++[val]) idxb
        | source == RightSource = doMult_ rest idxa (idxb++[val])
      doMult_ _ _ _ = trace "doMult_ received bad data" undefined
  in Array.array new_bounds new_assocs


multiply :: Num t
         => Factor t 'Unnormalized
         -> Factor t 'Unnormalized
         -> Factor t 'Unnormalized
multiply (Factor vars1 arr1) (Factor vars2 arr2) =
  let merged_info = unionSortedVars vars1 vars2
      new_vars = map (\(a, _, _) -> a) merged_info
      new_arr = elementwiseMult arr1 arr2 merged_info
  in Factor new_vars new_arr


sumout :: forall t. Num t
       => Factor t 'Unnormalized
       -> Var
       -> Factor t 'Unnormalized
sumout f@(Factor vars arr) var = case var `elemIndex` vars of
  Nothing -> f
  Just i  -> let new_vars = deleteAt i vars
                 new_bounds = makeValRange $ length new_vars
                 new_range = Array.range new_bounds
                 new_assocs = map sumit new_range

                 sumit :: [Val] -> ([Val], t)
                 sumit vs = let false_idx = insertAt i False vs
                                true_idx = insertAt i True vs
                            in (vs, arr!false_idx + arr!true_idx)

             in Factor new_vars $ Array.array new_bounds new_assocs

normalize :: Factor Prob 'Unnormalized
          -> Factor Prob 'Normalized
normalize (Factor [] arr) = NormalizedFactor [] arr
normalize (Factor vars arr) =
  let total = sum arr
      alpha = (1.0 / total)
  in NormalizedFactor vars $ fmap (*alpha) arr

toUnnormalized :: Factor Prob 'Normalized
               -> Factor Prob 'Unnormalized
toUnnormalized (NormalizedFactor vars arr) = Factor vars arr


