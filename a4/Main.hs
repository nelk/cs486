{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE DataKinds, KindSignatures, GADTs, TypeOperators, ScopedTypeVariables, StandaloneDeriving #-}

import Prelude
import Control.Applicative
--import GHC.TypeLits
--import Data.Tensor.Vector (Tensor, (:|:), generate, MultiIndex, fromMultiIndex, toMultiIndex, slice, Reverse, Ext)
import Data.Array (Array, Ix)
import qualified Data.Array as Array
--import qualified Data.Tensor.Vector as Tensor
--import Data.List (elemIndex)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Maybe (catMaybes)

import Debug.Trace (trace)

type Var = Int
type Val = Bool
type Prob = Float
data Normalized = Normalized | Unnormalized

allVals :: [Val]
allVals = [minBound .. maxBound]

makeValRange :: Int -> ([Val], [Val])
makeValRange i = (replicate i minBound, replicate i maxBound)


{-
data T :: [Nat] -> * where
  T :: T '[]
  T1 :: T '[1]
  T2 :: T '[2]
  TCons :: T '[i] -> T is -> T (i ': is)

thing2 = TCons T1 $ TCons T1 T2
-}

{-
data Factor :: * -> Normalized -> * -> * -> * where
  Factor :: t -> [Char] -> Tensor dom t -> Factor t 'Unnormalized [Char] (Tensor dom t)
  -}

--data Factor :: * -> Normalized -> Set.Set Char -> * -> * where
  --Factor :: t -> [Char] -> Tensor dom t -> Factor t 'Unnormalized vars (Tensor dom t)

{-
restrict :: MultiIndex ds
         => Factor t n vs (Tensor (d :|: ds) t)
         -> Char
         -> Bool
         -> Factor t Unnormalized vs (Tensor ds t)
restrict (Factor t vars tensor) var val =
  case elemIndex var vars of
    Nothing -> undefined
    Just i -> let new_vars = take i vars ++ drop (i + 1) vars
                  genNewTensor idx = let idx_i = fromMultiIndex idx
                                         -- TODO: For idx_i >= i use idx_i - 1
                                         -- TODO: For 
                                         selected_one_zero = idx_i !! i
                                     in undefined
              in Factor t new_vars $ generate genNewTensor
              -}

{-
restrict :: MultiIndex ds
         => Factor t n vs (Tensor ds t)
         -> i
         -> j
         -> Factor t Unnormalized vs (Tensor (Reverse (Ext (Reverse j) (Reverse (Ext i ds)))) t)
restrict (Factor t vars tensor) i j = Factor t vars (slice i j tensor)
-}

-- TODO: Use Data.IntSet? {vars :: IntSet, probs (mapping set of true vars to prob) :: Map IntSet Float)?

{-
data EitherOrBoth a = OnlyLeft a | OnlyRight a | Both a a

data FreeFactor :: * -> * where
  FreeFactor :: FreeFactor (Array ix t)
  -}

--foo = let (FreeFactor a) = FreeFactor
      --in a

-- TODO: Can merge with below.
{-
index_ :: (Ix a, Enum a) => ([a], [a]) -> [a] -> (Int, Int)
index_ (s:[], e:[]) (i:[])
  | s <= i && i <= e = (fromEnum i - fromEnum s, fromEnum e - fromEnum s)
  | otherwise        = undefined
index_ (s:ss, e:es) (i:is) = let (this_total, this_idx) = index_ ([s], [e]) [i]
                                 (rest_total, rest_idx) = index_ (ss, es) is
                             in (this_total * rest_total, this_idx * rest_total + rest_idx)
index_ _ _ = undefined
-}

instance (Ix a, Enum a) => Ix [a] where
  range ([], []) = [[]]
  range (s:ss, e:es) = (:) <$> [s..e] <*> Array.range (ss, es)
  range _ = undefined

  index (s:[], e:[]) (i:[])
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

data Factor :: Normalized -> * where
  Factor :: Set Var -> Array [Val] Prob -> Factor 'Unnormalized 
deriving instance Show (Factor n)

deleteAt :: Int -> [a] -> [a]
deleteAt _ [] = []
deleteAt 0 (_:as) = as
deleteAt i (a:as) = a:deleteAt (i-1) as

restrict :: Factor n
         -> Var
         -> Val
         -> Factor 'Unnormalized
restrict (Factor vars arr) var val = if var `Set.member` vars
  then let new_vars = var `Set.delete` vars
           n = Set.size vars
           restrict_f (idx, p)
            | (idx!!var) == val = Just (deleteAt var idx, p)
            | otherwise         = Nothing
           new_assocs = catMaybes $ map restrict_f $ Array.assocs arr
       in Factor new_vars $ Array.array (makeValRange $ n - 1) new_assocs
  else trace "Restricting variable that doesn't exist!" undefined

main :: IO ()
main = let a :: Array [Val] Prob = Array.array ([False, False, False], [True, True, True])
                                    [([x, y, z], 2*(fromIntegral(fromEnum x)*2 + fromIntegral(fromEnum y)) + fromIntegral(fromEnum z)) | x <- allVals, y <- allVals, z <- allVals]
       in print $ restrict (Factor (Set.fromList [0, 1, 2]) a) 2 False

