{-# LANGUAGE DataKinds, ScopedTypeVariables, GADTs #-}
module Bayes where

import Prelude hiding (foldl)
import Data.Foldable (foldl)
import Control.Applicative
import Data.Array (Array, Ix)
import qualified Data.Array as Array

import Factor

import Debug.Trace (trace)

-- |Give factors in "reverse" order - the order they should be computed.
inference :: [Factor Prob Unnormalized]
          -> [Var]
          -> [Var]
          -> [(Var, Val)]
          -> Prob
inference factors query_vars hidden_vars evidence =
  let restricted_factors = map restrict_ factors
      restrict_ fact = foldl (\f (var, val) -> restrict f var val) fact evidence

      step :: [Var] -> [Factor Prob Unnormalized] -> Factor Prob Unnormalized
      step _ [] = undefined
      step [] [f] = f
      step [] (f1:f2:fs) = step [] $ multiply f1 f2 : fs
      step (v:vs) [f] = step vs [sumout f v]
      step (v:vs) (f1@(Factor vars1 _):f2@(Factor vars2 _):fs)
        | v `elem` vars1 && v `elem` vars2 = step (v:vs) (multiply f1 f2 : fs)
        | otherwise = step vs (multiply (sumout f1 v) (sumout f2 v) : fs)

      computed_fact = step hidden_vars restricted_factors
      (NormalizedFactor _ final_arr) = normalize computed_fact
  in foldl (+) 0 final_arr

-- TODO: 'DSL' for constructing a Bayes network, setting up tables, and inferencing.

