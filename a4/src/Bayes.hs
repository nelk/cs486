module Bayes where

import Prelude hiding ((^), sum)
import Data.Foldable (sum)
import qualified Data.Array as Array
import Data.List (sort, sortBy, groupBy, elemIndex)
import Data.Ord (comparing)

import Debug.Trace (trace)

import Factor

-- |Give factors in "reverse" order - the order they should be computed.
inference :: [Factor Prob Unnormalized]
          -> [(Var, Val)]
          -> [Var]
          -> [(Var, Val)]
          -> Prob
inference factors query_vars hidden_vars evidence =
  let restricted_factors = traceShow "Restricted: " $ map (restrict_ evidence) factors

      restrict_ :: [(Var, Val)] -> Factor Prob 'Unnormalized -> Factor Prob 'Unnormalized
      restrict_ evd fact = foldl (\f (var, val) -> restrict f var val) fact evd

      -- TODO: Output factors after each step.

      step :: [Var] -> [Factor Prob Unnormalized] -> Factor Prob Unnormalized
      step _ [] = undefined
      step [] [f] = f
      step [] (f1:f2:fs) = step [] $ multiply f1 f2 : fs
      step (v:vs) [f] = step vs [sumout f v]
      step (v:vs) (f1@(Factor vars1 _):f2@(Factor vars2 _):fs)
        -- Both inside next sum - multiply.
        | v `elem` vars1 && v `elem` vars2 = step (v:vs) (multiply f1 f2 : fs)
        -- First inside next sum - sumout and multiply.
        | v `elem` vars1 = step vs (multiply (sumout f1 v) f2 : fs)
        -- Both outside - drop v and multiply
        | otherwise = step vs (multiply f1 f2 : fs)

      computed_fact = traceShow "Computed: " $ step hidden_vars restricted_factors

      -- TODO: Don't do this?
      --all_summed_out = map (\f -> foldl sumout f hidden_vars) restricted_factors
      --computed_fact = traceShow "Computed: " $ foldl1 multiply all_summed_out

      normalized_fact = traceShow "Normalized: " $ normalize computed_fact

      -- Restrict by each query var.
      (Factor _ final_arr) = restrict_ query_vars (toUnnormalized normalized_fact)
  in sum final_arr

-- |DSL for building factors.
data BayesAssgs = BayesAssgs [(Var, Val)]
                | BayesCombinedAssgs [(Var, Val)] [(Var, Val)]
data P = P BayesAssgs
data BayesTerm = BayesTerm BayesAssgs Prob

allAssigs :: BayesAssgs -> [(Var, Val)]
allAssigs (BayesAssgs assigs) = assigs
allAssigs (BayesCombinedAssgs assigs assigs') = assigs ++ assigs'

infixl 2 .|.
infixl 3 ^
infixr 4 .=

instance Num BayesAssgs where
  fromInteger var = BayesAssgs [(fromInteger var, True)]
  negate (BayesAssgs [(var, val)]) = BayesAssgs [(var, not val)]

  negate _ = trace "Tried to negate multiple variables at once." undefined
  (+) = trace "Operator + attempted on BayesAssgs." undefined
  (*) = trace "Operator * attempted on BayesAssgs." undefined
  abs = trace "Function abs attempted on BayesAssgs." undefined
  signum = trace "Function signum attempted on BayesAssgs." undefined

getVar :: BayesAssgs -> Var
getVar (BayesAssgs ((var, _):_)) = var
getVar (BayesCombinedAssgs ((var, _):_) _) = var
getVar _ = undefined

(.|.) :: BayesAssgs -> BayesAssgs -> BayesAssgs
(BayesAssgs assgs) .|. (BayesAssgs assgs') = BayesCombinedAssgs assgs assgs'
_ .|. _ = trace "Tried to .|. multiple times." undefined

(^) :: BayesAssgs -> BayesAssgs -> BayesAssgs
(BayesAssgs assgs) ^  (BayesAssgs assgs') = BayesAssgs $ assgs ++ assgs'
_ ^ _ = trace "Tried to ^ on results of .|." undefined

(.=) :: P -> Prob -> BayesTerm
(P assgs) .= p = BayesTerm assgs p

compute :: [BayesTerm] -> P -> [P] -> [Var] -> Prob
compute terms (P (BayesAssgs query)) factor_order hidden_vars =
  compute terms (P (BayesCombinedAssgs query [])) factor_order hidden_vars
compute terms (P (BayesCombinedAssgs query evidence)) factor_order hidden_vars =
  let sorted_evidence = sortBy (comparing fst) evidence
      sorted_query = sortBy (comparing fst) query

      factors :: [Factor Prob 'Unnormalized]
      factors = factorize terms

      factor_order_assigs :: [[Var]]
      factor_order_assigs = map (\(P (allAssigs -> assgs)) -> sort $ map fst assgs) factor_order

      -- Sort and reverse.
      filtered_factors = filter (\(Factor vars _) -> vars `elem` factor_order_assigs) factors
      sorted_factors = sortBy (flip (comparing $ \(Factor vars _) -> vars `elemIndex` factor_order_assigs)) filtered_factors
  in inference sorted_factors sorted_query hidden_vars sorted_evidence

factorize :: [BayesTerm] -> [Factor Prob 'Unnormalized]
factorize terms = let sorted_terms = map sort_assgs terms
                      sort_assgs :: BayesTerm -> BayesTerm
                      sort_assgs (BayesTerm (allAssigs -> assgs)  p) = BayesTerm (BayesAssgs $ sortBy (comparing fst) assgs) p

                      grouped_assgs = groupBy is_same_factor sorted_terms

                      is_same_factor :: BayesTerm -> BayesTerm -> Bool
                      is_same_factor (BayesTerm (allAssigs -> assgs1) _) (BayesTerm (allAssigs -> assgs2) _) =
                        map fst assgs1 == map fst assgs2

                      make_factor :: [BayesTerm] -> Factor Prob 'Unnormalized
                      make_factor [] = trace "Tried to make empty factor!" undefined
                      make_factor terms' =
                        let (BayesTerm (allAssigs -> head_assigs) _) = head terms'
                            vars = map fst head_assigs
                            make_assoc :: BayesTerm -> ([Val], Prob)
                            make_assoc (BayesTerm (allAssigs -> assgs) p) = (map snd assgs, p)
                            assocs = map make_assoc terms'
                        in Factor vars $ Array.array (makeValRange $ length vars)  assocs

                  in map make_factor grouped_assgs

