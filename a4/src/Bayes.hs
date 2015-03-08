module Bayes where

import Prelude hiding ((^), sum)
import Data.Foldable (sum)
import qualified Data.Array as Array
import Data.List (sort, sortBy, groupBy, elemIndex, intercalate, intersperse)
import Data.Ord (comparing)
import Control.Monad.Writer
import Control.Monad.Reader
import Text.Printf (printf)

import Debug.Trace (trace)

import Factor

data FactorOp = Init | Sumout | Multiply | Restrict | Normalize deriving Show
type VarNameFn = Var -> String
type FactorRenderFn = Reader VarNameFn String
type FactorLog = [FactorRenderFn]

renderFactor :: [Factor Prob] -> Factor Prob -> FactorOp -> Maybe Var -> FactorRenderFn
renderFactor source_facts (Factor vars arr) op m_var = ask >>= \varName -> return $ snd $ runWriter $ do
  let rendF :: [Var] -> Writer String ()
      rendF vs = do
        tell "F("
        tell $ intercalate ", " $ map varName vs
        tell ")"

  sequence_ $ intersperse (tell ", ") $ map (\(Factor vs _) -> rendF vs) source_facts
  tell $ " -> " ++ show op
  maybe (return ()) (tell . (" " ++ ) . varName) m_var
  tell " -> "
  rendF vars
  tell "\n"

  let assocs = Array.assocs arr

      render_assig :: Var -> Val -> String
      render_assig var True = varName var
      render_assig var False = "~" ++ varName var

      render_assoc :: [Var] -> ([Val], Prob) -> Writer String ()
      render_assoc vs (vals, p) = do
        tell "F("
        tell $ intercalate ", " $ zipWith render_assig vs vals
        tell $ ") = " ++ printf "%f" p ++ "\n"
  mapM_ (render_assoc vars) assocs

-- |Give factors in "reverse" order - the order they should be computed.
inference :: [Factor Prob]
          -> [(Var, Val)]
          -> [Var]
          -> [(Var, Val)]
          -> (Prob, FactorLog)
inference factors query_vars hidden_vars evidence = runWriter $ do
  let restrict_ :: [(Var, Val)] -> Factor Prob -> Writer FactorLog (Factor Prob)
      restrict_ evd fact = foldM (\f (var, val) -> do
          let new_f = restrict f var val
          tell [renderFactor [f] new_f Restrict (Just var)]
          return new_f
        ) fact evd

  restricted_factors <- mapM (restrict_ evidence) factors

  let hasVar :: Var -> [Factor Prob] -> Bool
      hasVar v = any (\(Factor vs _) -> v `elem` vs)

      step :: [Var] -> [Factor Prob] -> Writer FactorLog (Factor Prob)
      step _ [] = return undefined
      step [] [f] = return f
      step [] (f1:f2:fs) = do
        let new_f = multiply f1 f2
        tell [renderFactor [f1, f2] new_f Multiply Nothing]
        step [] $ new_f : fs
      step (v:vs) (f:fs)
        -- Variable gone, remove.
        | not (hasVar v (f:fs)) = step vs (f:fs)
        -- Variable only in first factor, sumout.
        | not (hasVar v fs) = do
          let new_f = sumout f v
          tell [renderFactor [f] new_f Sumout (Just v)]
          step vs (new_f:fs)
        -- Last factor - return it.
        | null fs = return f
        -- Else multiply.
        | otherwise = do
          let f2 = head fs
              new_f = multiply f f2
          tell [renderFactor [f, f2] new_f Multiply Nothing]
          step (v:vs) $ new_f:tail fs

  computed_fact <- step hidden_vars restricted_factors

{-
  -- Do all mults then all sums.
  all_multiplied <- foldM (\f1 f2 -> do
                            let new_f = multiply f1 f2
                            tell [renderFactor [f1, f2] new_f Multiply Nothing]
                            return new_f
                         ) (head restricted_factors) (tail restricted_factors)
  computed_fact <- foldM (\f' v -> do
                           let new_f = sumout f' v
                           tell [renderFactor [f'] new_f Sumout (Just v)]
                           return new_f
                         ) all_multiplied hidden_vars
-}

  let normalized_fact = normalize computed_fact
  tell [renderFactor [computed_fact] normalized_fact Normalize Nothing]

  -- Restrict by each query var.
  (Factor _ final_arr) <- restrict_ query_vars normalized_fact
  return $ sum final_arr

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

compute :: [BayesTerm] -> P -> [P] -> [Var] -> (Prob, FactorLog)
compute terms (P (BayesAssgs query)) factor_order hidden_vars =
  compute terms (P (BayesCombinedAssgs query [])) factor_order hidden_vars
compute terms (P (BayesCombinedAssgs query evidence)) factor_order hidden_vars =
  let sorted_evidence = sortBy (comparing fst) evidence
      sorted_query = sortBy (comparing fst) query

      factors :: [Factor Prob]
      factors = factorize terms

      factor_order_assigs :: [[Var]]
      factor_order_assigs = map (\(P (allAssigs -> assgs)) -> sort $ map fst assgs) factor_order

      -- Sort and reverse.
      filtered_factors = filter (\(Factor vars _) -> vars `elem` factor_order_assigs) factors
      sorted_factors = sortBy (flip (comparing $ \(Factor vars _) -> vars `elemIndex` factor_order_assigs)) filtered_factors
  in inference sorted_factors sorted_query hidden_vars sorted_evidence

factorize :: [BayesTerm] -> [Factor Prob]
factorize terms = let sorted_terms = map sort_assgs terms
                      sort_assgs :: BayesTerm -> BayesTerm
                      sort_assgs (BayesTerm (allAssigs -> assgs)  p) = BayesTerm (BayesAssgs $ sortBy (comparing fst) assgs) p

                      grouped_assgs = groupBy is_same_factor sorted_terms

                      is_same_factor :: BayesTerm -> BayesTerm -> Bool
                      is_same_factor (BayesTerm (allAssigs -> assgs1) _) (BayesTerm (allAssigs -> assgs2) _) =
                        map fst assgs1 == map fst assgs2

                      make_factor :: [BayesTerm] -> Factor Prob
                      make_factor [] = trace "Tried to make empty factor!" undefined
                      make_factor terms' =
                        let (BayesTerm (allAssigs -> head_assigs) _) = head terms'
                            vars = map fst head_assigs
                            make_assoc :: BayesTerm -> ([Val], Prob)
                            make_assoc (BayesTerm (allAssigs -> assgs) p) = (map snd assgs, p)
                            assocs = map make_assoc terms'
                        in Factor vars $ Array.array (makeValRange $ length vars)  assocs

                  in map make_factor grouped_assgs

