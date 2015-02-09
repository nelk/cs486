{-#LANGUAGE MultiParamTypeClasses, FlexibleInstances, ScopedTypeVariables #-}
module ConstraintSolver where

import qualified Data.Set as Set
import Control.Monad.Reader
import Data.List
import Data.Maybe (catMaybes)
import Data.Ord (comparing)
import Backtracking

data (Eq id, Ord dom) =>
  Var id dom = AssignedVar id dom
             | UnassignedVar id (Set.Set dom)

varDomain :: (Eq id, Ord dom) => Var id dom -> Set.Set dom
varDomain (AssignedVar _ s) = Set.singleton s
varDomain (UnassignedVar _ s) = s

varDomainSize :: (Eq id, Ord dom) => Var id dom -> Int
varDomainSize = Set.size . varDomain

varId :: (Eq id, Ord dom) => Var id dom -> id
varId (AssignedVar ident _) = ident
varId (UnassignedVar ident _) = ident

varAssigned :: (Eq id, Ord dom) => Var id dom -> Bool
varAssigned (AssignedVar _ _) = True
varAssigned (UnassignedVar _ _) = False

class Constraint c id dom where
  included :: c -> Var id dom -> Bool
  restrict :: c -> Var id dom -> [Var id dom] -> Maybe [Var id dom]

data Constraint c id dom => ConstraintProblem c id dom = ConstraintProblem
  { initialVars :: [Var id dom]
  , constraints :: [c]
  }

type ConstraintSoln id dom = (Maybe [Var id dom], Int)

data PersistState = PersistState
  { numAssignments :: Int
  }

data SolnState c id dom = SolnState
  { vars :: [Var id dom]
  }

type Backtracker c id dom = BacktrackingT PersistState (SolnState c id dom) (Reader (ConstraintProblem c id dom))


solveConstraintProblem :: (Eq id, Ord dom, Constraint c id dom)
                       => ConstraintProblem c id dom
                       -> ConstraintSoln id dom
solveConstraintProblem prob
  = let startingPersistState = PersistState 0
        startingSolnState = SolnState $ initialVars prob
        readerSoln = runBacktrackingT solveWithBacktracking startingPersistState startingSolnState
        (soln_either, PersistState n, SolnState s) = runReader readerSoln prob
    in case soln_either of
        Left False -> (Nothing, n)
        Left True -> (Just s, n)
        Right _ -> (Nothing, n)


solveWithBacktracking :: (Eq id, Ord dom, Constraint c id dom)
                      => Backtracker c id dom ()
solveWithBacktracking = do
  vs <- liftM vars getSoln
  -- Check if all assigned.
  if all varAssigned vs
    then success
    else -- Select which variable to assign next using MRV heuristic.
      case minimumRemainingValues vs of
        [] -> failure
        mrvVars -> do
          chosenVar <- mostConstrainingVariable mrvVars
          -- Decide what to set for it.
          chosenValues <- leastConstrainingValue chosenVar
          forM_ chosenValues $ \val -> do
            assignVar chosenVar val
            forwardCheck chosenVar
            solveWithBacktracking

assignVar :: (Eq id, Ord dom, Constraint c id dom)
          => Var id dom -> dom -> Backtracker c id dom ()
assignVar v val = do
  vs <- liftM vars getSoln
  let otherVars = filter (\v' -> varId v' /= varId v) vs
      newVar = AssignedVar (varId v) val
      newVars = newVar:otherVars
  putSoln $ SolnState newVars
  getPersistent >>= \(PersistState n) -> putPersistent (PersistState $ n + 1)


minimumRemainingValues :: (Eq id, Ord dom)
                       => [Var id dom] -> [Var id dom]
minimumRemainingValues = foldl foldFunc [] . filter ((> 1) . varDomainSize)
  where foldFunc [] newVar = [newVar]
        foldFunc oldVars@(oldVar:_) newVar
          = case comparing varDomainSize oldVar newVar of
              GT -> [newVar]
              LT -> oldVars
              EQ -> newVar:oldVars

mostConstrainingVariable :: (Eq id, Ord dom, Constraint c id dom)
                         => [Var id dom]
                         -> Backtracker c id dom (Var id dom)
mostConstrainingVariable [v] = return v
mostConstrainingVariable vs = do
  cs <- liftM constraints $ lift ask
  return $ maximumBy (comparing $ countConstraints cs) vs
    where countConstraints cs v = length $ filter id $ map (`included` v) cs


leastConstrainingValue :: forall c id dom.
                          (Eq id, Ord dom, Constraint c id dom)
                       => Var id dom -> Backtracker c id dom [dom]
leastConstrainingValue v = do
  SolnState originalVs <- getSoln
  assignsAndNumRestricts :: [(dom, Int)] <- fmap catMaybes $ forM (Set.toList $ varDomain v) $ countRestrictions originalVs
  return $ map fst $ sortBy (comparing snd) assignsAndNumRestricts
    where countRestrictions :: [Var id dom] -> dom -> Backtracker c id dom (Maybe (dom, Int))
          countRestrictions originalVs d = branch $ do
            assignVar v d
            forwardCheck v
            SolnState vs <- getSoln
            return (d, sum (map varDomainSize vs) - sum (map varDomainSize originalVs))

forwardCheck :: (Eq id, Ord dom, Constraint c id dom)
             => Var id dom -> Backtracker c id dom ()
forwardCheck var = do
  cs <- liftM constraints $ lift ask
  vs <- liftM vars getSoln
  case foldl restrictFolder (Just vs) cs of
    Nothing -> failure
    Just vs' -> putSoln (SolnState vs')
  where restrictFolder maybe_vs c = do
          vs <- maybe_vs
          restrict c var vs






