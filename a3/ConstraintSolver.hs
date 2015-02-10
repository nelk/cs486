{-#LANGUAGE MultiParamTypeClasses, FlexibleInstances, ScopedTypeVariables #-}
module ConstraintSolver
  ( solveConstraintProblem
  , Var(..)
  , varDomain
  , varDomainSize
  , varId
  , ConstraintProblem(..)
  , ConstraintSoln
  , Constraint(..)
  ) where

import qualified Data.Set as Set
import Control.Monad.Reader
import Data.List
import Data.Maybe (catMaybes)
import Data.Ord (comparing)
import Backtracking

import Debug.Trace

data (Eq id, Ord dom) =>
  Var id dom = AssignedVar id dom
             | UnassignedVar id (Set.Set dom)

instance (Show id, Show dom, Eq id, Ord dom)
      => Show (Var id dom) where
  show (AssignedVar ident dom) = "AssignedVar " ++ show ident ++ " " ++ show dom
  show (UnassignedVar ident domSet) = "UnassignedVar " ++ show ident ++ " " ++ show domSet

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


solveConstraintProblem :: (Show id, Show dom, Eq id, Ord dom, Constraint c id dom)
                       => ConstraintProblem c id dom
                       -> ConstraintSoln id dom
solveConstraintProblem prob
  = let startingPersistState = PersistState 0
        startingSolnState = SolnState $ initialVars prob
        initialChecking = forM_ (map varId $ filter varAssigned $ initialVars prob) forwardCheck
        comp = initialChecking >> solveWithBacktracking 0
        readerSoln = runBacktrackingT comp startingPersistState startingSolnState
        (soln_either, PersistState n, SolnState s) = runReader readerSoln prob
    in case soln_either of
        Left False -> (Nothing, n)
        Left True -> (Just s, n)
        Right _ -> (Nothing, n)


solveWithBacktracking :: (Show id, Show dom, Eq id, Ord dom, Constraint c id dom)
                      => Int -> Backtracker c id dom ()
solveWithBacktracking indent = do
  vs <- liftM vars getSoln
  -- Check if all assigned.
  --trace ("solveWithBacktracking " ++ show vs) $
  if all varAssigned vs
    then success
    else -- Select which variable to assign next using MRV heuristic.
      case minimumRemainingValues vs of
        [] -> failure
        mrvVars -> {- trace ("MRV " ++ show mrvVars) $ -} do
          chosenVar <- mostConstrainingVariable mrvVars
          -- Decide what to set for it.
          chosenValues <- leastConstrainingValue chosenVar
          --trace (replicate indent ' ' ++ "Choosing for " ++ show (varId chosenVar)) $ return ()
          forM_ chosenValues $ \val -> branch $ do
            --trace (replicate indent ' ' ++ "Assigning " ++ show val ++ " to " ++ show (varId chosenVar)) $ return ()
            assignVar (varId chosenVar) val
            incrementAssignments
            forwardCheck (varId chosenVar)
            --tmpvs <- liftM vars getSoln
            --trace (show tmpvs) $ return ()
            solveWithBacktracking (indent + 1)

assignVar :: (Show id, Show dom, Eq id, Ord dom, Constraint c id dom)
          => id -> dom -> Backtracker c id dom ()
assignVar vid val = do
  vs <- liftM vars getSoln
  when (length vs /= 81) $ error "FAIL2"
  let newVar = {- trace ("Assigning " ++ show val ++ " to " ++ show vid) $ -} AssignedVar vid val
      newVars = map (\v' -> if varId v' == vid
                              then newVar
                              else v'
                    ) vs
  when (length newVars /= 81) $ error "FAIL3"
  putSoln $ {- trace ("State after assigning: " ++ show newVars) $ -} SolnState newVars

incrementAssignments :: (Show id, Show dom, Eq id, Ord dom, Constraint c id dom)
                     => Backtracker c id dom ()
incrementAssignments = getPersistent >>= \(PersistState n) -> putPersistent (PersistState $ n + 1)


minimumRemainingValues :: (Eq id, Ord dom)
                       => [Var id dom] -> [Var id dom]
minimumRemainingValues = foldl foldFunc [] . filter (not . varAssigned)
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
                          (Show id, Show dom, Eq id, Ord dom, Constraint c id dom)
                       => Var id dom -> Backtracker c id dom [dom]
leastConstrainingValue v = do
  SolnState originalVs <- getSoln
  assignsAndNumRestricts :: [(dom, Int)] <- fmap catMaybes $ forM (Set.toList $ varDomain v) $ countRestrictions originalVs
  return $ map fst $ sortBy (comparing snd) assignsAndNumRestricts
    where countRestrictions :: [Var id dom] -> dom -> Backtracker c id dom (Maybe (dom, Int))
          countRestrictions originalVs d = branch $ do
            assignVar (varId v) d
            forwardCheck (varId v)
            SolnState vs <- getSoln
            return (d, sum (map varDomainSize vs) - sum (map varDomainSize originalVs))

forwardCheck :: (Show id, Show dom, Eq id, Ord dom, Constraint c id dom)
             => id -> Backtracker c id dom ()
forwardCheck vid = do
  cs <- liftM constraints $ lift ask
  vs <- liftM vars getSoln
  when (length vs /= 81) $ error "FAIL1"
  let var = head $ filter ((== vid) . varId) vs
  case foldl (restrictFolder var) (Just vs) cs of
    Nothing -> {- trace ("failure: no possible values for " ++ show vid) -} failure
    Just vs' -> {- trace ("forwardCheck " ++ show vs') $ -} putSoln (SolnState vs')
  where restrictFolder var maybe_vs c = do
          vs <- maybe_vs
          restrict c var vs

