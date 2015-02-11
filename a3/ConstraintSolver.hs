{-#LANGUAGE MultiParamTypeClasses, FlexibleInstances, ScopedTypeVariables, ConstraintKinds, UndecidableInstances #-}
module ConstraintSolver
  ( solveConstraintProblem
  , Var(..)
  , varDomain
  , varDomainSize
  , varId
  , CSP(..)
  , VarTypes
  , CSPTypes
  , ConstraintSoln
  , Constraint(..)
  ) where

import qualified Data.HashSet as Set
import Control.Monad.Reader
import Data.List
import Data.Maybe (catMaybes)
import Data.Ord (comparing)
import Data.Hashable
import Backtracking

-- |A variable parameterized by an identifier type and a domain type.
-- It is either assigned to something in the domain, or unassigned
-- and restricted to a set of values in the domain.
data VarTypes id dom =>
  Var id dom = AssignedVar id dom
             | UnassignedVar id (Set.HashSet dom)

-- |Type constraints for variable type parameters.
type VarTypes id dom = (Show id, Eq id, Show dom, Ord dom, Hashable dom)

-- |How to turn a Variable into a human-readable string, for debugging.
instance VarTypes id dom => Show (Var id dom) where
  show (AssignedVar ident dom) = "AssignedVar " ++ show ident ++ " " ++ show dom
  show (UnassignedVar ident domSet) = "UnassignedVar " ++ show ident ++ " " ++ show domSet

-- |Get the possible domain values this variable can be assigned to.
varDomain :: VarTypes id dom => Var id dom -> Set.HashSet dom
varDomain (AssignedVar _ s) = Set.singleton s
varDomain (UnassignedVar _ s) = s

-- |How many different domain values this variable could be assigned to.
varDomainSize :: VarTypes id dom => Var id dom -> Int
varDomainSize = Set.size . varDomain

-- |Get the variable's name.
varId :: VarTypes id dom => Var id dom -> id
varId (AssignedVar ident _) = ident
varId (UnassignedVar ident _) = ident

-- |Whether the variable assigned already.
varAssigned :: VarTypes id dom => Var id dom -> Bool
varAssigned (AssignedVar _ _) = True
varAssigned (UnassignedVar _ _) = False

-- |A class of types that represent constraints on variables.
class VarTypes id dom => Constraint c id dom where
  -- Whether a given variable constrainted by this constraint.
  included :: c -> Var id dom -> Bool

  -- Given a constraint, a variable that has changed, and the list of all current variables,
  -- it either returns Nothing if the assignment causes another variable to have no possible value,
  -- or Just a list variables that is arc consistent with respect to the changed variable.
  -- This leaves unassigned variables unassigned.
  restrict :: c -> Var id dom -> [Var id dom] -> Maybe [Var id dom]

-- |The definition of a constraint satisfaction problem. It contains a list of variables,
-- some of which may begin assigned, a list of constraints on these variables,
-- and possibly a maximum number of assignments to search for before failing.
data CSPTypes id dom c => CSP c id dom = CSP
  { initialVars :: [Var id dom]
  , constraints :: [c]
  , maxAssignments :: Maybe Int
  }

-- |Type constraints for CSP type parameters.
type CSPTypes id dom c = (VarTypes id dom, Constraint c id dom)

-- |A constraint solution. It could be a solution of assigned variables, or no solution.
-- It includes the number of assignments that were done (including backtracking).
type ConstraintSoln id dom = (Maybe [Var id dom], Int)

-- |The persistent state when backtracking, which is just the total number of assignments made.
data PersistState = PersistState Int

-- |The persistent state when backtracking, which is just the total number of assignments made.
data SolnState c id dom = SolnState [Var id dom]

-- |Alias for the BacktrackingT type used for CSP solving.
type Backtracker c id dom = BacktrackingT PersistState (SolnState c id dom) (Reader (CSP c id dom))

-- |Solve a given CSP.
solveConstraintProblem :: CSPTypes id dom c
                       => CSP c id dom
                       -> ConstraintSoln id dom
solveConstraintProblem prob
  = let startingPersistState = PersistState 0
        startingSolnState = SolnState $ initialVars prob
        -- Perform forward checking for all pre-assigned variables.
        initialChecking = forM_ (map varId $ filter varAssigned $ initialVars prob) forwardCheck
        -- The backtracking computation. It's composed of initial forward checking and then solving.
        comp = initialChecking >> solveWithBacktracking
        -- Result of running the backtracking computation (a function that requires the constraint problem).
        readerSoln = runBacktrackingT comp startingPersistState startingSolnState
        -- Result of running the problem-context-requiring computation.
        (soln_either, PersistState n, SolnState s) = runReader readerSoln prob
    in case soln_either of
        Left False -> (Nothing, n)
        Left True -> (Just s, n)
        Right _ -> (Nothing, n)

-- |The main constraint solving algorithm, as a backtracking computation.
solveWithBacktracking :: CSPTypes id dom c
                      => Backtracker c id dom ()
solveWithBacktracking = do
  -- Get the current variable assignments.
  SolnState vs <- getSoln
  -- If all variables are assigned, we are done!
  if all varAssigned vs
    then success
    else -- Select which variable to assign next using MRV heuristic.
      case minimumRemainingValues vs of
        -- Shouldn't occur - no unnassigned variables.
        [] -> failure
        mrvVars -> do
          -- We have possibly more than one variable with the same minimum
          --  number of remaining values, so use the most constraining variable
          --  heuritic to select one.
          chosenVar <- mostConstrainingVariable mrvVars
          -- Use the least constraining value heuristic to order the values we
          --  want to try to assign to the chosen variable.
          chosenValues <- leastConstrainingValue chosenVar

          -- For each value, try to assign it and then recurse for the next variable.
          forM_ chosenValues $ \val -> branch $ do
            -- Assign the chosen variable to the value for this iteration.
            assignVar (varId chosenVar) val
            -- Permamently increment the number of assignments.
            incrementAssignments
            -- Forward check for this new assignment.
            forwardCheck (varId chosenVar)
            -- Recurse this computation.
            solveWithBacktracking

-- |Assign the variable with the given name the given value.
-- This doesn't increment the number of assignments, as some
-- assignments can be temporary when used in heuristics.
assignVar :: CSPTypes id dom c
          => id -> dom -> Backtracker c id dom ()
assignVar vid val = do
  -- Get current assignments.
  SolnState vs <- getSoln
  let newVar = AssignedVar vid val
      -- Replace the named variable and leave the others the same.
      newVars = map (\v' -> if varId v' == vid
                              then newVar
                              else v'
                    ) vs
  -- Write the changed solution state.
  putSoln $ SolnState newVars

-- |Permamently increment the number of assignments used, and fail if
-- there is a maximum number of assignments set and we've crossed it.
incrementAssignments :: CSPTypes id dom c
                     => Backtracker c id dom ()
incrementAssignments = do
  -- Get persistent state.
  PersistState n <- getPersistent
  let n' = n + 1
  -- Write incremented persistent state.
  putPersistent $ PersistState n'
  -- Get maximum assignments setting from problem definition.
  maxN_m <- liftM maxAssignments $ lift ask
  case maxN_m of
    Nothing -> return ()
    -- Fail if we have a maximum and we've reached it.
    Just maxN -> when (n' >= maxN) failure

-- |MRV heuristic. It returns one or more unassigned variables with
-- the smallest number of remaining possible assignments in their domain.
minimumRemainingValues :: VarTypes id dom
                       => [Var id dom] -> [Var id dom]
minimumRemainingValues = foldl foldFunc [] . filter (not . varAssigned)
  where foldFunc [] newVar = [newVar]
        foldFunc oldVars@(oldVar:_) newVar
          = case comparing varDomainSize oldVar newVar of
              -- If older variable has more than new variable, throw away
              -- old list and start with newer minimal variable.
              GT -> [newVar]
              -- If newer variable has more than old variable, ignore it.
              LT -> oldVars
              -- If newer variable has same number, add it to list of minimal.
              EQ -> newVar:oldVars

-- |Most Constraining Variable heuristic.
-- Return the variable that is in the most constraints.
mostConstrainingVariable :: CSPTypes id dom c
                         => [Var id dom]
                         -> Backtracker c id dom (Var id dom)
mostConstrainingVariable [v] = return v
mostConstrainingVariable vs = do
  cs <- liftM constraints $ lift ask
  return $ maximumBy (comparing $ countConstraints cs) vs
    where countConstraints cs v = length $ filter id $ map (`included` v) cs

-- |Least Constraining Value heuristic for choosing a value for a variable.
-- Order the possible assignments for a given variable by which assignment
-- would result in the smallest number of removed options from the domains
-- of other variables when making them arc consistent to this variable and assignment.
leastConstrainingValue :: forall c id dom. CSPTypes id dom c
                       => Var id dom -> Backtracker c id dom [dom]
leastConstrainingValue v = do
  -- Original variables.
  SolnState originalVs <- getSoln
  -- Count the number of restrictions caused by each assignment that didn't result in a failed solution state.
  assignsAndNumRestricts :: [(dom, Int)] <- fmap catMaybes $ forM (Set.toList $ varDomain v) $ countRestrictions originalVs
  -- Sort the assignment values by ascending number of restrictions they would cause to other variables.
  return $ map fst $ sortBy (comparing snd) assignsAndNumRestricts
    where -- To count restrictions, just branch this backtracking computation and try to assign
          -- and forward check the assignment.
          countRestrictions :: [Var id dom] -> dom -> Backtracker c id dom (Maybe (dom, Int))
          countRestrictions originalVs d = branch $ do
            assignVar (varId v) d
            forwardCheck (varId v)
            -- Get new variable state after.
            SolnState vs <- getSoln
            -- Return the difference in sums of domain sizes.
            return (d, sum (map varDomainSize vs) - sum (map varDomainSize originalVs))

-- |Forward check for the newly assigned variable of the given name.
-- It will either leave all other variables arc-consistent to this one
-- or fail the computation if any variable becomes unassignable.
forwardCheck :: forall id dom c. CSPTypes id dom c
             => id -> Backtracker c id dom ()
forwardCheck vid = do
  -- Get problem constriants and current variables.
  cs <- liftM constraints $ lift ask
  SolnState vs <- getSoln
  -- Find the named variable.
  let var = head $ filter ((== vid) . varId) vs
  -- Apply restrict sequentially on the variables for each constraint.
  case foldl (restrictFolder var) (Just vs) cs of
    -- Fail if restrict failed at any point.
    Nothing -> failure
    -- Write the new variable assignments otherwise.
    Just vs' -> putSoln (SolnState vs')
  where restrictFolder var maybe_vs c = do
          vs <- maybe_vs
          restrict c var vs

