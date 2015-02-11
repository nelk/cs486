{-#LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}
module SudokuSolver
  ( solveSudoku
  , sudoku
  , validateSudokuSoln
  , Cell
  , Digit(..)
  , Row(..)
  , Col(..)
  , SudokuVar
  , SudokuSoln
  , Var(..)
  , varId
  ) where

import qualified Data.HashSet as Set
import Data.Hashable
import Data.List (sortBy)
import Data.Ord (comparing)
import Control.Applicative
import Control.Monad (unless)
import Data.Maybe (isJust)
import ConstraintSolver

import Debug.Trace

-- |Digits, Row numbers, and Cell numbers are all domains of 9 elements.
data Digit = D1 | D2 | D3 | D4 | D5 | D6 | D7 | D8 | D9
  deriving (Show, Eq, Ord, Enum)
data Row = R1 | R2 | R3 | R4 | R5 | R6 | R7 | R8 | R9
  deriving (Show, Eq, Ord, Enum)
data Col = C1 | C2 | C3 | C4 | C5 | C6 | C7 | C8 | C9
  deriving (Show, Eq, Ord, Enum)

-- |A cell is a pair of a row and column number.
type Cell = (Row, Col)

-- All 81 cells in order.
allCells :: [Cell]
allCells = [(i, j) | i <- enumFromTo R1 R9, j <- enumFromTo C1 C9]

-- |Make these things hashable for use in HashSets.
instance Hashable Digit where
  hashWithSalt salt digit = hashWithSalt salt $ fromEnum digit
instance Hashable Row where
  hashWithSalt salt digit = hashWithSalt salt $ fromEnum digit
instance Hashable Col where
  hashWithSalt salt digit = hashWithSalt salt $ fromEnum digit

-- |Aliases for the Sudoku specialization of CSP vars and solution.
type SudokuVar = Var Cell Digit
type SudokuSoln = ConstraintSoln Cell Digit

-- |A Sudoku constraint can only be an 'Alldiff' constraint of a set of cells.
data SudokuConstraint = Alldiff (Set.HashSet Cell) deriving Show

-- |Make SudokuConstraint a CSP constraint.
instance Constraint SudokuConstraint Cell Digit where
  -- A variable is included in this constraint if its in the constraint's set.
  included (Alldiff cellSet) var
    = varId var `Set.member` cellSet

  -- Restricting variables based off of a constraint and newly assigned variable.
  restrict (Alldiff cellSet) (AssignedVar cell val) vs = mapM filterVariables vs -- Simply filter the domains of each variable.
    where -- Filter a variable by the constraint, if applicable.
          filterVariables v
            -- If the assigned variable is not part of this constraint, it doesn't restrict any variables.
            | not (cell `Set.member` cellSet) = Just v
            | varId v `Set.member` cellSet = filterAssignments v
            | otherwise = Just v
          -- Filter an assigned variable will fail unless it's for the same value/cell.
          filterAssignments v@(AssignedVar cell' val')
            | cell == cell' || val /= val' = Just v
            | otherwise = Nothing
          -- Filtering an unassigned variable deletes the appropriate value from its domain.
          filterAssignments v@(UnassignedVar cell' available)
            | cell == cell' = Just v
            -- If this is the last value in the variable's domain, we fail.
            | available == Set.singleton val = Nothing
            -- Remove the value from the available domain elements.
            | otherwise = let available' = val `Set.delete` available
                          in Just $ UnassignedVar cell' available'

  -- Internal problem - should never restrict using an unconstrained variable.
  restrict _ (UnassignedVar _ _) vs = trace "BAD" $ Just vs

-- |Helper function like 'zip', but that puts together values only if the
-- results from applying their respective viewing functions check equal.
-- The two lists must give their identities in order.
-- Ones without a match will still be returned, but with the other side as Nothing.
zipMatching :: Ord i => (a -> i) -> [a] -> (b -> i) -> [b] -> [(Maybe a, Maybe b)]
zipMatching _      []     _      []     = []
zipMatching identA []     identB (b:bs) = (Nothing, Just b):zipMatching identA [] identB bs
zipMatching identA (a:as) identB []     = (Just a, Nothing):zipMatching identA as identB []
zipMatching identA (a:as) identB (b:bs) = case compare (identA a) (identB b) of
  EQ -> (Just a, Just b):zipMatching identA as identB bs
  LT -> (Just a, Nothing):zipMatching identA as identB (b:bs)
  GT -> (Nothing, Just b):zipMatching identA (a:as) identB bs

-- |Helper to turn a starting list of assignments into a set of all sudoku variables.
makeVarsFromStartingState :: [(Cell, Digit)] -> [SudokuVar]
makeVarsFromStartingState startVals =
  let -- Make sure starting values are sorted.
      sortedStartVals = sortBy (comparing fst) startVals
      -- Zip the starting values with all cells.
      zippedStarting = zipMatching fst sortedStartVals id allCells
      -- Turns a possibly matched starting value into a Sudoku variable for a cell.
      makeVar (_, Nothing) = error "Got unknown cell as starting Val."
      makeVar (Nothing, Just cell) = UnassignedVar cell $ Set.fromList $ enumFromTo D1 D9
      makeVar (Just (_, v), Just cell) = AssignedVar cell v
  in map makeVar zippedStarting

-- |A static set of constraints for 9x9 Sudoku.
sudokuConstraints :: [SudokuConstraint]
sudokuConstraints =
  -- Each number in the same row is different.
  map
    (\i -> Alldiff $ Set.fromList [(i, j) | j <- enumFromTo C1 C9])
    (enumFromTo R1 R9)
  ++
  -- Each number in the same column is different.
  map
    (\j -> Alldiff $ Set.fromList [(i, j) | i <- enumFromTo R1 R9])
    (enumFromTo C1 C9)
  ++
  -- Each number in the same square is different.
  map
    (\(rowOff, colOff) -> Alldiff $ Set.fromList [(i, j) | i <- enumFromTo rowOff (succ $ succ rowOff), j <- enumFromTo colOff (succ $ succ colOff)])
    ((,) <$> [R1, R4, R7] <*> [C1, C4, C7])

-- |Create a Sudoku CSP given a starting assignment.
sudoku :: [(Cell, Digit)] -> CSP SudokuConstraint Cell Digit
sudoku startVals = CSP
  { initialVars = makeVarsFromStartingState startVals
  , constraints = sudokuConstraints
  , maxAssignments = Just 10000 -- Stop at 10000 assignments.
  }

-- |Solve a Sudoku CSP using the constraint solver.
solveSudoku :: CSP SudokuConstraint Cell Digit
            -> SudokuSoln
solveSudoku = solveConstraintProblem

-- |A helper to validate that the solution given is correct.
-- Soln and start needs to be sorted.
validateSudokuSoln :: CSP SudokuConstraint Cell Digit
                   -> [(Cell, Digit)]
                   -> [Var Cell Digit]
                   -> Bool
validateSudokuSoln prob start soln =
  let -- Zip the solution with the starting values, and ensure each starting value
      -- is still assigned to what it should have started as.
      startValsCorrect = all startValsChecker $ zipMatching fst start varId soln
      startValsChecker (Just (_, v), Just (AssignedVar _ v')) = v == v'
      startValsChecker _ = True

      -- Check that every Alldiff constraint is satisfied.
      constraintsCorrect = all constraintChecker $ constraints prob
      -- Check that a constraint is satisfied by keeping a set of available values and making sure no two variables take the same value from this set.
      constraintChecker (Alldiff idSet) = isJust $ foldl (availabilityFolder idSet) (Just $ Set.fromList $ enumFromTo D1 D9) soln
      -- For each variable, if it's constrained by this constraint, remove its assigned value from the remaining available assignments.
      -- If the value is not in the set, we fail.
      availabilityFolder idSet valSet_m (AssignedVar cell val)
        | not (cell `Set.member` idSet) = valSet_m
        | otherwise = do
            valSet <- valSet_m
            unless (val `Set.member` valSet) Nothing
            Just $ val `Set.delete` valSet
      -- An unassigned variable will fail this check.
      availabilityFolder _ _ _ = Nothing
  in startValsCorrect && constraintsCorrect

