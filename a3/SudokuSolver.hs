{-#LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}
module SudokuSolver where

import qualified Data.Set as Set
import ConstraintSolver

data Digits = D1 | D2 | D3 | D4 | D5 | D6 | D7 | D8 | D9
  deriving (Show, Eq, Ord, Enum)
data Row = R1 | R2 | R3 | R4 | R5 | R6 | R7 | R8 | R9
  deriving (Show, Eq, Ord, Enum)
data Col = C1 | C2 | C3 | C4 | C5 | C6 | C7 | C8 | C9
  deriving (Show, Eq, Ord, Enum)
type Cell = (Row, Col)

type SudokuVar = Var Cell Digits

data SudokuConstraint = Alldiff (Set.Set Cell)
instance Constraint SudokuConstraint Cell Digits where
  included (Alldiff cellSet) var
    = varId var `Set.member` cellSet

  restrict (Alldiff cellSet) (AssignedVar cell val) vs = mapM filterVariables vs
    where filterVariables v
            | varId v `Set.member` cellSet = filterAssignments v
            | otherwise = Just v
          filterAssignments v@(AssignedVar cell' val')
            | cell == cell' || val == val' = Just v
            | otherwise = Nothing
          filterAssignments v@(UnassignedVar cell' available)
            | cell == cell' = Just v
            | available == Set.singleton val = Nothing
            | otherwise = let available' = val `Set.delete` available
                          in Just $ if Set.size available' == 1
                                      then AssignedVar cell (head $ Set.toList available')
                                      else UnassignedVar cell available'

  restrict _ (UnassignedVar _ _) vs = Just vs

sudoku :: ConstraintProblem SudokuConstraint Cell Digits
sudoku = ConstraintProblem
  { vars = []
  , constraints = []
  }

