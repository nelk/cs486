{-#LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}
module SudokuSolver where

import qualified Data.Set as Set
import Data.List (sortBy)
import Data.Ord (comparing)
import Control.Applicative
import ConstraintSolver

data Digit = D1 | D2 | D3 | D4 | D5 | D6 | D7 | D8 | D9
  deriving (Show, Eq, Ord, Enum)
data Row = R1 | R2 | R3 | R4 | R5 | R6 | R7 | R8 | R9
  deriving (Show, Eq, Ord, Enum)
data Col = C1 | C2 | C3 | C4 | C5 | C6 | C7 | C8 | C9
  deriving (Show, Eq, Ord, Enum)
type Cell = (Row, Col)

type SudokuVar = Var Cell Digit

data SudokuConstraint = Alldiff (Set.Set Cell)
instance Constraint SudokuConstraint Cell Digit where
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

zipMatching :: Ord i => (a -> i) -> [a] -> (b -> i) -> [b] -> [(Maybe a, Maybe b)]
zipMatching _      []     _      []     = []
zipMatching identA []     identB (b:bs) = (Nothing, Just b):zipMatching identA [] identB bs
zipMatching identA (a:as) identB []     = (Just a, Nothing):zipMatching identA as identB []
zipMatching identA (a:as) identB (b:bs) = case compare (identA a) (identB b) of
  EQ -> (Just a, Just b):zipMatching identA as identB bs
  LT -> (Just a, Nothing):zipMatching identA as identB (b:bs)
  GT -> (Nothing, Just b):zipMatching identA (a:as) identB bs

makeVarsFromStartingState :: [(Cell, Digit)] -> [Var Cell Digit]
makeVarsFromStartingState startVals =
  let sortedStartVals = sortBy (comparing fst) startVals
      allCells = [(i, j) | i <- enumFromTo R1 R9, j <- enumFromTo C1 C9]
      zippedStarting = zipMatching fst sortedStartVals id allCells
      makeVar (_, Nothing) = error "Got unknown cell as starting Val."
      makeVar (Nothing, Just cell) = UnassignedVar cell $ Set.fromList $ enumFromTo D1 D9
      makeVar (Just (_, v), Just cell) = AssignedVar cell v
  in map makeVar zippedStarting

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



sudoku :: [(Cell, Digit)] -> ConstraintProblem SudokuConstraint Cell Digit
sudoku startVals = ConstraintProblem
  { initialVars = makeVarsFromStartingState startVals
  , constraints = sudokuConstraints
  }

