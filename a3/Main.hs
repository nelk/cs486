{-#LANGUAGE ScopedTypeVariables #-}
module Main where

import System.IO (hPutStrLn, stderr)
import System.Environment (getArgs)
import System.Exit
import Control.Monad (unless, liftM, zipWithM)
import Data.List (sortBy)
import Data.Ord (comparing)
import Data.Maybe (catMaybes)
import SudokuSolver

exitError :: String -> IO a
exitError msg = do
  hPutStrLn stderr msg
  exitWith $ ExitFailure 1

parseSudoku :: String -> Either String [(Cell, Digit)]
parseSudoku s = do
  let rows = lines s
  unless (length rows == 10 && null (last rows)) $ Left "There need to be 9 rows in the given file."
  unless (all (\r -> length r == 18 && last r == ' ') $ init rows) $ Left "There need to be 9 columns in every row."
  liftM (catMaybes . concat) $ zipWithM parseRow (enumFromTo R1 R9) rows

  where parseRow :: Row -> String -> Either String [Maybe (Cell, Digit)]
        parseRow i row = zipWithM (parseCell i) (enumFromTo C1 C9) $ words row
        parseCell :: Row -> Col -> String -> Either String (Maybe (Cell, Digit))
        parseCell _ _ "0" = Right Nothing
        parseCell i j v
          | fromEnum D1 + 1<= read v && read v <= fromEnum D9 + 1
            = Right $ Just ((i, j), toEnum $ read v - 1)
          | otherwise
            = Left $ "Value \"" ++ v ++ "\" at " ++ show (i, j) ++ " needs to be between 1 and 9."

-- |Exit the program with fail status after printing an error message.
main :: IO ()
main = do
  args <- getArgs
  unless (length args == 1) $ exitError "Usage: ./sudoku-solver problems/16/1.sd"
  rawStart <- readFile $ head args
  case parseSudoku rawStart of
    Left e -> exitError e
    Right start ->
      let sudokuProb = sudoku start
      in case solveSudoku sudokuProb of
          (Nothing, n) -> exitError $ "Failed to find solution after " ++ show n ++ " variable assignments."
          (Just soln, n) -> do
            putStrLn $ prettyPrintSoln soln n
            unless (validateSudokuSoln sudokuProb start soln) $ exitError "Solution is INVALID."

splitEvery :: Int -> [a] -> [[a]]
splitEvery _ [] = []
splitEvery n as = let (front, back) = splitAt n as
                  in front:splitEvery n back

prettyPrintSoln :: [SudokuVar] -> Int -> String
prettyPrintSoln soln n =
  let sortedSoln = sortBy (comparing varId) soln
      rows = splitEvery 9 sortedSoln
      showVar (AssignedVar _ v) = show $ fromEnum v + 1
      showVar _ = error "Unassigned var in solution!"
      pretty = concatMap ((++ "\n") . concatMap ((++ " ") . showVar)) rows
  in pretty ++ "\nUsed " ++ show n ++ " variable assignments."


