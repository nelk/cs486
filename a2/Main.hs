{-#LANGUAGE MultiParamTypeClasses, FlexibleInstances, ScopedTypeVariables #-}
module Main where

import Prelude
import System.IO (hPutStrLn, stderr)
import System.Environment (getArgs)
import System.Exit
import Control.Monad
import Text.Read hiding (lift, get)
import Safe
import qualified System.Random as Random

import qualified Search
import TSP
import TSPAStar
import TSPSimulatedAnnealing

data RunMode = AStarSimple | AStarMaxTriangle | AStarMST | SALinear | SALog | SAWavy

-- Note: Returns path of constructing tours and each tour in reverse.
solveTSP :: RunMode -> TSP -> Random.StdGen -> (Maybe (Search.Path Tour), Int, Int)
solveTSP AStarSimple tsp _      = solveWithAStar (heuristicNumRemainingCities tsp) tsp
solveTSP AStarMaxTriangle tsp _ = solveWithAStar (heuristicFarthestSingleCityThenGoal tsp) tsp
solveTSP AStarMST tsp _         = solveWithAStar (heuristicMST tsp) tsp
solveTSP SALinear tsp rnd       = solveWithSA linearCooling tsp rnd
solveTSP SALog tsp rnd       = solveWithSA logCooling tsp rnd
solveTSP SAWavy tsp rnd       = solveWithSA wavyCooling tsp rnd

parseArgs :: [String] -> Maybe RunMode
parseArgs []      = Nothing
parseArgs (s:[])
  | s == "astarsimple"   = Just AStarSimple
  | s == "astartriangle" = Just AStarMaxTriangle
  | s == "astar"         = Just AStarMST
  | s == "salinear"      = Just SALinear
  | s == "salog"         = Just SALog
  | s == "sawavy"        = Just SAWavy
parseArgs _              = Nothing

exitError :: String -> IO a
exitError msg = do
  hPutStrLn stderr msg
  exitWith $ ExitFailure 1

parseCityInfos :: IO [CityInfo]
parseCityInfos = do
  numCitiesStr <- getLine
  case readMaybe numCitiesStr :: Maybe Int of
   Nothing -> exitError "Error - number of cities is not an integer!"
   Just numCities -> do
     cityInfoMaybes <- replicateM numCities readAndParseCity
     case sequence cityInfoMaybes of
      Nothing -> exitError "Failed."
      Just cityInfos -> return cityInfos

  where readAndParseCity :: IO (Maybe CityInfo)
        readAndParseCity = do
          l <- getLine
          case parseCity l of
             Nothing -> hPutStrLn stderr ("Failed to parse " ++ l) >> return Nothing
             Just cityInfo -> return $ Just cityInfo

        parseCity :: String -> Maybe CityInfo
        parseCity s = case words s of
                         (ident_s:x_s:y_s:[]) -> do
                           ident <- headMay ident_s
                           x :: Int <- readMaybe x_s
                           y :: Int <- readMaybe y_s
                           return (ident, x, y)
                         _ -> Nothing

main :: IO ()
main = do
  rnd <- Random.getStdGen
  args <- getArgs
  case parseArgs args of
    Nothing -> exitError "Usage: ./tsp-search (astarsimple|astartriangle|astar|salinear|salog|sawavy)"
    Just mode -> do
      cityInfos <- parseCityInfos
      let tsp = makeTSP cityInfos
      -- print $ tourCost tsp (Tour "ACBD")
      -- exitSuccess
      let (maybeSoln, numProcessed, numSuccessors) = solveTSP mode tsp rnd
      maybe (exitError "Failed to find a solution") (prettyPrintSoln tsp numProcessed numSuccessors) maybeSoln

  where prettyPrintSoln :: TSP -> Int -> Int -> [Tour] -> IO ()
        prettyPrintSoln tsp numProcessed numSuccessors tours =
          let (Tour tour) = head tours
              soln = reverse tour
              totalCost = tourCost tsp (Tour tour)
          in putStrLn $ "Solution: " ++ soln ++
                        "\nCost: " ++ show totalCost ++
                        "\nProcessed: " ++ show numProcessed ++ ", Successors: " ++ show numSuccessors



