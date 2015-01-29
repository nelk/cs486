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

-- |The different ways to instantiate the program, including different
-- A* heuristics and different SA cooling schedules.
data RunMode = AStarSimple | AStarMaxTriangle | AStarMST
             | SALinear | SALog | SAWavy

-- |Turns a raw string into a RunMode if possible.
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


-- |Solve a TSP with the given RunMode and return the best solution with some
-- other data (number of nodes processed and visited).
-- Note: Lists here are in reverse order, this is for both search path and
-- each tour itself.
solveTSP :: RunMode -> TSP -> Random.StdGen -> (Maybe (Search.Path Tour), Int, Int)
solveTSP AStarSimple tsp _      = solveWithAStar (heuristicNumRemainingCities tsp) tsp
solveTSP AStarMaxTriangle tsp _ = solveWithAStar (heuristicFarthestSingleCityThenGoal tsp) tsp
solveTSP AStarMST tsp _         = solveWithAStar (heuristicMST tsp) tsp
solveTSP SALinear tsp rnd       = solveWithSA linearCooling tsp rnd
solveTSP SALog tsp rnd       = solveWithSA logCooling tsp rnd
solveTSP SAWavy tsp rnd       = solveWithSA wavyCooling tsp rnd

-- |Exit the program with fail status after printing an error message.
exitError :: String -> IO a
exitError msg = do
  hPutStrLn stderr msg
  exitWith $ ExitFailure 1

-- |Read from stdin and convert to CityInfo types.
parseCityInfos :: IO [CityInfo]
parseCityInfos = do
  -- First line is number o cities.
  numCitiesStr <- getLine
  case readMaybe numCitiesStr :: Maybe Int of
   Nothing -> exitError "Error - number of cities is not an integer!"
   Just numCities -> do
     -- Call readAndParseCity numCities times.
     cityInfoMaybes <- replicateM numCities readAndParseCity
     -- Fail if any CityInfo failed to parse.
     case sequence cityInfoMaybes of
      Nothing -> exitError "Failed."
      Just cityInfos -> return cityInfos

  where readAndParseCity :: IO (Maybe CityInfo)
        readAndParseCity = do
          l <- getLine
          case parseCity l of
             Nothing -> hPutStrLn stderr ("Failed to parse " ++ l)
                        >> return Nothing
             Just cityInfo -> return $ Just cityInfo

        parseCity :: String -> Maybe CityInfo
        parseCity s = case words s of
                         (ident_s:x_s:y_s:[]) -> do
                           ident <- headMay ident_s
                           x :: Int <- readMaybe x_s
                           y :: Int <- readMaybe y_s
                           return (ident, x, y)
                         _ -> Nothing

-- |Program entry point.
main :: IO ()
main = do
  -- |Create pure random number generator from system seed.
  rnd <- Random.getStdGen
  args <- getArgs
  case parseArgs args of
    Nothing -> exitError "Usage: ./tsp-search (astarsimple|astartriangle|astar|salinear|salog|sawavy)"
    Just mode -> do
      cityInfos <- parseCityInfos
      -- Create TSP from the parsed input data.
      let tsp = makeTSP cityInfos
      -- Call out to appropriate solver to find a solution to the problem.
      let (maybeSoln, numProcessed, numSuccessors) = solveTSP mode tsp rnd
      -- If a solution was found, pretty-print it using the format below.
      maybe (exitError "Failed to find a solution")
            (prettyPrintSoln tsp numProcessed numSuccessors)
            maybeSoln

  where prettyPrintSoln :: TSP -> Int -> Int -> [Tour] -> IO ()
        prettyPrintSoln tsp numProcessed numSuccessors tours =
          let (Tour tour) = head tours
              soln = reverse tour
              totalCost = tourCost tsp (Tour tour)
          in putStrLn $ "Solution: " ++ soln ++
                        "\nCost: " ++ show totalCost ++
                        "\nProcessed: " ++ show numProcessed ++
                        ", Successors: " ++ show numSuccessors

