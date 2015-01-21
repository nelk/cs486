{-#LANGUAGE MultiParamTypeClasses, FlexibleInstances, ScopedTypeVariables #-}
module Main where

import Prelude
import System.IO (hPutStrLn, stderr)
import System.Environment (getArgs)
import System.Exit
import Control.Monad
import Data.List (sort)
import Text.Read
import Safe
import qualified System.Random as Random
import qualified Data.Set as Set
import qualified Data.Map as Map

import qualified Search
import qualified AStar
import qualified SimulatedAnnealing as SA

type City = Char
newtype Tour = Tour [City] deriving Show
data TSP = TSP [City] (Map.Map City (Int, Int))

instance Search.ProblemNode Tour [City] where
  ident (Tour tour) = tour

tspCities :: TSP -> [City]
tspCities (TSP cities _) = cities

tspGet :: TSP -> City -> (Int, Int)
tspGet (TSP _ cityMap) c = cityMap Map.! c

makeTSP :: [CityInfo] -> TSP
makeTSP = do
  orderedCityNames <- sort . map (\(c, _, _) -> c)
  cityMap <- Map.fromList . map (\(c, x, y) -> (c, (x, y)))
  return $ TSP orderedCityNames cityMap

cityDist :: TSP -> City -> City -> Search.Cost
cityDist tsp a b = let (x1, y1) = tspGet tsp a
                       (x2, y2) = tspGet tsp b
                       square x = x*x
                   in sqrt $ fromIntegral $ square (x2 - x1) + square (y2 - y1)

-- Only includes going back to goal if tour is of same length as number of cities.
tourCost :: TSP -> Tour -> Search.Cost
tourCost _ (Tour []) = 0
tourCost _ (Tour (_:[])) = 0
tourCost tsp (Tour tour@(a:b:_))
  | length tour < length (tspCities tsp) = cityDist tsp a b + tourCost tsp (Tour $ tail tour)
  | otherwise = cityDist tsp a (last tour) + cityDist tsp a b + tourCost tsp (Tour $ tail tour)

startCity :: City
startCity = 'A'

-- Note: Returns path of constructing tours and each tour in reverse.
solveTSP :: RunMode -> TSP -> Random.StdGen -> (Maybe (Search.Path Tour), Int)
solveTSP AStarSimple tsp _ = solveWithAStar False tsp
solveTSP AStarBetter tsp _ = solveWithAStar True tsp
solveTSP Annealing tsp rnd = solveWithSA tsp rnd

solveWithAStar :: Bool -> TSP -> (Maybe (Search.Path Tour), Int)
solveWithAStar useGoodHeuristic tsp =
  let problem = mkAStarTSPProblem useGoodHeuristic tsp
  in AStar.aStarSearch problem (Tour [startCity])


-- For this problem, ProblemNodes are actually tours.
mkAStarTSPProblem :: Bool -> TSP -> AStar.ProblemDef Tour [City]
mkAStarTSPProblem useGoodHeuristic tsp = AStar.ProblemDef
  { AStar.isGoalNode = isGoalNode
  , AStar.successors = successors
  , AStar.costSoFar = \(tour:_) -> tourCost tsp tour
  , AStar.heuristicCostToEnd = if useGoodHeuristic
                                  then heuristicFarthestSingleCityThenGoal
                                  else heuristicNumRemainingCities
  }
  where notYetVisited :: [City] -> [City]
        notYetVisited tour = let visitedSet = Set.fromList tour
                             in filter (not . (`Set.member` visitedSet)) $ tspCities tsp

        isGoalNode (Tour tour) = length tour == length (tspCities tsp)
        successors (Tour tour) = map (Tour . (:tour)) $ notYetVisited tour

        heuristicNumRemainingCities (Tour tour) = fromIntegral $ length (tspCities tsp) - length tour

        heuristicFarthestSingleCityThenGoal (Tour tour) =
          let leftToVisit = notYetVisited tour
              curCity = head tour
              goalCity = last tour
              toFarthestThenGoalCost = maximum $ map (\city -> cityDist tsp curCity city + cityDist tsp city goalCity) leftToVisit
          in if null leftToVisit
               then 0
               else toFarthestThenGoalCost

solveWithSA :: TSP -> Random.StdGen -> (Maybe (Search.Path Tour), Int)
solveWithSA tsp rnd =
  let problem = mkSATSPProblem tsp
  in SA.saSearch problem (Tour $ tspCities tsp) rnd -- TODO: Random start point

reverseRange :: String -> Int -> Int -> String
reverseRange s i j =
  let (prefix, nonPrefix) = splitAt i s
      (inRange, postfix) = splitAt (j-i+1) nonPrefix
  in prefix ++ reverse inRange ++ postfix


-- For this problem, ProblemNodes are actually tours.
mkSATSPProblem :: TSP -> SA.ProblemDef Tour [City]
mkSATSPProblem tsp = SA.ProblemDef
  { SA.successors = \(Tour tour) -> successors tour
  , SA.solutionCost = \(tour:_) -> tourCost tsp tour
  , SA.coolingSchedule = SA.CoolingSchedule
        { SA.startTemperature = 100.0
        , SA.decrement = 0.0002
        }
  , SA.maxSteps = 40000
  }
  where successors tour
          | length tour < 4 = []
          | otherwise = let ranges = [(i, j) | i <- [1..length tour - 3], j <- [i+1..length tour - 2]]
                        in map (Tour . uncurry (reverseRange tour)) ranges


type CityInfo = (City, Int, Int)

data RunMode = AStarSimple | AStarBetter | Annealing

parseArgs :: [String] -> Maybe RunMode
parseArgs []      = Nothing
parseArgs (s:[])
  | s == "simple" = Just AStarSimple
  | s == "astar"  = Just AStarBetter
  | s == "sa"     = Just Annealing
parseArgs _       = Nothing

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
    Nothing -> exitError "Usage: ./tsp-search (simple|astar|sa)"
    Just mode -> do
      cityInfos <- parseCityInfos
      let tsp = makeTSP cityInfos
      let (maybeSoln, numProcessed) = solveTSP mode tsp rnd
      maybe (exitError "Failed to find a solution") (prettyPrintSoln tsp numProcessed) maybeSoln

  where prettyPrintSoln :: TSP -> Int -> [Tour] -> IO ()
        prettyPrintSoln tsp numProcessed tours =
          let (Tour tour) = head tours
              soln = reverse tour
              totalCost = tourCost tsp (Tour tour)
          in putStrLn $ "Solution: " ++ soln ++
                        "\nCost: " ++ show totalCost ++
                        "\nNum Processed Nodes: " ++ show numProcessed



