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
import qualified Data.Set as Set
import qualified Data.Map as Map

import qualified Search
import qualified AStar

type City = Char
newtype Tour = Tour [City] deriving Show
data TSP = TSP [City] (Map.Map City (Int, Int))

instance AStar.ProblemNode Tour [City] where
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

cityDist :: TSP -> City -> City -> AStar.Cost
cityDist tsp a b = let (x1, y1) = tspGet tsp a
                       (x2, y2) = tspGet tsp b
                       square x = x*x
                   in sqrt $ fromIntegral $ square (x2 - x1) + square (y2 - y1)

-- Only includes going back to goal if all tour is of same length as number of cities.
tourCost :: TSP -> Tour -> Double
tourCost _ (Tour []) = 0
tourCost _ (Tour (_:[])) = 0
tourCost tsp (Tour tour@(a:b:_))
  | length tour < length (tspCities tsp) = cityDist tsp a b + tourCost tsp (Tour $ tail tour)
  | otherwise = cityDist tsp a (last tour) + cityDist tsp a b + tourCost tsp (Tour $ tail tour)

startCity :: City
startCity = 'A'

solveTSP :: RunMode -> TSP -> (Maybe (Search.Path Tour), Int)
solveTSP AStarSimple tsp = solveWithAStar False tsp
solveTSP AStarBetter tsp = solveWithAStar True tsp
solveTSP Annealing _ = undefined

solveWithAStar :: Bool -> TSP -> (Maybe (Search.Path Tour), Int)
solveWithAStar useGoodHeuristic tsp =
  let problem = mkAStarTSPProblem useGoodHeuristic tsp
  in AStar.aStarSearch problem (Tour [startCity])


-- For this problem, ProblemNodes are actually tours.
mkAStarTSPProblem :: Bool -> TSP -> AStar.ProblemDef Tour [City]
mkAStarTSPProblem useGoodHeuristic tsp = AStar.ProblemDef
  { AStar.isGoalNode = isGoalNode
  , AStar.neighbours = neighbours
  , AStar.costSoFar = \(tour:_) -> tourCost tsp tour
  , AStar.heuristicCostToEnd = if useGoodHeuristic
                                  then heuristicFarthestSingleCityThenGoal
                                  else heuristicNumRemainingCities
  }
  where notYetVisited :: [City] -> [City]
        notYetVisited tour = let visitedSet = Set.fromList tour
                             in filter (not . (`Set.member` visitedSet)) $ tspCities tsp

        isGoalNode (Tour tour) = length tour == length (tspCities tsp)
        neighbours (Tour tour) = map (Tour . (:tour)) $ notYetVisited tour

        heuristicNumRemainingCities (Tour tour) = fromIntegral $ length (tspCities tsp) - length tour

        heuristicFarthestSingleCityThenGoal (Tour tour) =
          let leftToVisit = notYetVisited tour
              curCity = head tour
              goalCity = last tour
              toFarthestThenGoalCost = maximum $ map (\city -> cityDist tsp curCity city + cityDist tsp city goalCity) leftToVisit
          in if null leftToVisit
               then 0
               else toFarthestThenGoalCost


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
  args <- getArgs
  case parseArgs args of
    Nothing -> exitError "Usage: ./tsp-search (simple|astar|sa)"
    Just mode -> do
      cityInfos <- parseCityInfos
      let tsp = makeTSP cityInfos
      let (maybeSoln, numProcessed) = solveTSP mode tsp
      maybe (exitError "Failed to find a solution") (prettyPrintSoln tsp numProcessed) maybeSoln

  where prettyPrintSoln :: TSP -> Int -> [Tour] -> IO ()
        prettyPrintSoln tsp numProcessed tours =
          let (Tour tour) = last tours
              soln = reverse tour
              totalCost = tourCost tsp (Tour tour)
          in putStrLn $ "Solution: " ++ soln ++
                        "\nCost: " ++ show totalCost ++
                        "\nNum Processed Nodes: " ++ show numProcessed



