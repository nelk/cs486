{-#LANGUAGE MultiParamTypeClasses, FlexibleInstances, ScopedTypeVariables #-}
module Main where

import Prelude
import System.IO (hPutStrLn, stderr)
import System.Environment (getArgs)
import System.Exit
import Control.Monad
import Data.List (sort)
import Text.Read hiding (lift, get)
import Control.Applicative
import Control.Monad.State
import Safe
import qualified System.Random as Random
import qualified Data.Set as Set
import qualified Data.Map as Map
import qualified Control.Monad.Trans.UnionFind as UnionFind
import qualified Data.Heap as Heap

import qualified Search
import qualified AStar
import qualified SimulatedAnnealing as SA

type City = Char
newtype Tour = Tour [City]
data TSP = TSP [City] (Map.Map City (Int, Int))

instance Show Tour where
  show (Tour t) = reverse t

instance Search.ProblemNode Tour [City] where
  ident (Tour tour) = tour

type CityInfo = (City, Int, Int)

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

notYetVisited :: TSP -> [City] -> [City]
notYetVisited tsp tour = let visitedSet = Set.fromList tour
                     in filter (not . (`Set.member` visitedSet)) $ tspCities tsp

startCity :: City
startCity = 'A'

-- Note: Returns path of constructing tours and each tour in reverse.
solveTSP :: RunMode -> TSP -> Random.StdGen -> (Maybe (Search.Path Tour), Int, Int)
solveTSP AStarSimple tsp _      = solveWithAStar (heuristicNumRemainingCities tsp) tsp
solveTSP AStarMaxTriangle tsp _ = solveWithAStar (heuristicFarthestSingleCityThenGoal tsp) tsp
solveTSP AStarMST tsp _         = solveWithAStar (heuristicMST tsp) tsp
solveTSP SALinear tsp rnd       = solveWithSA linearCooling tsp rnd
solveTSP SALog tsp rnd       = solveWithSA logCooling tsp rnd
solveTSP SAWavy tsp rnd       = solveWithSA wavyCooling tsp rnd

solveWithAStar :: (Tour -> Search.Cost) -> TSP -> (Maybe (Search.Path Tour), Int, Int)
solveWithAStar heuristic tsp =
  let problem = mkAStarTSPProblem heuristic tsp
  in AStar.aStarSearch problem (Tour [startCity])

heuristicNumRemainingCities :: TSP -> Tour -> Search.Cost
heuristicNumRemainingCities tsp (Tour tour) = fromIntegral $ length (tspCities tsp) - length tour

heuristicFarthestSingleCityThenGoal :: TSP -> Tour -> Search.Cost
heuristicFarthestSingleCityThenGoal tsp (Tour tour) =
  let leftToVisit = notYetVisited tsp tour
      curCity = head tour
      goalCity = last tour
      toFarthestThenGoalCost = maximum $ map (\city -> cityDist tsp curCity city + cityDist tsp city goalCity) leftToVisit
  in if null leftToVisit
       then 0
       else toFarthestThenGoalCost

type Edge = (City, City, Search.Cost)
type PointEdge = (UnionFind.Point City, UnionFind.Point City, Search.Cost)

type UF = UnionFind.UnionFindT City (State (Heap.MinPrioHeap Search.Cost PointEdge))

initMST_ :: [City] -> (City -> City -> Search.Cost) -> UF ()
initMST_ cities cost = do
  points <- mapM UnionFind.fresh cities
  let edges = map (uncurry mkEdge) $ pairs $ zip cities points
  lift $ put $ Heap.fromList $ map (\e@(_, _, c) -> (c, e)) edges

 where mkEdge :: (City, UnionFind.Point City) -> (City, UnionFind.Point City) -> PointEdge
       mkEdge (a, pa) (b, pb) = (pa, pb, cost a b)
       pairs :: [a] -> [(a, a)]
       pairs [] = []
       pairs (n:ns) = ((,) n <$> ns) ++ pairs ns

findMST_ :: UF Search.Cost
findMST_ = do
  (heap :: Heap.MinPrioHeap Search.Cost PointEdge) <- lift get
  case Heap.view heap of
    Nothing -> return 0.0
    Just ((_, (a, b, c)), heap') -> do
      lift $ put heap'
      isCycle <- UnionFind.equivalent a b
      UnionFind.union a b
      rest <- findMST_
      return $ if isCycle then rest else c + rest

findMST :: [City] -> (City -> City -> Search.Cost) -> Search.Cost
findMST cities cost =
  let st :: State (Heap.MinPrioHeap Search.Cost PointEdge) Search.Cost
      st = UnionFind.runUnionFind (initMST_ cities cost >> findMST_)
      (mst, _) = runState st Heap.empty
  in mst

heuristicMST :: TSP -> Tour -> Search.Cost
heuristicMST tsp (Tour tour) =
  let nodes = notYetVisited tsp tour
      mstCost = findMST nodes (cityDist tsp)
      cur = head tour
      goal = startCity -- Could make more general by taking last tour.
      curToClosest = minimum $ map (cityDist tsp cur) nodes
      goalToClosest = minimum $ map (cityDist tsp goal) nodes
      h | null nodes = 0.0
        | otherwise = mstCost + curToClosest + goalToClosest
  in h

-- For this problem, ProblemNodes are actually tours.
mkAStarTSPProblem :: (Tour -> Search.Cost) -> TSP -> AStar.ProblemDef Tour [City]
mkAStarTSPProblem heuristic tsp = AStar.ProblemDef
  { AStar.isGoalNode = isGoalNode
  , AStar.successors = successors
  , AStar.costSoFar = \(tour:_) -> tourCost tsp tour
  , AStar.heuristicCostToEnd = heuristic
  }
  where isGoalNode (Tour tour) = length tour == length (tspCities tsp)
        successors (Tour tour) = map (Tour . (:tour)) $ notYetVisited tsp tour

solveWithSA :: SA.CoolingSchedule -> TSP -> Random.StdGen -> (Maybe (Search.Path Tour), Int, Int)
solveWithSA cs tsp rnd =
  let problem = mkSATSPProblem cs tsp
  in SA.saSearch problem (Tour $ tspCities tsp) rnd -- TODO: Random start point

reverseRange :: String -> Int -> Int -> String
reverseRange s i j =
  let (prefix, nonPrefix) = splitAt i s
      (inRange, postfix) = splitAt (j-i+1) nonPrefix
  in prefix ++ reverse inRange ++ postfix


-- For this problem, ProblemNodes are actually tours.
mkSATSPProblem :: SA.CoolingSchedule -> TSP -> SA.ProblemDef Tour [City]
mkSATSPProblem cs tsp = SA.ProblemDef
  { SA.successors = \(Tour tour) -> successors tour
  , SA.solutionCost = \(tour:_) -> tourCost tsp tour
  , SA.coolingSchedule = cs
  , SA.maxSteps = 1000000 --40000
  }
  where successors tour
          | length tour < 4 = []
          | otherwise = let ranges = [(i, j) | i <- [1..length tour - 3], j <- [i+1..length tour - 2]]
                        in map (Tour . uncurry (reverseRange tour)) ranges

linearCooling :: SA.CoolingSchedule
linearCooling x = 1000 - x

logCooling :: SA.CoolingSchedule
logCooling x = 300*log(-x/100 + 30)

wavyCooling :: SA.CoolingSchedule
wavyCooling x = 501 - x/4 + (1000 - x/2)/2 * cos(x/5) * sin(x/12)
--wavyCooling x = 1000.0 - x + 100.0 * cos (x/5.0) * sin (x/12.0)


data RunMode = AStarSimple | AStarMaxTriangle | AStarMST | SALinear | SALog | SAWavy

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



