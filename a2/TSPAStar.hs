{-#LANGUAGE ScopedTypeVariables #-}
module TSPAStar where

import qualified Control.Monad.Trans.UnionFind as UnionFind
import Control.Monad.State
import qualified Data.Heap as Heap
import Control.Applicative

import qualified Search
import TSP
import qualified AStar

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
