{-#LANGUAGE ScopedTypeVariables #-}
module TSPAStar where

import qualified Control.Monad.Trans.UnionFind as UnionFind
import Control.Monad.State
import qualified Data.Heap as Heap
import Control.Applicative

import qualified Search
import TSP
import qualified AStar

-- |Use A* to solve a TSP with a given heuristic function.
solveWithAStar :: (Tour -> Search.Cost) -> TSP -> (Maybe (Search.Path Tour), Int, Int)
solveWithAStar heuristic tsp =
  let problem = mkAStarTSPProblem heuristic tsp
  in AStar.aStarSearch problem (Tour [startCity])

-- |A simple heuristic function that simply counts how many cities are left
-- to visit.
heuristicNumRemainingCities :: TSP -> Tour -> Search.Cost
heuristicNumRemainingCities tsp (Tour tour) =
  fromIntegral $ length (tspCities tsp) - length tour

-- |A simple heuristic that finds the maximum of
-- (dist(current,c) + dist(c, first_city)) over all unvisited cities c.
heuristicFarthestSingleCityThenGoal :: TSP -> Tour -> Search.Cost
heuristicFarthestSingleCityThenGoal tsp (Tour tour) =
  let leftToVisit = notYetVisited tsp tour
      curCity = head tour
      goalCity = last tour
      toFarthestThenGoalCost =
        maximum $ map (\city ->
          cityDist tsp curCity city + cityDist tsp city goalCity)
                      leftToVisit
  in if null leftToVisit
       then 0
       else toFarthestThenGoalCost

-- |Representations for the MST heuristic.
-- MST is computed using a heap to add edges from shortest to longest, and
-- a UnionFind data structure to keep track of which nodes are connected.

-- An edge in the MST is represented by its two city endpoints and their
-- distance. The cities are using a UnionFind type so that they can be
-- looked up directly.
type PointEdge = (UnionFind.Point City, UnionFind.Point City, Search.Cost)

-- |This is an alias for the UnionFind Monad Transformer with the identifier
-- and inner State types we need.
type UF = UnionFind.UnionFindT City
          (State (Heap.MinPrioHeap Search.Cost PointEdge))

-- |Initialize the heap used for the MST algorithm below from a list of cities.
initMST_ :: [City] -> (City -> City -> Search.Cost) -> UF ()
initMST_ cities cost = do
  -- Create UnionFind identifiers for all cities.
  points <- mapM UnionFind.fresh cities
  -- Create an edge for the MST algorithm for every pair of cities.
  let edges = map (uncurry mkEdge) $ pairs $ zip cities points
  -- Put all of these edges in the heap with priority equal to the edge length.
  lift $ put $ Heap.fromList $ map (\e@(_, _, c) -> (c, e)) edges

 where -- Helper function to create a PointEdge out of two cities and their
       -- UnionFind representations.
       mkEdge :: (City, UnionFind.Point City)
              -> (City, UnionFind.Point City)
              -> PointEdge
       mkEdge (a, pa) (b, pb) = (pa, pb, cost a b)

       -- Helper function that returns all pairs of items given a list of items.
       pairs :: [a] -> [(a, a)]
       -- An empty list has no pairs.
       pairs [] = []
       -- Take the first item in the list as the first item in a pair, and
       -- make a pair with every other item in the list. Then append
       -- all pairs of the remainder of the list by recursing on it.
       pairs (n:ns) = ((,) n <$> ns) ++ pairs ns

-- |Implementation of finding the MST cost.
findMST_ :: UF Search.Cost
findMST_ = do
  -- Get the internal heap state.
  (heap :: Heap.MinPrioHeap Search.Cost PointEdge) <- lift get
  case Heap.view heap of
    -- If no edges left in heap, return 0 cost.
    Nothing -> return 0.0
    -- Take top item off of heap.
    Just ((_, (a, b, c)), heap') -> do
      -- Set internal heap to be heap with top element removed.
      lift $ put heap'
      -- Check if the two cities of this edge are in the same UnionFind set.
      -- If they are then they are already connected and so adding this
      -- edge would cause a cycle in the spanning tree.
      isCycle <- UnionFind.equivalent a b
      -- Union the two cities
      -- (will have no effect if they are already connected).
      UnionFind.union a b
      -- Recurse with the current heap and UnionFind state to get the cost
      -- of the rest of the edges in the MST.
      rest <- findMST_
      -- Return the total cost. Include the edge taken above unless it would
      -- form a cycle.
      return $ if isCycle then rest else c + rest

-- |Find the cost of an MST given a list of cities and a cost function that
-- returns the length of edge between two cities.
findMST :: [City] -> (City -> City -> Search.Cost) -> Search.Cost
findMST cities cost =
  let st :: State (Heap.MinPrioHeap Search.Cost PointEdge) Search.Cost
      st = UnionFind.runUnionFind (initMST_ cities cost >> findMST_)
      (mst, _) = runState st Heap.empty
  in mst

-- |A good heuristic function that computes the cost of an MST of the unvisited
-- nodes and adds the shortest edges connecting the current node to it and
-- the starting state to it.
heuristicMST :: TSP -> Tour -> Search.Cost
heuristicMST tsp (Tour tour) =
  let -- The nodes of the MST will be unvisited cities.
      nodes = notYetVisited tsp tour
      -- Compute the MST cost using the above algorithm.
      mstCost = findMST nodes (cityDist tsp)
      cur = head tour
      goal = startCity
      -- Shortest edge connecting current city to MST.
      curToClosest = minimum $ map (cityDist tsp cur) nodes
      -- Shortest edge connecting first city to MST.
      goalToClosest = minimum $ map (cityDist tsp goal) nodes
      -- Heuristic cost. Zero if no unvisited cities, otherwise sum
      -- the MST cost and the two edges found above.
      h | null nodes = 0.0
        | otherwise = mstCost + curToClosest + goalToClosest
  in h

-- For this problem, ProblemNodes are actually tours.
-- |Create the problem definition for a TSP for A* given a heuristic function.
mkAStarTSPProblem :: (Tour -> Search.Cost)
                  -> TSP
                  -> AStar.ProblemDef Tour [City]
mkAStarTSPProblem heuristic tsp = AStar.ProblemDef
  { AStar.isGoalNode = isGoalNode
  , AStar.successors = successors
    -- Cost only depends on the problem node (partial tour), not the A* search
    -- nodes that have been traversed so far.
  , AStar.costSoFar = \(tour:_) -> tourCost tsp tour
  , AStar.heuristicCostToEnd = heuristic
  }
  where -- A goal node is just a complete tour (no repeating start node).
        isGoalNode (Tour tour) = length tour == length (tspCities tsp)
        -- A successor is created for visiting each unvisited city next.
        successors (Tour tour) = map (Tour . (:tour)) $ notYetVisited tsp tour
