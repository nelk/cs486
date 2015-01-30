{-#LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}
module AStar (ProblemDef(..), aStarSearch) where

import qualified Search
import qualified Data.Set as Set
import qualified Data.Heap as Heap

-- |A generic search node for A*. It holds the path through problem nodes
-- searched so far and the current problem node.
data (Search.ProblemNode pn k, Show pn)
  => AStarNode pn k = AStarNode (Search.Path pn) pn
  deriving Show

-- |A type holding the problem information required for A*.
data Search.ProblemNode pn k => ProblemDef pn k = ProblemDef
  { -- A function to generate the neighbours of a given problem node.
    successors :: pn -> [pn]
    -- The cost of the current search path.
  , costSoFar :: Search.Path pn -> Search.Cost
    -- An admissable heuristic function that estimates the cost from
    -- a given problem node to the goal node.
  , heuristicCostToEnd :: pn -> Search.Cost
    -- Whether a given problem node is a goal.
  , isGoalNode :: pn -> Bool
  }

-- |Making AStarState a searcher that can be used by the generic search
-- algorithm.
instance (Search.ProblemNode pn k, Show pn)
      => Search.Searcher (AStarState pn k) pn where
  -- The next node is just the top of the heap.
  nextNode = fmap (\(_, AStarNode _ n) -> n) . Heap.viewHead . fringe

  -- The search path through problem nodes is also stored in the A* node
  -- at the top of the heap.
  searchPath s = case Heap.viewHead (fringe s) of
                Nothing -> []
                Just (_, AStarNode path _) -> path

  -- We just ask the problem definition if the next node is a goal node.
  isAtGoal s = case Search.nextNode s of
                    Nothing -> False
                    Just pn -> isGoalNode (problemDef s) pn

  -- Here is where the work happens to create a new A* state by expanding a
  -- node.
  expandNextNode s = case Heap.view $ fringe s of
    Nothing -> s
    -- Remove the top of the fringe heap.
    Just ((_, AStarNode curPath curProblemNode), newFringe) ->
      let prob = problemDef s
          -- Generate neighbours of the current node.
          nextProblemNodes = successors prob curProblemNode
          -- Filter any neighbours that have been processed already.
          nextUnprocessedProblemNodes =
            filter (not . flip Set.member (processed s) . Search.ident)
                   nextProblemNodes
          -- Turn the successor problem nodes into A* nodes by adding them to
          -- the search path gone so far to reach the current node.
          nextNodes = map (\probNode -> AStarNode (probNode:curPath) probNode)
                          nextUnprocessedProblemNodes
          -- Add all of these new states to the heap.
          newFringe' = foldl (\h n' -> Heap.insert (nodeCost prob n', n') h)
                             newFringe nextNodes
          -- Add the current node to the set of processed nodes.
          newProcessed = Set.insert (Search.ident curProblemNode) $ processed s

      -- Build the next searcher state from the data calculated above.
      in s { fringe = newFringe'
           , processed = newProcessed
           , successorCount = successorCount s + length nextUnprocessedProblemNodes
           , processedCount = processedCount s + 1
           }

-- |A* searcher state.
data AStarState pn k = AStarState
  { -- A min heap to process successors with lowest cost + heuristic cost first.
    fringe :: !(Heap.MinPrioHeap Search.Cost (AStarNode pn k))
    -- A set to keep track of what problem nodes have been seen already.
  , processed :: !(Set.Set k)
    -- The problem info given by the user.
  , problemDef :: ProblemDef pn k
    -- Two counters for measuring how A* performed.
  , successorCount :: !Int
  , processedCount :: !Int
  }

-- Note: Returns path in reverse order.
-- |Solve a given problem using A*. It takes a problem and a starting problem
-- node, and tries to return a solution along with performance stats.
aStarSearch :: (Search.ProblemNode pn k, Show pn)
            => ProblemDef pn k -> pn -> (Maybe (Search.Path pn), Int, Int)
aStarSearch probDef startProblemNode =
  -- Use solution from generic search and output with counter values.
  (fst soln, processedCount $ snd soln, successorCount $ snd soln)
  where -- Construct the initial A* state.
        searcher = AStarState
          { problemDef = probDef
          , processed = Set.empty
          , fringe = Heap.singleton
                       (nodeCost probDef startAStarNode, startAStarNode)
          , successorCount = 0
          , processedCount = 0
          }
        -- Wrap starting problem node in A* node.
        startAStarNode = AStarNode [startProblemNode] startProblemNode
        -- Get solution using generic search method.
        soln = Search.search searcher

-- |The cost function for A* which adds the known cost of the search path so
-- far and the heuristic cost from the current problem node to the goal.
nodeCost :: (Search.ProblemNode pn k, Show pn)
         => ProblemDef pn k -> AStarNode pn k -> Search.Cost
nodeCost prob (AStarNode path cur) = costSoFar prob path
                                   + heuristicCostToEnd prob cur



