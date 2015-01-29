{-#LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}
module AStar (ProblemDef(..), aStarSearch) where

import qualified Search
import qualified Data.Set as Set
import qualified Data.Heap as Heap

data (Search.ProblemNode pn k, Show pn) => AStarNode pn k = AStarNode (Search.Path pn) pn
  deriving Show

data Search.ProblemNode pn k => ProblemDef pn k = ProblemDef
  { successors :: pn -> [pn]
  , costSoFar :: Search.Path pn -> Search.Cost
  , heuristicCostToEnd :: pn -> Search.Cost
  , isGoalNode :: pn -> Bool
  }

data AStarState pn k = AStarState
  { fringe :: !(Heap.MinPrioHeap Search.Cost (AStarNode pn k))
  , processed :: !(Set.Set k)
  , problemDef :: ProblemDef pn k
  , successorCount :: !Int
  , processedCount :: !Int
  }

-- Note: Returns path in reverse order.
aStarSearch :: (Search.ProblemNode pn k, Show pn) => ProblemDef pn k -> pn -> (Maybe (Search.Path pn), Int, Int)
aStarSearch probDef startProblemNode = (fst soln, processedCount $ snd soln, successorCount $ snd soln)
  where searcher = AStarState { problemDef = probDef
                              , processed = Set.empty
                              , fringe = Heap.singleton (nodeCost probDef startAStarNode, startAStarNode)
                              , successorCount = 0
                              , processedCount = 0
                              }
        startAStarNode = AStarNode [startProblemNode] startProblemNode
        soln = Search.search searcher

nodeCost :: (Search.ProblemNode pn k, Show pn) => ProblemDef pn k -> AStarNode pn k -> Search.Cost
nodeCost prob (AStarNode path cur) = costSoFar prob path + heuristicCostToEnd prob cur

instance (Search.ProblemNode pn k, Show pn) => Search.Searcher (AStarState pn k) pn where
  nextNode = fmap (\(_, AStarNode _ n) -> n) . Heap.viewHead . fringe

  searchPath s = case Heap.viewHead (fringe s) of
                Nothing -> []
                Just (_, AStarNode path _) -> path

  isAtGoal s = case Search.nextNode s of
                    Nothing -> False
                    Just pn -> isGoalNode (problemDef s) pn

  expandNextNode s = case Heap.view $ fringe s of
                      Nothing -> s
                      Just ((_, AStarNode curPath curProblemNode), newFringe) ->
                        let prob = problemDef s
                            nextProblemNodes = successors prob curProblemNode
                            nextUnprocessedProblemNodes = filter (not . flip Set.member (processed s) . Search.ident) nextProblemNodes
                            nextNodes = map (\probNode -> AStarNode (probNode:curPath) probNode) nextUnprocessedProblemNodes
                            newFringe' = foldl (\h n' -> Heap.insert (nodeCost prob n', n') h) newFringe nextNodes
                            newProcessed = Set.insert (Search.ident curProblemNode) $ processed s
                        in s { fringe = newFringe'
                             , processed = newProcessed
                             , successorCount = successorCount s + length nextUnprocessedProblemNodes
                             , processedCount = processedCount s + 1
                             }


