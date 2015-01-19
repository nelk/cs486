{-#LANGUAGE MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances #-}
module AStar (ProblemNode(..), ProblemDef(..), Cost, aStarSearch) where

import Control.Applicative
import Control.Arrow
import qualified Search
import qualified Data.Set as Set
import qualified Data.Heap as Heap

import Debug.Trace

type Cost = Double

class Ord k => ProblemNode pn k | pn -> k where
  ident :: pn -> k

data (ProblemNode pn k, Show pn) => AStarNode pn k = AStarNode (Search.Path pn) pn
  deriving Show

data ProblemNode pn k => ProblemDef pn k = ProblemDef
  { neighbours :: pn -> [pn]
  , costSoFar :: Search.Path pn -> Cost
  , heuristicCostToEnd :: pn -> Cost
  , isGoalNode :: pn -> Bool
  }

data AStarState pn k = AStarState
  { fringe :: Heap.MinPrioHeap Cost (AStarNode pn k)
  , processed :: Set.Set k
  , problemDef :: ProblemDef pn k
  }

aStarSearch :: (ProblemNode pn k, Show pn) => ProblemDef pn k -> pn -> (Maybe (Search.Path pn), Int)
aStarSearch probDef startProblemNode = (reverse <$>) *** (Set.size . processed) $ Search.search searcher
  where searcher = AStarState { problemDef = probDef
                              , processed = Set.empty
                              , fringe = Heap.singleton (nodeCost probDef startAStarNode, startAStarNode)
                              }
        startAStarNode = AStarNode [startProblemNode] startProblemNode

nodeCost :: (ProblemNode pn k, Show pn) => ProblemDef pn k -> AStarNode pn k -> Cost
nodeCost prob (AStarNode path cur) = costSoFar prob path + heuristicCostToEnd prob cur

instance (ProblemNode pn k, Show pn) => Search.Searcher (AStarState pn k) pn where
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
                            nextProblemNodes = neighbours prob curProblemNode
                            nextUnprocessedProblemNodes = filter (not . flip Set.member (processed s) . ident) nextProblemNodes
                            nextNodes = map (\probNode -> AStarNode (probNode:curPath) probNode) nextUnprocessedProblemNodes
                            newFringe' = foldl (\h n' -> Heap.insert (nodeCost prob n', n') h) newFringe nextNodes
                            newProcessed = Set.insert (ident curProblemNode) $ processed s
                        in {-trace ("nextUnprocessedProblemNodes " ++ show nextUnprocessedProblemNodes) $ -}
                          s { fringe = newFringe'
                             , processed = newProcessed
                             }


