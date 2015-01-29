{-#LANGUAGE MultiParamTypeClasses, FlexibleInstances, ScopedTypeVariables #-}
module SimulatedAnnealing (CoolingSchedule, ProblemDef(..), saSearch) where

import qualified System.Random as Random
import qualified Search

import Debug.Trace (trace)

data (Search.ProblemNode pn k, Show pn) => SANode pn k = SANode (Search.Path pn) pn
  deriving Show

type CoolingSchedule = Double -> Double

data Search.ProblemNode pn k => ProblemDef pn k = ProblemDef
  { successors :: pn -> [pn]
  , solutionCost :: Search.Path pn -> Search.Cost
  , coolingSchedule :: CoolingSchedule
  , maxSteps :: Int
  }

data SAState pn k = SAState
  { problemDef :: ProblemDef pn k
  , successorCount :: !Int
  , processedCount :: !Int
  , numSteps :: Int
  , stdGen :: Random.StdGen
  , nextSuccessor :: SANode pn k
  , bestNode :: SANode pn k
  , bestCost :: Search.Cost
  }

getTemperature :: Search.ProblemNode pn k => SAState pn k -> Double
getTemperature = do
  cs <- coolingSchedule.problemDef
  st <- numSteps
  ms <- maxSteps.problemDef
  return $ cs $ 1000.0 * fromIntegral st / fromIntegral ms

instance (Search.ProblemNode pn k, Show pn) => Search.Searcher (SAState pn k) pn where
  nextNode s
    | Search.isAtGoal s = let (SANode _ pn) = bestNode s in Just pn
    | otherwise         = let (SANode _ pn) = nextSuccessor s in Just pn

  searchPath s
    | Search.isAtGoal s = let (SANode path _) = bestNode s in path
    | otherwise         = let (SANode path _) = nextSuccessor s in path

  isAtGoal s = numSteps s > maxSteps (problemDef s) || getTemperature s <= 0.0

  expandNextNode s =
    let problem = problemDef s
        (SANode curPath curProblemNode) = nextSuccessor s
        succs = successors problem curProblemNode
        rnd = stdGen s
        (rndMoveIdx :: Int, rnd') = Random.randomR (0, length succs - 1) rnd
        potentialNextProblemNode = succs !! rndMoveIdx
        curCost = solutionCost problem curPath
        nextCost = solutionCost problem (potentialNextProblemNode:curPath)
        t = getTemperature s
        moveProb = exp 1.0 ** (-(curCost - nextCost)/t)
        (rndProb :: Double, rnd'') = Random.randomR (0.0, 1.0) rnd'
        nextProblemNode
          | null succs || rndProb > moveProb = curProblemNode
          | otherwise = potentialNextProblemNode
        (newBestCost, newBestNode)
          | nextCost < bestCost s = (nextCost, SANode (potentialNextProblemNode:curPath) potentialNextProblemNode)
          | otherwise             = (bestCost s, bestNode s)

        traceState :: a -> a
        traceState
          | newBestCost == bestCost s = id
          | otherwise = trace $ show (numSteps s + 1) ++ ", best=" ++ show newBestCost

    in traceState $ s { processedCount = processedCount s + 1
         , successorCount = successorCount s + length succs
         , stdGen = rnd''
         , nextSuccessor = SANode [nextProblemNode] nextProblemNode -- [] -> curPath
         , numSteps = numSteps s + 1
         , bestNode = newBestNode
         , bestCost = newBestCost
         }

-- Note: Returns path in reverse order.
saSearch :: (Search.ProblemNode pn k, Show pn) => ProblemDef pn k -> pn -> Random.StdGen -> (Maybe (Search.Path pn), Int, Int)
saSearch probDef startProblemNode rnd = (fst soln, processedCount $ snd soln, successorCount $ snd soln)
  where searcher = SAState
          { problemDef = probDef
          , processedCount = 0
          , successorCount = 0
          , numSteps = 0
          , stdGen = rnd
          , nextSuccessor = SANode [startProblemNode] startProblemNode
          , bestNode = SANode [startProblemNode] startProblemNode
          , bestCost = solutionCost probDef [startProblemNode]
          }
        soln = Search.search searcher
 
