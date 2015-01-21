{-#LANGUAGE MultiParamTypeClasses, FlexibleInstances, ScopedTypeVariables #-}
module SimulatedAnnealing (CoolingSchedule(..), ProblemDef(..), saSearch) where

import Control.Arrow
import qualified System.Random as Random
import qualified Search

data (Search.ProblemNode pn k, Show pn) => SANode pn k = SANode (Search.Path pn) pn
  deriving Show

data CoolingSchedule = CoolingSchedule
      { startTemperature :: Double
      , decrement :: Double
      }

data Search.ProblemNode pn k => ProblemDef pn k = ProblemDef
  { successors :: pn -> [pn]
  , solutionCost :: Search.Path pn -> Search.Cost
  , coolingSchedule :: CoolingSchedule
  , maxSteps :: Int
  }

data SAState pn k = SAState
  { problemDef :: ProblemDef pn k
  , successorCount :: Int
  , numSteps :: Int
  , stdGen :: Random.StdGen
  , nextSuccessor :: SANode pn k
  , temperature :: Double
  }

instance (Search.ProblemNode pn k, Show pn) => Search.Searcher (SAState pn k) pn where
  nextNode s = let (SANode _ pn) = nextSuccessor s
               in Just pn

  searchPath s = let (SANode path _) = nextSuccessor s
                 in path

  isAtGoal s = numSteps s > maxSteps (problemDef s) || temperature s <= 0.0

  expandNextNode s =
    let problem = problemDef s
        (SANode curPath curProblemNode) = nextSuccessor s
        succs = successors problem curProblemNode
        rnd = stdGen s
        (rndMoveIdx :: Int, rnd') = Random.randomR (0, length succs - 1) rnd
        potentialNextProblemNode = succs !! rndMoveIdx
        curCost = solutionCost problem curPath
        nextCost = solutionCost problem (potentialNextProblemNode:curPath)
        t = temperature s
        moveProb = exp 1.0 ** (-(curCost - nextCost)/t)
        (rndProb :: Double, rnd'') = Random.randomR (0.0, 1.0) rnd'
        nextProblemNode
          | null succs || rndProb > moveProb = curProblemNode
          | otherwise = potentialNextProblemNode
    in s { successorCount = successorCount s + length succs
         , stdGen = rnd''
         , nextSuccessor = SANode (nextProblemNode:[]) nextProblemNode -- [] -> curPath
         , numSteps = numSteps s + 1
         , temperature = t - decrement (coolingSchedule problem)
         }

-- Note: Returns path in reverse order.
saSearch :: (Search.ProblemNode pn k, Show pn) => ProblemDef pn k -> pn -> Random.StdGen -> (Maybe (Search.Path pn), Int)
saSearch probDef startProblemNode rnd = second successorCount $ Search.search searcher
  where searcher = SAState
          { problemDef = probDef
          , successorCount = 0
          , numSteps = 0
          , stdGen = rnd
          , nextSuccessor = SANode [startProblemNode] startProblemNode
          , temperature = startTemperature $ coolingSchedule probDef
          }
 
