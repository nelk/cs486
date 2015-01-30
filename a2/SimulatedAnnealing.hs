{-#LANGUAGE MultiParamTypeClasses, FlexibleInstances, ScopedTypeVariables #-}
module SimulatedAnnealing (CoolingSchedule, ProblemDef(..), saSearch) where

import qualified System.Random as Random
import qualified Search

import Debug.Trace (trace)

-- |SA node to keep track of the path searched through problem nodes.
-- This path isn't used and so won't actually be evaluated (because of lazy
-- evaluation).
data (Search.ProblemNode pn k, Show pn)
  => SANode pn k = SANode (Search.Path pn) pn
  deriving Show

-- |A cooling schedule is just a mathematical function from the time to
-- the temperature.
type CoolingSchedule = Double -> Double

-- |Problem definition with required information for SA.
data Search.ProblemNode pn k => ProblemDef pn k = ProblemDef
  { -- Neighbourhood operator.
    successors :: pn -> [pn]
    -- Cost of the current state/path.
  , solutionCost :: Search.Path pn -> Search.Cost
  , coolingSchedule :: CoolingSchedule
    -- Stop after a maximum number of iterations.
  , maxSteps :: Int
  }

-- |SA searcher state.
data SAState pn k = SAState
  { problemDef :: ProblemDef pn k
  , successorCount :: !Int
  , processedCount :: !Int
  , numSteps :: Int
    -- Pure random number generator.
  , stdGen :: Random.StdGen
    -- Holding onto the next node to process.
  , nextSuccessor :: SANode pn k
    -- Best node and cost seen so far.
  , bestNode :: SANode pn k
  , bestCost :: Search.Cost
  }

-- |Get the current temperature using the cooling schedule and iteration count
-- from the SA searcher state.
getTemperature :: Search.ProblemNode pn k => SAState pn k -> Double
getTemperature = do
  -- Get required information from current state.
  cs <- coolingSchedule.problemDef
  st <- numSteps
  ms <- maxSteps.problemDef
  -- Scale cooling schedule function up to maximum number of steps we plan take.
  -- They are scaled to work for 1000 steps by default.
  return $ cs $ 1000.0 * fromIntegral st / fromIntegral ms

-- |Make SAState a searcher.
instance (Search.ProblemNode pn k, Show pn)
      => Search.Searcher (SAState pn k) pn where
  -- If we are stopping (calling it a goal state for generic search) then
  -- return the best solution we've ever seen. Otherwise return the next node.
  nextNode s
    | Search.isAtGoal s = let (SANode _ pn) = bestNode s in Just pn
    | otherwise         = let (SANode _ pn) = nextSuccessor s in Just pn

  -- If we are stopping (calling it a goal state for generic search) then
  -- return the best path we've ever seen. Otherwise return the current path.
  searchPath s
    | Search.isAtGoal s = let (SANode path _) = bestNode s in path
    | otherwise         = let (SANode path _) = nextSuccessor s in path

  -- Stopping condition.
  isAtGoal s = numSteps s > maxSteps (problemDef s) || getTemperature s <= 0.0

  -- Advance the state to the next neighbour.
  expandNextNode s =
    let problem = problemDef s
        (SANode curPath curProblemNode) = nextSuccessor s
        -- Get successors of current node.
        succs = successors problem curProblemNode
        rnd = stdGen s
        -- Generate a random integer (and a new generator) for choosing a
        -- successor.
        (rndMoveIdx :: Int, rnd') = Random.randomR (0, length succs - 1) rnd
        -- Choose the successor.
        potentialNextProblemNode = succs !! rndMoveIdx
        -- Calculate the cost of the current solution.
        curCost = solutionCost problem curPath
        -- Calculate the cost of the successor.
        nextCost = solutionCost problem (potentialNextProblemNode:curPath)
        -- Get the current temperature.
        t = getTemperature s
        -- Use the Boltzmann formula to compute a probability of accepting
        -- the successor.
        moveProb = exp 1.0 ** ((curCost - nextCost)/t)
        -- Generate a random double in [0, 1].
        (rndProb :: Double, rnd'') = Random.randomR (0.0, 1.0) rnd'
        -- Move to the successor if the random number is less than or equal to
        -- the move probability, which is larger than one if the new node has a
        -- better cost.
        nextProblemNode
          | null succs || rndProb > moveProb = curProblemNode
          | otherwise = potentialNextProblemNode
        -- Update the best if the next node we're moving to is better.
        (newBestCost, newBestNode)
          | nextCost < bestCost s = (nextCost, SANode (potentialNextProblemNode:curPath) potentialNextProblemNode)
          | otherwise             = (bestCost s, bestNode s)

        -- Helper to print running current and best for graphing.
        traceState :: a -> a
        traceState
          | newBestCost /= bestCost s || (numSteps s + 1) `mod` 1000 == 0 =
              trace $ show (numSteps s + 1) ++ " cur=" ++ show curCost ++
                                               " best=" ++ show newBestCost
          | otherwise = id

    -- Create next searcher state from the data computed above.
    in traceState $ s { processedCount = processedCount s + 1
         , successorCount = successorCount s + length succs
         , stdGen = rnd''
         , nextSuccessor = SANode [nextProblemNode] nextProblemNode
         , numSteps = numSteps s + 1
         , bestNode = newBestNode
         , bestCost = newBestCost
         }

-- |Solve a problem at the given starting node using SA.
saSearch :: (Search.ProblemNode pn k, Show pn)
         => ProblemDef pn k
         -> pn
         -> Random.StdGen
         -> (Maybe (Search.Path pn), Int, Int)
saSearch probDef startProblemNode rnd = (fst soln, processedCount $ snd soln, successorCount $ snd soln)
  where -- Initial searcher state for SA.
        searcher = SAState
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
 
