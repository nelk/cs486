module TSPSimulatedAnnealing where

import qualified System.Random as Random

import qualified Search
import qualified SimulatedAnnealing as SA
import TSP

-- |Iterate on finding better solutions to TSP with the given cooling schedule.
solveWithSA :: SA.CoolingSchedule
            -> TSP
            -> Random.StdGen
            -> (Maybe (Search.Path Tour), Int, Int)
solveWithSA cs tsp rnd =
  let problem = mkSATSPProblem cs tsp
  in SA.saSearch problem (Tour $ tspCities tsp) rnd

-- |Helper to reverse the range of items in a list between two given indices
-- (inclusive).
reverseRange :: [a] -> Int -> Int -> [a]
reverseRange s i j =
  let (prefix, nonPrefix) = splitAt i s
      (inRange, postfix) = splitAt (j-i+1) nonPrefix
  in prefix ++ reverse inRange ++ postfix

-- |Create an SA problem definition for a given TSP and cooling schedule.
mkSATSPProblem :: SA.CoolingSchedule -> TSP -> SA.ProblemDef Tour [City]
mkSATSPProblem cs tsp = SA.ProblemDef
  { SA.successors = \(Tour tour) -> successors tour
  , SA.solutionCost = \(tour:_) -> tourCost tsp tour
  , SA.coolingSchedule = cs
  , SA.maxSteps = 2000000 -- 2M iterations.
  }
  where -- Neighbourhood operator.
        successors tour
          -- No successors for 3 or less nodes.
          | length tour < 4 = []
          -- Create a successor by reversing each possible sublist that starts
          -- after the first item.
          | otherwise = let ranges = [(i, j) | i <- [1..length tour - 2]
                                             , j <- [i+1..length tour - 1]]
                        in map (Tour . uncurry (reverseRange tour)) ranges

-- |Simple cooling schedule. Start at 5000 and decrease linearly to 0.
linearCooling :: SA.CoolingSchedule
linearCooling x = 5*(1000 - x)

-- |Logarithmic cooling schedule (flipped about x and shifted).
logCooling :: SA.CoolingSchedule
logCooling x = 5*(300*log(-x/100 + 30))

-- |Oscillatory cooling schedule.
wavyCooling :: SA.CoolingSchedule
wavyCooling x = 5*(501 - x/4 + (1000 - x/2)/2 * cos(x/5) * sin(x/12))

