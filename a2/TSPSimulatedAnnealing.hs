module TSPSimulatedAnnealing where

import qualified System.Random as Random

import qualified Search
import qualified SimulatedAnnealing as SA
import TSP

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

