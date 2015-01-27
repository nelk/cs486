
import Prelude
import System.IO
import Text.Regex
import Data.List
import Data.Function
import Graphics.Rendering.Chart.Easy
import Graphics.Rendering.Chart.Backend.Cairo

data Instance = Instance
  { numCities :: Int
  , solution :: String
  , cost :: Double
  , numExpanded :: Int
  , timeTaken :: Double
  }

parseInstance :: IO Instance
parseInstance = do
  instanceText <- getLine
  solutionText <- getLine
  costText <- getLine
  numProcessedText <- getLine
  timeText <- getLine
  return Instance
    { numCities = let Just (n:_) = matchRegex numCitiesRegex instanceText in read n
    , solution = let Just (soln:_) = matchRegex solnRegex solutionText in soln
    , cost = 0
    , numExpanded = let Just (n:_) = matchRegex nodesRegex numProcessedText in read n
    , timeTaken = let Just (t:_) = matchRegex timeRegex timeText in read t
    }
  where numCitiesRegex = mkRegex "Running randTSP/([0-9]+)/instance_([0-9]+)\\.txt"
        solnRegex = mkRegex "Solution: ([A-Z]+)$"
        -- nodesRegex = mkRegex "Processed Nodes: ([0-9]+)$"
        nodesRegex = mkRegex "Processed: ([0-9]+), Successors: ([0-9]+)$"
        timeRegex = mkRegex "astar *([0-9]+\\.[0-9]+)s user"

parseAllInstances :: IO [Instance]
parseAllInstances = do
  eof <- isEOF
  if eof
     then return []
     else do
       inst <- parseInstance
       rest <- parseAllInstances
       return (inst:rest)

averageInt :: [Int] -> Double
averageInt ls = fromIntegral (sum ls) / fromIntegral (length ls)

averageDouble :: [Double] -> Double
averageDouble ls = sum ls / fromIntegral (length ls)

main :: IO ()
main = do
  instances <- parseAllInstances
  let sortedInstances = sortBy (compare `on` numCities) instances
      groupedInstances = groupBy ((==) `on` numCities) sortedInstances
      averagedExpandedNodes :: [(Int, Double, Double)]
      averagedExpandedNodes = map (\insts -> (numCities $ head insts, averageInt $ map numExpanded insts, averageDouble $ map timeTaken insts)) groupedInstances
  putStrLn $ averagedExpandedNodes >>= (\(x, y, z) -> show x ++ ", " ++ show y ++ ", " ++ show z ++ "\n")

{-
  toFile def "astar_data.png" $ do
    layout_title .= "AStar for TSP - Expanded Nodes vs Problem Size"
    -- layout_x_axis . laxis_generate .= scaledAxis def (2.0, 16.0)
    layout_y_axis . laxis_generate .= autoScaledLogAxis def
    plot (line "AStar" [averagedExpandedNodes])
    -- plot (points "AStar" averagedExpandedNodes)
-}

