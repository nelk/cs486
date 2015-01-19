{-#LANGUAGE MultiParamTypeClasses, FlexibleInstances, ScopedTypeVariables #-}
module Main where

import Prelude
import System.IO
import Control.Monad
import qualified AStar
import Text.Read
import Safe
import qualified Data.Set as Set
import qualified Data.Map as Map

import Debug.Trace

type City = Char
newtype Tour = Tour [City] deriving Show

instance AStar.ProblemNode Tour [City] where
  ident (Tour tour) = tour

startCity :: City
startCity = 'A'

-- For this problem, ProblemNodes are actually tours.
mkTSPProblem :: [CityInfo] -> AStar.ProblemDef Tour [City]
mkTSPProblem cities = AStar.ProblemDef
  { AStar.isGoalNode = isGoalNode
  , AStar.neighbours = neighbours
  , AStar.costSoFar = \(tour:_) -> costSoFar tour
  , AStar.heuristicCostToEnd = heuristicCostToEnd
  }
  where cityMap = Map.fromList $ map (\(c, x, y) -> (c, (x, y))) cities
        notYetVisited :: [City] -> [City]
        notYetVisited tour = let visitedSet = Set.fromList tour
                             in filter (not . (`Set.member` visitedSet)) $ Map.keys cityMap

        isGoalNode (Tour tour) = length tour == length cities
        neighbours (Tour tour) = map (Tour . (:tour)) $ notYetVisited tour

        cityDist :: City -> City -> AStar.Cost
        cityDist a b = let (x1, y1) = cityMap Map.! a
                           (x2, y2) = cityMap Map.! b
                           cost = sqrt $ fromIntegral $ (x2 - x1)^2 + (y2 - y1)^2
                       in cost

        nonLoopCostSoFar [] = 0
        nonLoopCostSoFar (_:[]) = 0
        nonLoopCostSoFar (a:b:rest) = cityDist a b + nonLoopCostSoFar (b:rest)

        costSoFar (Tour tour@(a:b:rest)) = cityDist (last (b:rest)) a + nonLoopCostSoFar tour
        costSoFar (Tour tour) = nonLoopCostSoFar tour

        -- TODO.
        heuristicCostToEnd (Tour tour) = fromIntegral $ length cities - length tour


type CityInfo = (City, Int, Int)

main :: IO ()
main = do
  numCitiesStr <- getLine
  case readMaybe numCitiesStr :: Maybe Int of
   Nothing -> hPutStrLn stderr "Error - number of cities is not an integer!"
   Just numCities -> do
     cityInfoMaybes <- replicateM numCities readAndParseCity
     case sequence cityInfoMaybes of
      Nothing -> hPutStrLn stderr "Failed."
      Just cityInfos -> let problem = mkTSPProblem cityInfos
                            maybeSoln = AStar.aStarSearch problem (Tour [startCity])
                        in maybe (hPutStrLn stderr "Failed to find a solution") (prettyPrintSoln problem) maybeSoln

  where prettyPrintSoln :: AStar.ProblemDef Tour [City] -> [Tour] -> IO ()
        prettyPrintSoln problem tours =
          let (Tour tour) = last tours
              soln = reverse tour
              totalCost = AStar.costSoFar problem [Tour tour]
          in putStrLn $ "Solution: " ++ soln ++ "\nCost: " ++ show totalCost

        readAndParseCity :: IO (Maybe CityInfo)
        readAndParseCity = do
          l <- getLine
          case parseCity l of
             Nothing -> hPutStrLn stderr ("Failed to parse " ++ l) >> return Nothing
             Just cityInfo -> return $ Just cityInfo

        parseCity :: String -> Maybe CityInfo
        parseCity s = case words s of
                         (ident_s:x_s:y_s:[]) -> do
                           ident <- headMay ident_s
                           x :: Int <- readMaybe x_s
                           y :: Int <- readMaybe y_s
                           return (ident, x, y)
                         _ -> Nothing


