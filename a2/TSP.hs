{-#LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}
module TSP where

import Prelude
import Data.List (sort)
import qualified Data.Map as Map
import qualified Data.Set as Set

import qualified Search

type City = Char
newtype Tour = Tour [City]
data TSP = TSP [City] (Map.Map City (Int, Int))

instance Show Tour where
  show (Tour t) = reverse t

instance Search.ProblemNode Tour [City] where
  ident (Tour tour) = tour

type CityInfo = (City, Int, Int)

tspCities :: TSP -> [City]
tspCities (TSP cities _) = cities

tspGet :: TSP -> City -> (Int, Int)
tspGet (TSP _ cityMap) c = cityMap Map.! c

makeTSP :: [CityInfo] -> TSP
makeTSP = do
  orderedCityNames <- sort . map (\(c, _, _) -> c)
  cityMap <- Map.fromList . map (\(c, x, y) -> (c, (x, y)))
  return $ TSP orderedCityNames cityMap

cityDist :: TSP -> City -> City -> Search.Cost
cityDist tsp a b = let (x1, y1) = tspGet tsp a
                       (x2, y2) = tspGet tsp b
                       square x = x*x
                   in sqrt $ fromIntegral $ square (x2 - x1) + square (y2 - y1)

-- Only includes going back to goal if tour is of same length as number of cities.
tourCost :: TSP -> Tour -> Search.Cost
tourCost _ (Tour []) = 0
tourCost _ (Tour (_:[])) = 0
tourCost tsp (Tour tour@(a:b:_))
  | length tour < length (tspCities tsp) = cityDist tsp a b + tourCost tsp (Tour $ tail tour)
  | otherwise = cityDist tsp a (last tour) + cityDist tsp a b + tourCost tsp (Tour $ tail tour)

notYetVisited :: TSP -> [City] -> [City]
notYetVisited tsp tour = let visitedSet = Set.fromList tour
                     in filter (not . (`Set.member` visitedSet)) $ tspCities tsp

startCity :: City
startCity = 'A'

