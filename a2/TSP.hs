{-#LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}
module TSP where

import Prelude
import Data.List (sort, intercalate)
import qualified Data.Map as Map
import qualified Data.Set as Set

import qualified Search

-- |Representations for TSP.
-- A City is represented by a its name.
-- A tour is a list of Cities.
-- A TSP is a list of cities and a mapping from each city to its coordinates.
type City = String
newtype Tour = Tour [City]
data TSP = TSP [City] (Map.Map City (Int, Int))

-- |How to display a Tour as a human-readable string.
instance Show Tour where
  show (Tour t) = intercalate "," $ reverse t

-- |Making a Tour a ProblemNode where it is identified by an ordered list
-- of visited cities.
instance Search.ProblemNode Tour [City] where
  ident (Tour tour) = tour

-- |Format for representing the input to the problem.
type CityInfo = (City, Int, Int)

-- |Get the cities of a TSP.
tspCities :: TSP -> [City]
tspCities (TSP cities _) = cities

-- |Get a city's coordinates from a TSP.
tspGet :: TSP -> City -> (Int, Int)
tspGet (TSP _ cityMap) c = cityMap Map.! c

-- |Create a TSP from a bunch of city information.
makeTSP :: [CityInfo] -> TSP
makeTSP = do
  orderedCityNames <- sort . map (\(c, _, _) -> c)
  cityMap <- Map.fromList . map (\(c, x, y) -> (c, (x, y)))
  return $ TSP orderedCityNames cityMap

-- |The Euclidean distance between two cities in a given TSP.
cityDist :: TSP -> City -> City -> Search.Cost
cityDist tsp a b = let (x1, y1) = tspGet tsp a
                       (x2, y2) = tspGet tsp b
                       square x = x*x
                   in sqrt $ fromIntegral $ square (x2 - x1) + square (y2 - y1)

-- |The cost of a possibly incomplete tour. If the tour is partial then it's
-- the sum of the distance between pairwise cities. A tour shouldn't have any
-- duplicate cities - if it includes all cities then it's considered a complete
-- tour that revisits the start node and so this return distance is included.
tourCost :: TSP -> Tour -> Search.Cost
-- Zero or one cities has a cost of 0.
tourCost _ (Tour []) = 0
tourCost _ (Tour (_:[])) = 0
tourCost tsp (Tour tour@(a:b:_))
  -- Partial tour is the distance between the first two cities plus the
  -- cost of the tour that excludes the first city.
  | length tour < length (tspCities tsp) = cityDist tsp a b
                                         + tourCost tsp (Tour $ tail tour)
  -- Complete tour is like the above except it also includes an edge from the
  -- last city to the first city.
  | otherwise = cityDist tsp a (last tour) + cityDist tsp a b
              + tourCost tsp (Tour $ tail tour)

-- |Get the cities from a TSP that are not in a given list.
notYetVisited :: TSP -> [City] -> [City]
notYetVisited tsp tour = let visitedSet = Set.fromList tour
                     in filter (not . (`Set.member` visitedSet)) $ tspCities tsp

-- |The city to start the search from.
startCity :: City
startCity = "A"

