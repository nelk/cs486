module Main where

import System.Environment
import Data.Array (Array, (!))
import qualified Data.Array as Array
import Data.List (maximumBy, sortBy)
import Data.Ord (comparing)
import Control.Arrow
import Data.Monoid
import Control.Monad (forM_)
import Text.Printf (printf)

type Coords = (Int, Int)
type Grid = Array Coords Float
type Epsilon = Float

-- |Elementary coordinate operations.
coordAdd :: Coords -> Coords -> Coords
coordAdd (x, y) (x', y') = (x + x', y + y')

coordSub :: Coords -> Coords -> Coords
coordSub (x, y) (x', y') = (x - x', y - y')

-- |Bounds and ranges of the grid for these problems.
gridBounds :: (Coords, Coords)
gridBounds = ((0, 0), (2, 2))

xRange :: [Int]
xRange = [fst (fst gridBounds)..fst (snd gridBounds)]

yRange :: [Int]
yRange = [snd (fst gridBounds)..snd (snd gridBounds)]

-- |Construct the problem format of the grid with only one cell varying.
makeSimpleGrid :: Float -> Grid
makeSimpleGrid r = Array.array gridBounds $
  zipWith ($)
  [(,) (x, y) | y <- reverse yRange, x <- xRange]
  [ r, -1, 10,
   -1, -1, -1,
   -1, -1, -1 ]

-- |Empty grid.
zeroGrid :: Grid
zeroGrid = Array.array gridBounds [((x, y), 0.0) | y <- yRange, x <- xRange]

-- |Position at which you can't move out of.
endCoords :: Coords
endCoords = (2, 2)

maxIters :: Int
maxIters = 25

-- |Given a utility grid, start position, and delta position, gives the expected
-- utility of making this move.
getNeighbourUtil :: Grid -> Coords -> Coords -> Float
getNeighbourUtil g start delta =
  let end = coordAdd start delta
      -- If out of range, stay at end.
      end' | Array.inRange (Array.bounds g) end = end
           | otherwise = start
  -- Lookup value in grid.
  in g ! end'

-- |All possible movement deltas.
movementDeltas :: [Coords]
movementDeltas = [(-1, 0), (1, 0), (0, -1), (0, 1)]

-- |Compute policy by finding the movement delta which gives maximum utility.
getPolicy :: Grid -> Coords -> Coords
getPolicy g start = fst $ maximumBy (comparing snd) $ map (id &&& getNeighbourUtil g start) movementDeltas

-- |Compute the probability of making a move based off the policy delta.
movementProb :: Coords -> Coords -> Float
movementProb pd@(pdx, pdy) delta
  -- Going in the same direction as the policy 80% of the time.
  | delta == pd = 0.8
  -- Never move backwards.
  | delta == (-pdx, -pdy) = 0.0
  | otherwise = 0.1

-- |Main value iteration algorithm.
-- Given a reward grid, initial utility grid, discounting factor, convergence
-- epsilon, and maximum number of iterations, it returns the expected utility
-- of starting at each place, the epsilon it ended at, and the number of
-- iterations run.
valueIteration :: Grid -> Grid -> Float -> Epsilon -> Int -> (Grid, Epsilon, Int)
valueIteration _ utility _ _ 0 = (utility, 0, 0)
valueIteration rewards utility discount epsilon its =
  let -- Computes the utility of using a particular policy at a given location.
      policy_utility policy start =
        sum [ movementProb policy delta * getNeighbourUtil utility start delta
            | delta <- movementDeltas
            ]
      -- Computes the maximum utility of using any policy at a given location.
      max_policy_utility start =
        maximum [ policy_utility policy start
                | policy <- movementDeltas
                ]
      -- Association list for the grid after applying a Bellman Update to each cell.
      new_utility_assocs = [ (coords, r + (if coords == endCoords then 0 else discount * max_policy_utility coords))
                           | (coords, r) <- Array.assocs rewards
                           ]
      -- New utility grid after applying Bellman Update.
      new_utility = Array.array (Array.bounds rewards) new_utility_assocs
      -- Calculating largest change in utility to compare to epsilon.
      max_delta = maximum $
                    map (abs . uncurry (-)) $
                      zip (Array.elems utility) (map snd new_utility_assocs)
      -- Resulting values. If on last iteration, return the new grid.
      answer | its == 1 = (new_utility, max_delta, 1)
             -- If delta was within epsilon, end as well.
             | max_delta < epsilon*(1.0-discount)/discount = (new_utility, max_delta, 1)
             -- Otherwise& recurse with new utility grid.
             | otherwise = let (utility', epsilon', its') =
                                 valueIteration rewards new_utility discount epsilon (its - 1)
                           in (utility', epsilon', its' + 1)
  in answer

-- |Printing functions.
prettyPrintGrid :: Grid -> ((Coords, Float) -> IO ()) -> IO ()
prettyPrintGrid grid =
  forM_ (sortBy (comparing ((0-).snd.fst) <> comparing (fst.fst)) $ Array.assocs grid)

prettyPrintGridValues :: Grid -> String -> String -> IO ()
prettyPrintGridValues g sep endl = prettyPrintGrid g $ \((x, _), u) -> do
  putStr $ printf "%8.4f" u
  putStr $ if x == fst (snd gridBounds) then endl else sep

aztexPrintPolicy :: Grid -> IO ()
aztexPrintPolicy g = prettyPrintGrid g $ \((x, y), _) -> do
  putStr $ case getPolicy g (x, y) of
   (0, 1) -> "$uparrow"
   (0, -1) -> "$downarrow"
   (1, 0) -> "$rightarrow"
   (-1, 0) -> "$leftarrow"
   _ -> "?"
  putStr $ if x == fst (snd gridBounds) then "\\\\\n" else " & "

prettyPrintPolicy :: Grid -> IO ()
prettyPrintPolicy g = prettyPrintGrid g $ \((x, y), _) -> do
  putStr $ case getPolicy g (x, y) of
   (0, 1) -> "↑"
   (0, -1) -> "↓"
   (1, 0) -> "→"
   (-1, 0) -> "←"
   _ -> "?"
  putStr $ if x == fst (snd gridBounds) then "\n" else " "

prettyPrint :: Bool -> Float -> (Grid, Epsilon, Int) -> IO ()
prettyPrint aztex r (utility, epsilon, iters) = do
  let printGrid | aztex = \g -> prettyPrintGridValues g " & " "\\\\\n"
                | otherwise = \g -> prettyPrintGridValues g ", " "\n"
  putStrLn $ "Problem with r = " ++ show r ++ ":"
  printGrid (makeSimpleGrid r)
  putStrLn $ "Stopped after " ++ show iters ++ " iterations."
  putStrLn $ "Epsilon = " ++ show epsilon ++ "."
  putStrLn "Utility:"
  printGrid utility
  putStrLn "Policy:"
  (if aztex then aztexPrintPolicy else prettyPrintPolicy) utility
  putStr "\n"

-- |Perform value iteration on a grid with the given "r" value and 0.9 discounting factor.
solve :: Float -> (Grid, Epsilon, Int)
solve r = valueIteration (makeSimpleGrid r) zeroGrid 0.9 0.01 maxIters

solveAndPrint :: Bool -> Float -> IO ()
solveAndPrint aztex = uncurry ($) . (prettyPrint aztex &&& solve)

main :: IO ()
main = do
  args <- getArgs
  let aztex = "--aztex" `elem` args
  mapM_ (solveAndPrint aztex) [a_r, b_r, c_r, d_r]
  -- Specific "r" values.
  where a_r = 100
        b_r = -3
        c_r = 0
        d_r = 3

