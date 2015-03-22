module Main where

import Data.Array (Array, (!))
import qualified Data.Array as Array
import Control.Applicative
import Data.List (maximumBy, sortBy)
import Data.Ord (comparing)
import Control.Arrow
import Data.Monoid
import Control.Monad (when, forM_)
import Text.Printf (printf)

import Debug.Trace (trace)
traceShow a = trace (show a) a

type Coords = (Int, Int)
type Grid = Array Coords Float
type Epsilon = Float

coordAdd (x, y) (x', y') = (x + x', y + y')
coordSub (x, y) (x', y') = (x - x', y - y')

gridBounds = ((0, 0), (2, 2))
xRange = [fst (fst gridBounds)..fst (snd gridBounds)]
yRange = [snd (fst gridBounds)..snd (snd gridBounds)]

makeSimpleGrid :: Float -> Grid
makeSimpleGrid r = Array.array gridBounds $
  zipWith ($)
  [(,) (x, y) | y <- reverse yRange, x <- xRange]
  [ r, -1, 10,
   -1, -1, -1,
   -1, -1, -1 ]

zeroGrid :: Grid
zeroGrid = Array.array gridBounds [((x, y), 0.0) | y <- yRange, x <- xRange]

endState :: Coords
endState = (2, 2)

maxIters :: Int
maxIters = 25

neighbours :: Grid -> Coords -> [Coords]
neighbours g (x, y) =
  filter
    (Array.inRange (Array.bounds g))
    [(x + dx, y + dy) | (dx, dy) <- [(-1, 0), (1, 0), (0, -1), (0, 1)]]

policy :: Grid -> Coords -> Coords
policy g start =
  let end = fst $ maximumBy (comparing snd) $ map (id &&& (g !)) $ neighbours g start
  in coordSub end start

movementProb :: Grid -> Coords -> Coords -> Float
movementProb g start end =
  let delta = policy g start
      prob | start == endState = 0
           | coordAdd start delta == end = 0.8
           | coordSub start delta == end = 0.1
           | otherwise = 0.0
  in prob

valueIteration :: Grid -> Grid -> Float -> Epsilon -> Int -> (Grid, Epsilon, Int)
valueIteration _ utility _ _ 0 = (utility, 0, 0)
valueIteration rewards utility discount epsilon its =
  let max_neighbour_utility coords =
        maximum [ movementProb utility coords nb * (utility ! nb)
                | nb <- neighbours utility coords
                ]
      new_utility_assocs = [ (coords, r + discount * max_neighbour_utility coords)
                           | (coords, r) <- Array.assocs rewards
                           ]
      new_utility = Array.array (Array.bounds rewards) new_utility_assocs
      max_delta :: Float
      max_delta = maximum $
                    map (abs . uncurry (-)) $
                      zip (Array.elems utility) (map snd new_utility_assocs)
      answer | its == 1 = (new_utility, max_delta, 1)
             | max_delta < epsilon*(1.0-discount)/discount = (new_utility, max_delta, 1)
             | otherwise = let (utility', epsilon', its') =
                                 valueIteration rewards new_utility discount epsilon (its - 1)
                           in (utility', epsilon', its' + 1)
  in answer

prettyPrintGrid :: Grid -> ((Coords, Float) -> IO ()) -> IO ()
prettyPrintGrid grid fmt =
  forM_ (sortBy (comparing ((0-).snd.fst) <> comparing (fst.fst)) $ Array.assocs grid) fmt

prettyPrintGridValues :: Grid -> IO ()
prettyPrintGridValues g = prettyPrintGrid g $ \((x, _), u) -> do
  putStr $ printf "%8.4f, " u
  when (x == fst (snd gridBounds)) $ putStr "\n"

prettyPrintPolicy :: Grid -> IO ()
prettyPrintPolicy g = prettyPrintGrid g $ \((x, y), u) -> do
  putStr $ case policy g (x, y) of
   (0, 1) -> "↑"
   (0, -1) -> "↓"
   (1, 0) -> "→"
   (-1, 0) -> "←"
  if (x == fst (snd gridBounds))
     then putStr "\n"
     else putStr " "

prettyPrint :: Float -> (Grid, Epsilon, Int) -> IO ()
prettyPrint r (utility, epsilon, iters) = do
  putStrLn $ "Problem with r = " ++ show r ++ ":"
  prettyPrintGridValues $ makeSimpleGrid r
  putStrLn $ "Stopped after " ++ show iters ++ " iterations."
  putStrLn $ "Epsilon = " ++ show epsilon ++ "."
  putStrLn $ "Utility:"
  prettyPrintGridValues utility
  putStrLn $ "Policy:"
  prettyPrintPolicy utility
  putStr "\n"

solve :: Float -> (Grid, Epsilon, Int)
solve r = valueIteration (makeSimpleGrid r) zeroGrid 0.9 0.01 maxIters

solveAndPrint :: Float -> IO ()
solveAndPrint = uncurry ($) . (prettyPrint &&& solve)

main :: IO ()
main = mapM_ solveAndPrint [a_r, b_r, c_r, d_r]
  where a_r = 100
        b_r = -3
        c_r = 0
        d_r = 3


