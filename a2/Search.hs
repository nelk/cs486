{-#LANGUAGE MultiParamTypeClasses, FunctionalDependencies, ScopedTypeVariables #-}
module Search (Searcher(..), Path, Cost, ProblemNode(..), search) where

import Prelude
import Control.Monad.State

type Cost = Double
type Path = []

-- |Interface for types that represent states in the problem space that will
-- be searched through.
class Ord k => ProblemNode pn k | pn -> k where
  ident :: pn -> k

-- |Interface for a type that implements a searching algorithm.
class Show n => Searcher s n | s -> n where
  -- Returns the next problem node to consider.
  nextNode :: s -> Maybe n
  -- Returns the sequence of problem states traversed so far.
  searchPath :: s -> Path n
  -- Returns True if the next node is a goal node.
  isAtGoal :: s -> Bool
  -- Expands the next node and returns the updated searcher's state.
  expandNextNode :: s -> s

-- |Use a provided searcher to possibly find a solution.
search :: Searcher s n => s -> (Maybe (Path n), s)
search = runState search_

-- |Internal implementation for generic searching.
search_ :: Searcher s n => State s (Maybe (Path n))
search_ = do
  -- Get the current state of the searcher.
  s <- get
  let next = nextNode s
  if isAtGoal s
     -- If we are at a goal state then we are done, return the path we took
     -- to get there.
     then return $ return $ searchPath s
     -- Otherwise we fail if there is no next node or recurse after expanding
     -- the next node.
     else case next of
               Nothing -> return Nothing
               Just _ -> do
                 put $ expandNextNode s
                 search_

