{-#LANGUAGE MultiParamTypeClasses, FunctionalDependencies, ScopedTypeVariables #-}
module Search (Searcher(..), Path, Cost, ProblemNode(..), search) where

import Prelude
import Control.Monad.State

type Cost = Double
type Path = []

class Ord k => ProblemNode pn k | pn -> k where
  ident :: pn -> k

class Show n => Searcher s n | s -> n where
  nextNode :: s -> Maybe n
  searchPath :: s -> Path n
  isAtGoal :: s -> Bool
  expandNextNode :: s -> s

search :: Searcher s n => s -> (Maybe (Path n), s)
search = runState search_

search_ :: Searcher s n => State s (Maybe (Path n))
search_ = do
  s <- get
  let next = nextNode s
  if isAtGoal s
     then return $ return $ searchPath s
     else case next of
               Nothing -> return Nothing
               Just _ -> do
                 put $ expandNextNode s
                 search_

