{-#LANGUAGE MultiParamTypeClasses, FunctionalDependencies, ScopedTypeVariables #-}
module Search (Searcher, Path, expandNextNode, searchPath, nextNode, isAtGoal, search) where

import Prelude
import Control.Monad.State

import Debug.Trace

type Path = []

class Show n => Searcher s n | s -> n where
  nextNode :: s -> Maybe n
  searchPath :: s -> Path n
  isAtGoal :: s -> Bool
  expandNextNode :: s -> s

search :: Searcher s n => s -> Maybe (Path n)
search = evalState search_

search_ :: Searcher s n => State s (Maybe (Path n))
search_ = do
  s <- get
  let next = nextNode s
  if isAtGoal s
     then return $ return $ searchPath s
     else case next of
               Nothing -> return Nothing
               Just n -> {-trace (show n) $-} do
                 put $ expandNextNode s
                 search_

