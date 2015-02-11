module Backtracking where

import Control.Applicative ((<$>))
import Control.Monad.Trans (MonadTrans, lift)

-- |A backtracking computation Monad Transformer.
-- Adds the ability to modify a state until it's an accepted solution
-- while trying multiple branches. It backtracks from failing branches,
-- resetting the solution state. A persistent state is kept even
-- when backtracking.
data BacktrackingT persist soln m a = BacktrackingT
  { runBacktrackingT :: persist -> soln -> m (Either Bool a, persist, soln) }

instance Functor m => Functor (BacktrackingT persist soln m) where
  fmap f m = BacktrackingT $ \p s -> (\(r, p', s') -> (fmap f r, p', s')) <$> runBacktrackingT m p s

instance Monad m => Monad (BacktrackingT persist soln m) where
  return a = BacktrackingT $ \p s -> return (Right a, p, s)
  m >>= f = BacktrackingT $ \p s -> do (r, p', s') <- runBacktrackingT m p s
                                       decide r p' s'
              where decide (Left b) p s = return (Left b, p, s)
                    decide (Right a) p s = runBacktrackingT (f a) p s

instance MonadTrans (BacktrackingT persist soln) where
  lift m = BacktrackingT $ \p s -> do a <- m
                                      return (Right a, p, s)

-- |Try a branch. if it succeeds then keep the solution. If it fails then reset the
-- solution state.
branch :: Monad m => BacktrackingT persist soln m a -> BacktrackingT persist soln m (Maybe a)
branch bt = BacktrackingT $ \p s -> do (r, p', s') <- runBacktrackingT bt p s
                                       decide r s p' s'
              where decide (Left True) _ p' s' = return (Left True, p', s')
                    decide (Left False) s p' _ = return (Right Nothing, p', s)
                    decide (Right a) s p' _ = return (Right (Just a), p', s)

-- |Mark computation as successful. The current overall computation will
-- get the current solution.
success :: Monad m => BacktrackingT persist soln m ()
success = BacktrackingT $ \p s -> return (Left True, p, s)

-- |Mark computation as failed. This will begin backtracking to the last
-- branch call or fail the overall computation.
failure :: Monad m => BacktrackingT persist soln m ()
failure = BacktrackingT $ \p s -> return (Left False, p, s)

-- |Helper to return the persistent state of this computation.
getPersistent :: Monad m => BacktrackingT persist soln m persist
getPersistent = BacktrackingT $ \p s -> return (Right p, p, s)

-- |Helper to set the persistent state of this computation.
putPersistent :: Monad m => persist -> BacktrackingT persist soln m ()
putPersistent p = BacktrackingT $ \_ s -> return (Right (), p, s)

-- |Helper to get the solution state of this computation.
getSoln :: Monad m => BacktrackingT persist soln m soln
getSoln = BacktrackingT $ \p s -> return (Right s, p, s)

-- |Helper to set the solution state of this computation.
putSoln :: Monad m => soln -> BacktrackingT persist soln m ()
putSoln s = BacktrackingT $ \p _ -> return (Right (), p, s)

