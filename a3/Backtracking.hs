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

instance Applicative m => Applicative (BacktrackingT persist soln m) where
  pure a = BacktrackingT $ \p s -> pure (Right a, p, s)
  apFunc <*> apVal = BacktrackingT (\p s ->
      (\(r', p', s') -> case r' of
         Left bool -> const (Left bool, p', s') -- Rewrap because of type change (Either Bool b) not (Either Bool (a -> b))
         Right f -> \(r'', p'', s'') -> (fmap f r'', p'', s'')
      ) <$> runBacktrackingT apFunc p s <*> runBacktrackingT apVal p s
    )

instance Monad m => Monad (BacktrackingT persist soln m) where
  return = pure
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

