module Examples.ExternalEnvironment.Common (extractNextState, extractPayoffAndNextState) where

import Engine.Engine (MonadOptic(..))

---------------------------------------------------------------------
-- Utility functions to extract the next state and payoff from a
-- monad optic. TODO: Perhaps there is a better location for these
-- functions.
---------------------------------------------------------------------

extractNextState :: MonadOptic m s t a b -> s -> m a
extractNextState (MonadOptic v _) x = do
  (_, nextState) <- v x
  pure nextState

extractPayoffAndNextState :: MonadOptic m s t a b -> s -> b -> m (t, a)
extractPayoffAndNextState (MonadOptic v u) x r = do
  (z, nextState) <- v x
  payoff <- u z r
  pure (payoff, nextState)

