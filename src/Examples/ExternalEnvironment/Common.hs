module Examples.ExternalEnvironment.Common (extractNextState, extractPayoffAndNextState) where

import Engine.Engine (MonadOptic(..))

-- TODO: These probably belong elsewhere (where?)

-- extract next state (action)
extractNextState :: MonadOptic m s t a b -> s -> m a
extractNextState (MonadOptic v _) x = do
  (_,a) <- v x
  pure a

-- extract both the next state and the continuation
extractPayoffAndNextState :: MonadOptic m s t a b -> s -> b -> m (t, a)
extractPayoffAndNextState (MonadOptic v u) x r = do
  (z, nextState) <- v x
  payoff <- u z r
  return (payoff, nextState)

