module Examples.ExternalEnvironment.Common (extractPayoff, extractNextState, extractPayoffAndNextState) where

import Engine.Engine (MonadOptic(..))

-- TODO: These probably belong elsewhere (in Engine.OpticClass perhaps?)

-- extract continuation
extractPayoff :: MonadOptic m () t a () -> m t
extractPayoff (MonadOptic v u) = do
  (z,_) <- v ()
  u z ()

-- extract next state (action)
extractNextState :: MonadOptic m () t a b -> m a
extractNextState (MonadOptic v _) = do
  (_,a) <- v ()
  pure a

-- extract both the next state and the continuation
extractPayoffAndNextState :: MonadOptic m () t a () -> m (t, a)
extractPayoffAndNextState (MonadOptic v u) = do
  (z, nextState) <- v ()
  payoff <- u z ()
  return (payoff, nextState)

