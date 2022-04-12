module Examples.ExternalEnvironment.Common (extractPayoff, extractNextState, extractPayoffAndNextState) where

import Engine.Engine (MonadOptic(..))

-- TODO: These probably belong elsewhere (in Engine.OpticClass perhaps?)

-- NOTE: Both extractPayoff and extractNextState invoke the first optic (v) so
-- if you run them twice the effects are duplicated. If you need both the next
-- state and the payoff, run extractPayoffAndNextState, which executes the
-- effects only once.

-- extract continuation
extractPayoff :: MonadOptic m s t a b -> s -> b -> m t
extractPayoff (MonadOptic v u) x r = do
  (z,_) <- v x
  u z r

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

