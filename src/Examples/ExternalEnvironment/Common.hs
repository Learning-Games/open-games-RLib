module Examples.ExternalEnvironment.Common (extractPayoff, extractNextState) where

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
