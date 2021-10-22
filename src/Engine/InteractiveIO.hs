{-# LANGUAGE DatatypeContexts #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE NamedFieldPuns, PolyKinds #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards #-}

module Engine.InteractiveIO where

import           Engine.Diagnostics
import           Engine.OpenGames hiding (lift)
import           Engine.OpticClass
import           Engine.TLL

import Data.Foldable
import Data.List (maximumBy)
import Data.Ord (comparing)
import Numeric.Probability.Distribution hiding (map, lift, filter)

type InteractiveStageGame m a b x s y r = OpenGame (MonadOptic m) (MonadContext m) a b x s y r


type Agent = String

support :: Stochastic x -> [x]
support = map fst . decons

bayes :: (Eq y) => Stochastic (x, y) -> y -> Stochastic x
bayes a y = mapMaybe (\(x, y') -> if y' == y then Just x else Nothing) a





deviationsInContext :: (Show x, Show y, Ord y, Show theta, Monad m)
                    => Double -> Agent -> x -> theta -> Stochastic (m y) -> (y -> m Double) -> [y] -> m [DiagnosticInfoBayesian x y]
deviationsInContext epsilon name x theta strategy u ys
  = do
  let strategicPayoff = expected (fmap u strategy)
      (optimalPlay, optimalPayoff) = maximumBy (comparing snd) [(y, u y) | y <- ys]
  payoff <- optimalPayoff
  strategicPayoff' <- strategicPayoff
  pure [DiagnosticInfoBayesian { equilibrium = strategicPayoff' >=  payoff - epsilon,
                      player = name,
                      payoff = strategicPayoff,
                      optimalMove = optimalPlay,
                      optimalPayoff = payoff,
                      context = u ,
                      state = x,
                      unobservedState = show theta,
                      strategy = strategy}]


{--
-- Takes IO and does an evaluate? As in the Bayesian Game?
interactiveInput ::
  Monad m
  => Agent -> [a] -> InteractiveStageGame m '[ m a] '[m b] () () a Double
interactiveInput name ys = OpenGame {
  play =  \(strat ::- Nil) -> let  v obs = do
                                           action' <- strat
                                           pure ((),action')
                                        in MonadOptic v (\_ -> (\_ -> pure ())),
  -- ^ This finds the optimal action or chooses randomly
  evaluate = \(a ::- Nil) (MonadContext _ k) ->
      do
        u <- \y ->  k () y
        (concat [ let u y = k () y
                      strategy = a
                      in deviationsInContext 0 name () () strategy u ys ]) ::- Nil }






 \(strat ::- Nil) (MonadContext h k) ->
              let
                output = do
                   (_,pdenv') <- strat
                   (z,obs) <- h
                   -- ^ Take the (old observation) from the context
                   (action,actionChoiceType) <- chooseActionFunction actionSpace (State pdenv' obs)
                   (reward,obsNew) <- k z action
                   let st = (_obsAgent pdenv')
                   bounds <- liftIO (A.getBounds (_qTable pdenv'))
                   RIO.glog (exportRewards
                                exportType
                                (_player pdenv')
                                (_iteration pdenv')
                                (st, action)
                                (Ix.index bounds (st, toIdx action))
                                actionChoiceType
                                (_exploreRate pdenv')
                                reward)
                   (State env' _) <- ST.execStateT (updateFunction actionSpace (State pdenv' obs) obsNew  (action,actionChoiceType) reward)
                                                   (State pdenv' obs)
                   return (action,env')
                in (output ::- Nil)}



{-# INLINE deterministicStratStage #-}
deterministicStratStage ::  (MonadIO m, MonadReader r m, HasGLogFunc r, GMsg r ~ QLearningMsg n o a) =>  Agent -> (o a -> a) -> InteractiveStageGame m '[m a] '[m a] (o a) () a  (Double,(o a))
deterministicStratStage name policy = OpenGame {
  play =  \(_ ::- Nil) -> let v obs = pure $ ((),policy obs)
                              in MonadOptic v (\_ -> (\_ -> pure ())),
  evaluate = \(a ::- Nil) (MonadContext h k) ->
              let
                output = do
                   (_,obs) <- h
                   -- ^ Take the (old observation) from the context
                   pure $ policy obs
                in (output ::- Nil)}

-}
