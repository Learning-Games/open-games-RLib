{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE NamedFieldPuns #-}
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
{-# LANGUAGE DeriveGeneric #-}

module Examples.QLearning.AsymmetricLearners.MarkovInternal
  where

import           Examples.QLearning.AsymmetricLearners.Internal
import           Engine.Diagnostics
import           Engine.QLearning.ImportAsymmetricLearners
import           Engine.QLearningToBayesian
import           Engine.OpenGames
import           Engine.OpticClass
import           Engine.BayesianGames
import           Engine.TLL
import           Preprocessor.Compile

import           Control.Arrow (Kleisli)
import           Control.Monad.State  hiding (state,void)
import qualified Control.Monad.State  as ST
import           Data.Csv


-------------------------------------------------------
-- Uses the learned policy to check standard equilibria
-------------------------------------------------------

-- TODO test on concrete game result
-- TODO adapt the import facility

----------------------------------------
-- 0. Import strategies from the outside
strategyImport :: (FromField s, FromField a, FromField s, FromField a)
               => FilePath
               -> FilePath
               -> IO
                    (Either
                      String
                      (List '[ Kleisli Stochastic s a
                             , Kleisli Stochastic s a]))
strategyImport filePathState filePathQMatrix = do
  p1 <- importQMatrixAndStateIndex filePathState filePathQMatrix 1
  p2 <- importQMatrixAndStateIndex filePathState filePathQMatrix 2
  case p1 of
    Left str -> return $ Left str
    Right lsQValues1 ->
      case p2 of
        Left str' -> return $ Left str'
        Right lsQValues2 ->
           return $ Right (fromListToStrategy lsQValues1 ::- fromListToStrategy lsQValues2 ::- Nil)


-- 1. The stage game 
stageMC actionSpace1 actionSpace2 parameters discountFactor = [opengame|
   inputs    : state ;
   feedback  :      ;

   :-----------------:
   inputs    :  state    ;
   feedback  :      ;
   operation : dependentDecision "Player1" (const actionSpace1) ;
   outputs   :  p1 ;
   returns   :  profit1 parameters p1 p2 ;

   inputs    : state     ;
   feedback  :      ;
   operation : dependentDecision "Player2" (const actionSpace2)  ;
   outputs   :  p2 ;
   returns   :  profit2 parameters p1 p2    ;

   operation : discount "player1" (\x -> x * discountFactor) ;

   operation : discount "player2" (\x -> x * discountFactor) ;

   :-----------------:

   outputs   :  (Obs (p1, p2))    ;
   returns   :      ;

|]


-- 2. Evaluate the strategies one shot
evaluateLearnedStrategiesOneShot :: FilePath
                                 -> FilePath
                                 -> [PriceSpace]
                                 -> [PriceSpace]
                                 -> Parameters
                                 -> Double
                                 -> (Observation PriceSpace)
                                 -> IO ()
evaluateLearnedStrategiesOneShot filePathState filePathQMatrix actionSpace1 actionsSpace2 parameters discountFactor initialState  = do
  strat <- strategyImport filePathState filePathQMatrix
  case strat of
    Left str -> print str
    Right strat' -> generateIsEq $ evaluate (stageMC actionSpace1 actionsSpace2 parameters discountFactor) strat' (StochasticStatefulContext (pure ((),(initialState))) (\_ _ -> pure ()))

-- 3. Evaluate the strategies in the Markov game
-- extract continuation
extractContinuation :: StochasticStatefulOptic s () a () -> s -> StateT Vector Stochastic ()
extractContinuation (StochasticStatefulOptic v u) x = do
  (z,a) <- ST.lift (v x)
  u z ()

-- extract next state (action)
extractNextState :: StochasticStatefulOptic s () a () -> s -> Stochastic a
extractNextState (StochasticStatefulOptic v _) x = do
  (z,a) <- v x
  pure a


-- determine continuation for iterator, with the same repeated strategy
determineContinuationPayoffs :: Integer
                             -> List
                                '[Kleisli Stochastic (Observation PriceSpace) PriceSpace,
                                  Kleisli Stochastic (Observation PriceSpace) PriceSpace]
                             -> Observation PriceSpace
                             -> [PriceSpace]
                             -> [PriceSpace]
                             -> Parameters
                             -> Double
                             -> StateT Vector Stochastic ()
determineContinuationPayoffs 1        strat action actionSpace1 actionsSpace2 parameters discountFactor = pure ()
determineContinuationPayoffs iterator strat action actionSpace1 actionsSpace2 parameters discountFactor = do
   extractContinuation executeStrat action
   nextInput <- ST.lift $ extractNextState executeStrat action
   determineContinuationPayoffs (pred iterator) strat nextInput actionSpace1 actionsSpace2 parameters discountFactor
 where executeStrat =  play (stageMC actionSpace1 actionsSpace2 parameters discountFactor) strat


-- Repeated game
repeatedGameEq iterator strat initialAction actionSpace1 actionsSpace2 parameters discountFactor = evaluate (stageMC actionSpace1 actionsSpace2 parameters discountFactor) strat context
  where context  = StochasticStatefulContext (pure ((),initialAction)) (\_ action -> determineContinuationPayoffs iterator strat action actionSpace1 actionsSpace2 parameters discountFactor)


-- Eq output for the repeated game
eqOutput iterator strat initialAction actionSpace1 actionsSpace2 parameters discountFactor = generateIsEq $ repeatedGameEq iterator strat initialAction actionSpace1 actionsSpace2 parameters discountFactor


-- Evaluate the strategies as an approximation of the Markov game
evaluateLearnedStrategiesMarkov iterator initialAction filePathState filePathQMatrix actionSpace1 actionsSpace2 parameters discountFactor   = do
  strat <- strategyImport filePathState filePathQMatrix
  case strat of
    Left str -> print str
    Right strat' -> eqOutput iterator strat' initialAction actionSpace1 actionsSpace2 parameters discountFactor

