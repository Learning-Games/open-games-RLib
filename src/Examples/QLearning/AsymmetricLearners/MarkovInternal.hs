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

import           Engine.Diagnostics
import           Engine.QLearning.ImportAsymmetricLearners
import           Examples.QLearning.AsymmetricLearners.Internal (Parameters(..),demand1,demand2)
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
import           Data.Void (Void(..))
import           GHC.Generics
import           Text.Megaparsec
import           Text.Megaparsec.Char (char, string, space, digitChar, newline)
import           System.Random (StdGen)


-------------------------------------------------------
-- Uses the learned policy to check standard equilibria
-------------------------------------------------------

-- TODO test on concrete game result
-- TODO adapt the import facility

profit1 :: Parameters -> Action -> Action -> Double
profit1 par@Parameters {..} p1   p2 = (p1 - pC1)* (demand1 par p1 p2)

-- profit player 2
profit2 :: Parameters -> Action -> Action -> Double
profit2 par@Parameters {..} p1  p2 = (p2 - pC2)* (demand2 par p1 p2)

dist1 :: Parameters -> Double
dist1 par =  (upperBound1 par - lowerBound1 par) / pM1 par

lowerBound1,upperBound1 :: Parameters -> Double
lowerBound1 Parameters{pBertrandPrice1,pKsi,pMonopolyPrice1} = pBertrandPrice1 - pKsi*(pMonopolyPrice1 - pBertrandPrice1)
upperBound1 Parameters{pBertrandPrice1,pKsi,pMonopolyPrice1} = pMonopolyPrice1 + pKsi*(pMonopolyPrice1 - pBertrandPrice1)

actionSpace par = [lowerBound1 par,lowerBound1 par + dist1 par .. upperBound1 par]


-- Data type for importing relevant game information to test for equilibrium
data ImportParameters = ImportParameters
  { parametersGame     :: Parameters
  , initialObservation :: (Action,Action)
  , filePathStateIndex :: FilePath
  , filePathQMatrix    :: FilePath
  , iterations         :: Integer
  }

-- Export type for data storage
data ExportEqAnalysis = ExportEquAnalysis
   { filePath :: FilePath
   , equilibrium :: Bool
   } deriving (Generic)

instance ToField Bool
instance ToRecord ExportEqAnalysis 



----------------------------------------
-- 0. Import strategies from the outside
strategyImport :: FilePath
               -> FilePath
               -> IO
                    (Either
                      (ParseErrorBundle String Data.Void.Void)
                      (List '[ Kleisli Stochastic (Action,Action) Action
                             , Kleisli Stochastic (Action,Action) Action]))
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
stageMC actionSpace1 actionSpace2 parameters = [opengame|
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

   operation : discount "player1" (\x -> x * (pGamma parameters)) ;

   operation : discount "player2" (\x -> x * (pGamma parameters)) ;

   :-----------------:

   outputs   :  (p1, p2)    ;
   returns   :      ;

|]


-- 2. Evaluate the strategies one shot
evaluateLearnedStrategiesOneShot :: FilePath
                                 -> FilePath
                                 -> [Action]
                                 -> [Action]
                                 -> Parameters
                                 -> (Action,Action)
                                 -> IO ()
evaluateLearnedStrategiesOneShot filePathState filePathQMatrix actionSpace1 actionsSpace2 parameters initialState  = do
  strat <- strategyImport filePathState filePathQMatrix
  case strat of
    Left str -> print str
    Right strat' -> generateIsEq $ evaluate (stageMC actionSpace1 actionsSpace2 parameters) strat' (StochasticStatefulContext (pure ((),(initialState))) (\_ _ -> pure ()))

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
                                '[Kleisli Stochastic (Action,Action) Action,
                                  Kleisli Stochastic (Action,Action) Action]
                             -> (Action,Action)
                             -> [Action]
                             -> [Action]
                             -> Parameters
                             -> StateT Vector Stochastic ()
determineContinuationPayoffs 1        strat action actionSpace1 actionsSpace2 parameters = pure ()
determineContinuationPayoffs iterator strat action actionSpace1 actionsSpace2 parameters = do
   extractContinuation executeStrat action
   nextInput <- ST.lift $ extractNextState executeStrat action
   determineContinuationPayoffs (pred iterator) strat nextInput actionSpace1 actionsSpace2 parameters
 where executeStrat =  play (stageMC actionSpace1 actionsSpace2 parameters) strat


-- Repeated game
repeatedGameEq iterator strat initialAction parameters = evaluate (stageMC actionSpace1 actionSpace2 parameters) strat context
  where context  = StochasticStatefulContext (pure ((),initialAction)) (\_ action -> determineContinuationPayoffs iterator strat action actionSpace1 actionSpace2 parameters)
        actionSpace1 = actionSpace parameters
        actionSpace2 = actionSpace parameters

-- Eq output for the repeated game
eqOutput iterator strat initialAction parameters = generateIsEq $ repeatedGameEq iterator strat initialAction parameters

-- Expose to the outside 
evaluateLearnedStrategiesMarkov ImportParameters{..} = do
  strat <- strategyImport filePathStateIndex filePathQMatrix
  case strat of
    Left str -> print str
    Right strat' -> eqOutput iterations strat' initialObservation parametersGame

-- Eq output for the repeated game
eqOutput2 iterator strat initialAction parameters = generateEquilibrium $ repeatedGameEq iterator strat initialAction parameters

-- Expose to the outside 
evaluateLearnedStrategiesMarkov2 ImportParameters{..} = do
  strat <- strategyImport filePathStateIndex filePathQMatrix
  return $ fmap (\strat -> eqOutput2 iterations strat initialObservation parametersGame) strat
  --case strat of
  --  Left str -> return $ Left str
  --  Right strat' -> return $ Right $ eqOutput iterations strat' initialObservation parametersGame

-- TODO write the bundle for a list of importParameters and export it to the same csv



