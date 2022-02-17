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

import qualified Data.ByteString.Lazy as L
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

-- Grid distance
dist1,dist2 :: Parameters -> Double
dist1 par =  (upperBound1 par - lowerBound1 par) / pM1 par
dist2 par =  (upperBound2 par - lowerBound2 par) / pM2 par

-- Bounds on grid
lowerBound1,upperBound1,lowerBound2,upperBound2 :: Parameters -> Double
lowerBound1 Parameters{pBertrandPrice1,pKsi,pMonopolyPrice1} = pBertrandPrice1 - pKsi*(pMonopolyPrice1 - pBertrandPrice1)
upperBound1 Parameters{pBertrandPrice1,pKsi,pMonopolyPrice1} = pMonopolyPrice1 + pKsi*(pMonopolyPrice1 - pBertrandPrice1)
lowerBound2 Parameters{pBertrandPrice2,pKsi,pMonopolyPrice2} = pBertrandPrice2 - pKsi*(pMonopolyPrice2 - pBertrandPrice2)
upperBound2 Parameters{pBertrandPrice2,pKsi,pMonopolyPrice2} = pMonopolyPrice2 + pKsi*(pMonopolyPrice2 - pBertrandPrice2)

-- Grid for player 1 and player 2
actionSpace1 par = [lowerBound1 par,lowerBound1 par + dist1 par .. upperBound1 par]
actionSpace2 par = [lowerBound2 par,lowerBound2 par + dist2 par .. upperBound2 par]

-- All possible observations
lsObservations :: Parameters -> [(Action,Action)]
lsObservations par = [(x,y) | x <- actionSpace1 par, y <- actionSpace2 par]

-- Export type for data storage
data ExportEqAnalysis = ExportEqAnalysis
   { filePath :: FilePath
   , initialObs1 :: Action
   , initialObs2 :: Action
   , equilibrium :: Bool
   } deriving (Generic)

headerExport = "file_path,initial_observation_1,intial_observation_2,equilibrium"

instance ToField Bool where
  toField True = "T"
  toField False = "F"
instance ToRecord ExportEqAnalysis 

type ImportFilePath = FilePath
type ExportFilePath = FilePath

----------------------------------------
-- 0. Import strategies from the outside
strategyImport :: ImportFilePath
               -> ImportFilePath
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
evaluateLearnedStrategiesOneShot :: ImportFilePath
                                 -> ImportFilePath
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
repeatedGameEq iterator strat initialAction parameters = evaluate (stageMC actionSpaceP1 actionSpaceP2 parameters) strat context
  where context  = StochasticStatefulContext (pure ((),initialAction)) (\_ action -> determineContinuationPayoffs iterator strat action actionSpaceP1 actionSpaceP2 parameters)
        actionSpaceP1 = actionSpace1 parameters
        actionSpaceP2 = actionSpace2 parameters


-- Eq output for the repeated game
eqOutput iterator strat parameters initialAction = generateEquilibrium $ repeatedGameEq iterator strat initialAction parameters

-- For one given estimation, check the equilibria for all possible initial conditions.
evaluateLearnedStrategiesMarkovLs
    :: Parameters
    -> Integer
    -> ImportFilePath
    -> ImportFilePath
    -> IO (Either (ParseErrorBundle String Void) [(FilePath, Action,Action,Bool)])
evaluateLearnedStrategiesMarkovLs parameters iterations filePathStateIndex filePathQMatrix = do
  strat <- strategyImport filePathStateIndex filePathQMatrix
  let  lsInitialObservations = lsObservations parameters
  return $
    fmap (\strat ->
            fmap (\(initialObs1,initialObs2) ->
                        ( filePathQMatrix
                        , initialObs1
                        , initialObs2
                        , eqOutput iterations strat parameters (initialObs1,initialObs2)))
                 lsInitialObservations)
         strat

------------------------------
-- Expose to the outside world

-- Given the parameters for a given run, the number of iterations to run, the file path to the state index, the list of paths to the qvaluematrices, compute the equilibria for each possible starting condition for each run, and export it to a .csv in the provided filepath 
importAndAnalyzeEquilibria :: Parameters
                           -> Integer
                           -> ExportFilePath
                           -> ImportFilePath
                           -> [ImportFilePath]
                           -> IO ()
importAndAnalyzeEquilibria parameters iterations exportFilePath filePathStateIndex lsFilePathQMatrix = do
   ls <- mapM  (evaluateLearnedStrategiesMarkovLs parameters iterations filePathStateIndex) lsFilePathQMatrix
   L.writeFile exportFilePath headerExport
   mapM_ (appendToCsv exportFilePath) ls  
   where
    appendToCsv :: ExportFilePath -> (Either (ParseErrorBundle String Void) [(FilePath, Action,Action,Bool)]) -> IO ()
    appendToCsv exportFilePath result = do
      case result of
        Left str  -> print str
        Right ls -> L.appendFile exportFilePath  (encode $ fmap (\(fp,ac1,ac2,b) -> ExportEqAnalysis fp ac1 ac2 b) ls)

