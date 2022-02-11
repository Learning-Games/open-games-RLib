{-# LANGUAGE NamedFieldPuns #-}

module Import
  where


import Examples.QLearning.AsymmetricLearners.MarkovInternal
import Examples.QLearning.AsymmetricLearners3Phases (Parameters(..),actionSpace1,actionSpace2,randomInitialObservation)
import Engine.QLearning (CTable(..))

import qualified Data.Vector as V
import System.Random


------------------------------------------------
-- This is recreating the game used for learning
-- without _PriceSpace_. This is an interface to
-- the existing runs.
------------------------------------------------

{--
TODO
* import file paths for a range of results
* test for all starting conditions? 

MAYBE
* think about an approximate criterion? 

-}



parametersGame :: StdGen -> StdGen -> StdGen -> StdGen -> Parameters
parametersGame gEnv1 gEnv2 gObs1 gObs2 = Parameters
  { pKsi = 0.1
  , pBeta =  0.000004
  , pInitialExploreRate1 = ((exp 1) ** 0)
  , pInitialExploreRate2 = ((exp 1) ** 0)
  , pBertrandPrice1 = 1.47
  , pBertrandPrice2 = 1.47
  , pMonopolyPrice1 = 2.62
  , pMonopolyPrice2 = 2.62
  , pGamma = 0.95
  , pLearningRate = 0.15
  , pMu = 0.25
  , pA1 = 2
  , pA2 = 2
  , pA0 = 0
  , pC1 = 1
  , pC2 = 1
  , pM1 = 19 -- NOTE: Due to the construction, we need to take the orginial value of Calvano and take -1
  , pM2 = 19 -- NOTE: Due to the construction, we need to take the orginial value of Calvano and take -1
  , pGeneratorEnv1 = gEnv1
  , pGeneratorEnv2 = gEnv2
  , pGeneratorObs1 = gObs1
  , pGeneratorObs2 = gObs2
  }

sourcePath = "/Users/philippzahn/Documents/projects/learning/Software/results/p1e1p2e1_phase2_run_1/"

pathStateIndex = sourcePath ++ "state_action_index_2.csv"

pathQMatrix = sourcePath ++ "qvalues.csv"


initialObs = (10,10)

dist1 :: Parameters -> Double
dist1 par =  (upperBound1 par - lowerBound1 par) / pM1 par



lowerBound1,upperBound1 :: Parameters -> Double
lowerBound1 Parameters{pBertrandPrice1,pKsi,pMonopolyPrice1} = pBertrandPrice1 - pKsi*(pMonopolyPrice1 - pBertrandPrice1)
upperBound1 Parameters{pBertrandPrice1,pKsi,pMonopolyPrice1} = pMonopolyPrice1 + pKsi*(pMonopolyPrice1 - pBertrandPrice1)

actionSpace par = [lowerBound1 par,lowerBound1 par + dist1 par .. upperBound1 par]

importAndAnalyze = do
  gEnv1 <- newStdGen
  gEnv2 <- newStdGen
  gObs1 <- newStdGen
  gObs2 <- newStdGen
  let par = parametersGame gEnv1 gEnv2 gObs1 gObs2
  evaluateLearnedStrategiesMarkov 100 (1.355,1.355) pathStateIndex pathQMatrix (actionSpace par) (actionSpace par) par (pGamma par)
