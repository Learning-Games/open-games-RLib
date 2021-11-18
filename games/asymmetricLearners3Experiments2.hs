{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# LANGUAGE DuplicateRecordFields, GADTs #-}


import           Data.HashMap
import           System.Random

import qualified Engine.QLearning.ExportAsymmetricLearnersLog as ExportAsymmetricLearners
import qualified Examples.QLearning.AsymmetricLearnersTwoPhases as Scenario



-------------------------
-- Fix variables for Game
-------------------------
-- Player parameters for learning phase
-- Game with low costs
-- Denoted as e1
parametersGameLowC :: StdGen -> StdGen -> StdGen -> StdGen -> Scenario.Parameters
parametersGameLowC gEnv1 gEnv2 gObs1 gObs2 = Scenario.Parameters
  { pKsi = 0.1
  , pBeta =  0.000004
  , pInitialExploreRate1 = ((exp 1) ** 0)
  , pInitialExploreRate2 = ((exp 1) ** 0)
  , pBertrandPrice1 = 1.47
  , pBertrandPrice2 = 1.47
  , pMonopolyPrice1 = 2.52
  , pMonopolyPrice2 = 2.52
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

-- Game with medium costs
-- Denoted as e2
parametersGameMedC :: StdGen -> StdGen -> StdGen -> StdGen -> Scenario.Parameters
parametersGameMedC gEnv1 gEnv2 gObs1 gObs2 = Scenario.Parameters
  { pKsi = 0.1
  , pBeta =  0.000004
  , pInitialExploreRate1 = ((exp 1) ** 0)
  , pInitialExploreRate2 = ((exp 1) ** 0)
  , pBertrandPrice1 = 1.47
  , pBertrandPrice2 = 1.47
  , pMonopolyPrice1 = 2.52
  , pMonopolyPrice2 = 2.52
  , pGamma = 0.95
  , pLearningRate = 0.15
  , pMu = 0.25
  , pA1 = 2.3
  , pA2 = 2.3
  , pA0 = 0
  , pC1 = 1.3
  , pC2 = 1.3
  , pM1 = 19 -- NOTE: Due to the construction, we need to take the orginial value of Calvano and take -1
  , pM2 = 19 -- NOTE: Due to the construction, we need to take the orginial value of Calvano and take -1
  , pGeneratorEnv1 = gEnv1
  , pGeneratorEnv2 = gEnv2
  , pGeneratorObs1 = gObs1
  , pGeneratorObs2 = gObs2
  }


-- Game with high costs
-- Denoted as e3
parametersGameHighC :: StdGen -> StdGen -> StdGen -> StdGen -> Scenario.Parameters
parametersGameHighC gEnv1 gEnv2 gObs1 gObs2 = Scenario.Parameters
  { pKsi = 0.1
  , pBeta =  0.000004
  , pInitialExploreRate1 = ((exp 1) ** 0)
  , pInitialExploreRate2 = ((exp 1) ** 0)
  , pBertrandPrice1 = 1.47
  , pBertrandPrice2 = 1.47
  , pMonopolyPrice1 = 2.52
  , pMonopolyPrice2 = 2.52
  , pGamma = 0.95
  , pLearningRate = 0.15
  , pMu = 0.25
  , pA1 = 2.6
  , pA2 = 2.6
  , pA0 = 0
  , pC1 = 1.6
  , pC2 = 1.6
  , pM1 = 19 -- NOTE: Due to the construction, we need to take the orginial value of Calvano and take -1
  , pM2 = 19 -- NOTE: Due to the construction, we need to take the orginial value of Calvano and take -1
  , pGeneratorEnv1 = gEnv1
  , pGeneratorEnv2 = gEnv2
  , pGeneratorObs1 = gObs1
  , pGeneratorObs2 = gObs2
  }


-- Create rematching map
-- NOTE Keys have to follow the pattern ei
parametersMap = fromList [("e1",parametersGameLowC),("e2",parametersGameMedC),("e3",parametersGameHighC)]

expIds = keys parametersMap

-- Parameters for rematching phase
-- Parameters after learning took place
-- player1 of experiment 1 is matched with player 2 experiment 2
p1e1p2e2 :: StdGen -> StdGen -> StdGen -> StdGen -> Scenario.Parameters
p1e1p2e2 gEnv1 gEnv2 gObs1 gObs2 = Scenario.Parameters
  { pKsi = 0.1
  , pBeta =  0.000004
  , pInitialExploreRate1 = 0
  , pInitialExploreRate2 = 0
  , pBertrandPrice1 = 1.47
  , pBertrandPrice2 = 1.47
  , pMonopolyPrice1 = 2.52
  , pMonopolyPrice2 = 2.52
  , pGamma = 0.95
  , pLearningRate = 0.15
  , pMu = 0.25
  , pA1 = 2
  , pA2 = 2.3
  , pA0 = 0
  , pC1 = 1
  , pC2 = 1.3
  , pM1 = 19 -- NOTE: Due to the construction, we need to take the orginial value of Calvano and take -1
  , pM2 = 19 -- NOTE: Due to the construction, we need to take the orginial value of Calvano and take -1
  , pGeneratorEnv1 = gEnv1
  , pGeneratorEnv2 = gEnv2
  , pGeneratorObs1 = gObs1
  , pGeneratorObs2 = gObs2
  }

-- player1 of experiment 1 is matched with player 2 experiment 3
p1e1p2e3 :: StdGen -> StdGen -> StdGen -> StdGen -> Scenario.Parameters
p1e1p2e3 gEnv1 gEnv2 gObs1 gObs2 = Scenario.Parameters
  { pKsi = 0.1
  , pBeta =  0.000004
  , pInitialExploreRate1 = 0
  , pInitialExploreRate2 = 0
  , pBertrandPrice1 = 1.47
  , pBertrandPrice2 = 1.47
  , pMonopolyPrice1 = 2.52
  , pMonopolyPrice2 = 2.52
  , pGamma = 0.95
  , pLearningRate = 0.15
  , pMu = 0.25
  , pA1 = 2
  , pA2 = 2.6
  , pA0 = 0
  , pC1 = 1
  , pC2 = 1.6
  , pM1 = 19 -- NOTE: Due to the construction, we need to take the orginial value of Calvano and take -1
  , pM2 = 19 -- NOTE: Due to the construction, we need to take the orginial value of Calvano and take -1
  , pGeneratorEnv1 = gEnv1
  , pGeneratorEnv2 = gEnv2
  , pGeneratorObs1 = gObs1
  , pGeneratorObs2 = gObs2
  }

-- player1 of experiment 2 is matched with player 2 experiment 1
p1e2p2e1 :: StdGen -> StdGen -> StdGen -> StdGen -> Scenario.Parameters
p1e2p2e1 gEnv1 gEnv2 gObs1 gObs2 = Scenario.Parameters
  { pKsi = 0.1
  , pBeta =  0.000004
  , pInitialExploreRate1 = 0
  , pInitialExploreRate2 = 0
  , pBertrandPrice1 = 1.47
  , pBertrandPrice2 = 1.47
  , pMonopolyPrice1 = 2.52
  , pMonopolyPrice2 = 2.52
  , pGamma = 0.95
  , pLearningRate = 0.15
  , pMu = 0.25
  , pA1 = 2.3
  , pA2 = 2
  , pA0 = 0
  , pC1 = 1.3
  , pC2 = 1
  , pM1 = 19 -- NOTE: Due to the construction, we need to take the orginial value of Calvano and take -1
  , pM2 = 19 -- NOTE: Due to the construction, we need to take the orginial value of Calvano and take -1
  , pGeneratorEnv1 = gEnv1
  , pGeneratorEnv2 = gEnv2
  , pGeneratorObs1 = gObs1
  , pGeneratorObs2 = gObs2
  }

-- player1 of experiment 2 is matched with player 2 experiment 3
p1e2p2e3 :: StdGen -> StdGen -> StdGen -> StdGen -> Scenario.Parameters
p1e2p2e3 gEnv1 gEnv2 gObs1 gObs2 = Scenario.Parameters
  { pKsi = 0.1
  , pBeta =  0.000004
  , pInitialExploreRate1 = 0
  , pInitialExploreRate2 = 0
  , pBertrandPrice1 = 1.47
  , pBertrandPrice2 = 1.47
  , pMonopolyPrice1 = 2.52
  , pMonopolyPrice2 = 2.52
  , pGamma = 0.95
  , pLearningRate = 0.15
  , pMu = 0.25
  , pA1 = 2.3
  , pA2 = 2.6
  , pA0 = 0
  , pC1 = 1.3
  , pC2 = 1.6
  , pM1 = 19 -- NOTE: Due to the construction, we need to take the orginial value of Calvano and take -1
  , pM2 = 19 -- NOTE: Due to the construction, we need to take the orginial value of Calvano and take -1
  , pGeneratorEnv1 = gEnv1
  , pGeneratorEnv2 = gEnv2
  , pGeneratorObs1 = gObs1
  , pGeneratorObs2 = gObs2
  }

-- player1 of experiment 3 is matched with player 2 experiment 1
p1e3p2e1 :: StdGen -> StdGen -> StdGen -> StdGen -> Scenario.Parameters
p1e3p2e1 gEnv1 gEnv2 gObs1 gObs2 = Scenario.Parameters
  { pKsi = 0.1
  , pBeta =  0.000004
  , pInitialExploreRate1 = 0
  , pInitialExploreRate2 = 0
  , pBertrandPrice1 = 1.47
  , pBertrandPrice2 = 1.47
  , pMonopolyPrice1 = 2.52
  , pMonopolyPrice2 = 2.52
  , pGamma = 0.95
  , pLearningRate = 0.15
  , pMu = 0.25
  , pA1 = 2.6
  , pA2 = 2
  , pA0 = 0
  , pC1 = 1.6
  , pC2 = 1
  , pM1 = 19 -- NOTE: Due to the construction, we need to take the orginial value of Calvano and take -1
  , pM2 = 19 -- NOTE: Due to the construction, we need to take the orginial value of Calvano and take -1
  , pGeneratorEnv1 = gEnv1
  , pGeneratorEnv2 = gEnv2
  , pGeneratorObs1 = gObs1
  , pGeneratorObs2 = gObs2
  }


-- player1 of experiment 3 is matched with player 2 experiment 2
p1e3p2e2 :: StdGen -> StdGen -> StdGen -> StdGen -> Scenario.Parameters
p1e3p2e2 gEnv1 gEnv2 gObs1 gObs2 = Scenario.Parameters
  { pKsi = 0.1
  , pBeta =  0.000004
  , pInitialExploreRate1 = 0
  , pInitialExploreRate2 = 0
  , pBertrandPrice1 = 1.47
  , pBertrandPrice2 = 1.47
  , pMonopolyPrice1 = 2.52
  , pMonopolyPrice2 = 2.52
  , pGamma = 0.95
  , pLearningRate = 0.15
  , pMu = 0.25
  , pA1 = 2.6
  , pA2 = 2.3
  , pA0 = 0
  , pC1 = 1.6
  , pC2 = 1.3
  , pM1 = 19 -- NOTE: Due to the construction, we need to take the orginial value of Calvano and take -1
  , pM2 = 19 -- NOTE: Due to the construction, we need to take the orginial value of Calvano and take -1
  , pGeneratorEnv1 = gEnv1
  , pGeneratorEnv2 = gEnv2
  , pGeneratorObs1 = gObs1
  , pGeneratorObs2 = gObs2
  }



-- Control rematching
-- player1 of experiment 1 is matched with player 2 experiment 1
p1e1p2e1 :: StdGen -> StdGen -> StdGen -> StdGen -> Scenario.Parameters
p1e1p2e1 gEnv1 gEnv2 gObs1 gObs2 = Scenario.Parameters
  { pKsi = 0.1
  , pBeta =  0.000004
  , pInitialExploreRate1 = 0
  , pInitialExploreRate2 = 0
  , pBertrandPrice1 = 1.47
  , pBertrandPrice2 = 1.47
  , pMonopolyPrice1 = 2.52
  , pMonopolyPrice2 = 2.52
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

-- Control rematching
-- player1 of experiment 2 is matched with player 2 experiment 2
p1e2p2e2 :: StdGen -> StdGen -> StdGen -> StdGen -> Scenario.Parameters
p1e2p2e2 gEnv1 gEnv2 gObs1 gObs2  = Scenario.Parameters
  { pKsi = 0.1
  , pBeta =  0.000004
  , pInitialExploreRate1 = 0
  , pInitialExploreRate2 = 0
  , pBertrandPrice1 = 1.47
  , pBertrandPrice2 = 1.47
  , pMonopolyPrice1 = 2.52
  , pMonopolyPrice2 = 2.52
  , pGamma = 0.95
  , pLearningRate = 0.15
  , pMu = 0.25
  , pA1 = 2.3
  , pA2 = 2.3
  , pA0 = 0
  , pC1 = 1.3
  , pC2 = 1.3
  , pM1 = 19 -- NOTE: Due to the construction, we need to take the orginial value of Calvano and take -1
  , pM2 = 19 -- NOTE: Due to the construction, we need to take the orginial value of Calvano and take -1
  , pGeneratorEnv1 = gEnv1
  , pGeneratorEnv2 = gEnv2
  , pGeneratorObs1 = gObs1
  , pGeneratorObs2 = gObs2
  }


-- Control rematching
-- player1 of experiment 3 is matched with player 2 experiment 3
p1e3p2e3 :: StdGen -> StdGen -> StdGen -> StdGen -> Scenario.Parameters
p1e3p2e3 gEnv1 gEnv2 gObs1 gObs2  = Scenario.Parameters
  { pKsi = 0.1
  , pBeta =  0.000004
  , pInitialExploreRate1 = 0
  , pInitialExploreRate2 = 0
  , pBertrandPrice1 = 1.47
  , pBertrandPrice2 = 1.47
  , pMonopolyPrice1 = 2.52
  , pMonopolyPrice2 = 2.52
  , pGamma = 0.95
  , pLearningRate = 0.15
  , pMu = 0.25
  , pA1 = 2.6
  , pA2 = 2.6
  , pA0 = 0
  , pC1 = 1.6
  , pC2 = 1.6
  , pM1 = 19 -- NOTE: Due to the construction, we need to take the orginial value of Calvano and take -1
  , pM2 = 19 -- NOTE: Due to the construction, we need to take the orginial value of Calvano and take -1
  , pGeneratorEnv1 = gEnv1
  , pGeneratorEnv2 = gEnv2
  , pGeneratorObs1 = gObs1
  , pGeneratorObs2 = gObs2
  }

-- Create the map which contains all the parameterizations
-- NOTE: the keys have to follow the pattern "exey"
parametersGameRematchingMap = fromList [(("e1","e2"),p1e1p2e2),(("e1","e3"),p1e1p2e3),(("e2","e1"),p1e2p2e1),(("e2","e3"),p1e2p2e3),(("e3","e1"),p1e3p2e1),(("e3","e2"),p1e3p2e2),(("e1","e1"),p1e1p2e1),(("e2","e2"),p1e2p2e2),(("e3","e3"),p1e3p2e3)]

rematchIds = [Scenario.ReMatchType "e1" "e2" True, Scenario.ReMatchType "e1" "e3" True, Scenario.ReMatchType "e2" "e1" True, Scenario.ReMatchType "e2" "e3" True, Scenario.ReMatchType "e3" "e1" True, Scenario.ReMatchType "e3" "e2" True, Scenario.ReMatchType "e1" "e1" False, Scenario.ReMatchType "e2" "e2" False, Scenario.ReMatchType "e3" "e3" False]

-----------------------------------
-- Fix variables for Run and Export
-----------------------------------
-- Number of runs to be executed
numberOfRuns :: Int
numberOfRuns = 500

-- How many of the last iterations should be exported
keepOnlyNLastIterations :: Int
keepOnlyNLastIterations = 100


-- Configuration of run and export parameters for initial learning run
exportConfigGameLearning name parameters = ExportAsymmetricLearners.ExportConfig
    { iterations = 1000000000
    -- ^ how many iterations?
    , incrementalMode = True
    -- ^ report incremental changes to qmatrix or export full qmatrix with each iteration?
    , outputEveryN = 1
    -- ^ For complete reporting of Q-values, how often should values be exported?
      , threshold = 1000000
    -- ^ Stopping criterion: After how many runs should the computation be stopped?
    , mapStagesM_ = Scenario.mapStagesMFinalResult parameters
    , initial = Scenario.initialStrat parameters (Scenario.initialArray1 parameters) (Scenario.initialArray2 parameters) (Scenario.randomInitialObservation parameters) >>= Scenario.sequenceL
    , ctable1 = Scenario.actionSpace1 parameters
    , ctable2 = Scenario.actionSpace2 parameters
    , mkObservation = \a b -> Scenario.Obs (a, b)
    , runName = name
    , players = 2
    }

-- Configuration of run and export parameters for rematching phase
exportConfigGameRematching name parameters arr1 arr2 obs = ExportAsymmetricLearners.ExportConfig
    { iterations = 1000
    -- ^ how many iterations?
    , incrementalMode = True
    -- ^ report incremental changes to qmatrix or export full qmatrix with each iteration?
    , outputEveryN = 1
    -- ^ For complete reporting of Q-values, how often should values be exported?
      , threshold = 100000 -- NOTE this is a hack, as we avoid stopping the execution too early
    -- ^ Stopping criterion: After how many runs should the computation be stopped?
    , mapStagesM_ = Scenario.mapStagesMFinalResult parameters
    , initial = Scenario.initialStrat parameters arr1 arr2 obs >>= Scenario.sequenceL
    , ctable1 = Scenario.actionSpace1 parameters
    , ctable2 = Scenario.actionSpace2 parameters
    , mkObservation = \a b -> Scenario.Obs (a, b)
    , runName = name
    , players = 2
    }


----------------
-- Main run file
----------------
main :: IO ()
main = do
  sequence_ $
    fmap (\name -> Scenario.executeAndRematchSingleRun
                    name
                    exportConfigGameLearning
                    parametersMap
                    keepOnlyNLastIterations
                    parametersGameRematchingMap
                    exportConfigGameRematching
                    expIds
                    rematchIds)
           $ fmap show [1..numberOfRuns]
