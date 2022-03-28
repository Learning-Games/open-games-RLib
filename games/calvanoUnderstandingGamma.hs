{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# LANGUAGE DuplicateRecordFields, GADTs #-}


import           Data.HashMap
import           System.Random

import qualified Engine.QLearningNoMemory.Export as ExportAsymmetricLearners
import qualified Examples.QLearning.NoMemory3Phases as Scenario

-- This experiment considers asymmetric cost runs where players do not observe actual prices

-------------------------
-- Fix variables for Game
-------------------------
-- Player parameters for learning phase
-- Game with low costs
-- Denoted as e1
parametersGame1 :: Double -> StdGen -> StdGen -> StdGen -> StdGen -> Scenario.Parameters
parametersGame1 gamma gEnv1 gEnv2 gObs1 gObs2 = Scenario.Parameters
  { pKsi = 0
  , pBeta =  0.000004
  , pInitialExploreRate1 = ((exp 1) ** 0)
  , pInitialExploreRate2 = ((exp 1) ** 0)
  , pBertrandPrice1 = 1.47
  , pBertrandPrice2 = 1.47
  , pMonopolyPrice1 = 1.92
  , pMonopolyPrice2 = 1.92
  , pGamma = gamma
  , pLearningRate = 0.15
  , pMu = 0.25
  , pA1 = 2
  , pA2 = 2
  , pA0 = 0
  , pC1 = 1
  , pC2 = 1
  , pM1 = 1 -- NOTE: Due to the construction, we need to take the orginial value of Calvano and take -1
  , pM2 = 1 -- NOTE: Due to the construction, we need to take the orginial value of Calvano and take -1
  , pGeneratorEnv1 = gEnv1
  , pGeneratorEnv2 = gEnv2
  , pGeneratorObs1 = gObs1
  , pGeneratorObs2 = gObs2
  }

-- Denoted as e2
parametersGame2 :: Double -> StdGen -> StdGen -> StdGen -> StdGen -> Scenario.Parameters
parametersGame2 gamma gEnv1 gEnv2 gObs1 gObs2 = Scenario.Parameters
  { pKsi = 0
  , pBeta =  0.000004
  , pInitialExploreRate1 = ((exp 1) ** 0)
  , pInitialExploreRate2 = ((exp 1) ** 0)
  , pBertrandPrice1 = 1.47
  , pBertrandPrice2 = 1.47
  , pMonopolyPrice1 = 2.52
  , pMonopolyPrice2 = 2.52
  , pGamma = gamma
  , pLearningRate = 0.15
  , pMu = 0.25
  , pA1 = 2.3
  , pA2 = 2.3
  , pA0 = 0
  , pC1 = 1.3
  , pC2 = 1.3
  , pM1 = 1 -- NOTE: Due to the construction, we need to take the orginial value of Calvano and take -1
  , pM2 = 1 -- NOTE: Due to the construction, we need to take the orginial value of Calvano and take -1
  , pGeneratorEnv1 = gEnv1
  , pGeneratorEnv2 = gEnv2
  , pGeneratorObs1 = gObs1
  , pGeneratorObs2 = gObs2
  }


-- Denoted as e3
parametersGame3 :: Double -> StdGen -> StdGen -> StdGen -> StdGen -> Scenario.Parameters
parametersGame3 gamma gEnv1 gEnv2 gObs1 gObs2 = Scenario.Parameters
  { pKsi = 0
  , pBeta =  0.000004
  , pInitialExploreRate1 = ((exp 1) ** 0)
  , pInitialExploreRate2 = ((exp 1) ** 0)
  , pBertrandPrice1 = 1.47
  , pBertrandPrice2 = 1.47
  , pMonopolyPrice1 = 2.52
  , pMonopolyPrice2 = 2.52
  , pGamma = gamma
  , pLearningRate = 0.15
  , pMu = 0.25
  , pA1 = 2.6
  , pA2 = 2.6
  , pA0 = 0
  , pC1 = 1.6
  , pC2 = 1.6
  , pM1 = 1 -- NOTE: Due to the construction, we need to take the orginial value of Calvano and take -1
  , pM2 = 1 -- NOTE: Due to the construction, we need to take the orginial value of Calvano and take -1
  , pGeneratorEnv1 = gEnv1
  , pGeneratorEnv2 = gEnv2
  , pGeneratorObs1 = gObs1
  , pGeneratorObs2 = gObs2
  }

-- Create rematching map
-- NOTE Keys have to follow the pattern ei
parametersMap gamma = fromList [ ("e1",parametersGame1 gamma)
                               , ("e2",parametersGame2 gamma)
                               , ("e3",parametersGame3 gamma)]

expIds gamma = keys $ parametersMap gamma





--------------------
-- Symmetric matches

p1e1p2e1 :: Double -> StdGen -> StdGen -> StdGen -> StdGen -> Scenario.Parameters
p1e1p2e1 gamma gEnv1 gEnv2 gObs1 gObs2 = Scenario.Parameters
  { pKsi = 0
  , pBeta =  0.000004
  , pInitialExploreRate1 = 0
  , pInitialExploreRate2 = 0
  , pBertrandPrice1 = 1.47
  , pBertrandPrice2 = 1.47
  , pMonopolyPrice1 = 1.92
  , pMonopolyPrice2 = 1.92
  , pGamma = gamma
  , pLearningRate = 0.15
  , pMu = 0.25
  , pA1 = 2
  , pA2 = 2
  , pA0 = 0
  , pC1 = 1
  , pC2 = 1
  , pM1 = 1 -- NOTE: Due to the construction, we need to take the orginial value of Calvano and take -1
  , pM2 = 1 -- NOTE: Due to the construction, we need to take the orginial value of Calvano and take -1
  , pGeneratorEnv1 = gEnv1
  , pGeneratorEnv2 = gEnv2
  , pGeneratorObs1 = gObs1
  , pGeneratorObs2 = gObs2
  }

-- Denoted as e2
p1e2p2e2 :: Double -> StdGen -> StdGen -> StdGen -> StdGen -> Scenario.Parameters
p1e2p2e2 gamma gEnv1 gEnv2 gObs1 gObs2 = Scenario.Parameters
  { pKsi = 0
  , pBeta =  0.000004
  , pInitialExploreRate1 = 0
  , pInitialExploreRate2 = 0
  , pBertrandPrice1 = 1.47
  , pBertrandPrice2 = 1.47
  , pMonopolyPrice1 = 2.52
  , pMonopolyPrice2 = 2.52
  , pGamma = gamma
  , pLearningRate = 0.15
  , pMu = 0.25
  , pA1 = 2.3
  , pA2 = 2.3
  , pA0 = 0
  , pC1 = 1
  , pC2 = 1
  , pM1 = 1 -- NOTE: Due to the construction, we need to take the orginial value of Calvano and take -1
  , pM2 = 1 -- NOTE: Due to the construction, we need to take the orginial value of Calvano and take -1
  , pGeneratorEnv1 = gEnv1
  , pGeneratorEnv2 = gEnv2
  , pGeneratorObs1 = gObs1
  , pGeneratorObs2 = gObs2
  }


-- Denoted as e3
p1e3p2e3 :: Double -> StdGen -> StdGen -> StdGen -> StdGen -> Scenario.Parameters
p1e3p2e3 gamma gEnv1 gEnv2 gObs1 gObs2 = Scenario.Parameters
  { pKsi = 0
  , pBeta =  0.000004
  , pInitialExploreRate1 = 0
  , pInitialExploreRate2 = 0
  , pBertrandPrice1 = 1.47
  , pBertrandPrice2 = 1.47
  , pMonopolyPrice1 = 2.52
  , pMonopolyPrice2 = 2.52
  , pGamma = gamma
  , pLearningRate = 0.15
  , pMu = 0.25
  , pA1 = 2.6
  , pA2 = 2.6
  , pA0 = 0
  , pC1 = 1.6
  , pC2 = 1.6
  , pM1 = 1 -- NOTE: Due to the construction, we need to take the orginial value of Calvano and take -1
  , pM2 = 1 -- NOTE: Due to the construction, we need to take the orginial value of Calvano and take -1
  , pGeneratorEnv1 = gEnv1
  , pGeneratorEnv2 = gEnv2
  , pGeneratorObs1 = gObs1
  , pGeneratorObs2 = gObs2
  }


----------------
-- Create the map which contains all the parameterizations
-- NOTE: the keys have to follow the pattern "exey"
parametersGameRematchingMap gamma explore1 explore2 = fromList 
                                       [ (("e1","e1"),p1e1p2e1 gamma)
                                       , (("e2","e2"),p1e2p2e2 gamma)
                                       , (("e3","e3"),p1e3p2e3 gamma)]

rematchIds = [ Scenario.ReMatchType "e1" "e1" False
             , Scenario.ReMatchType "e2" "e2" False
             , Scenario.ReMatchType "e3" "e3" False]
             

-----------------------------------
-- Fix variables for Run and Export
-----------------------------------

-- Number of runs to be executed
numberOfRuns :: Int
numberOfRuns = 4

-- How many of the last iterations should be exported
keepOnlyNLastIterations :: Int
keepOnlyNLastIterations = 100


-- Configuration of run and export parameters for initial learning run
exportConfigGameLearning name parameters = ExportAsymmetricLearners.ExportConfig
    { iterations = 1000
    -- ^ how many iterations?
    , qValueExportMode = ExportAsymmetricLearners.LastOnly
    -- ^ report incremental changes to qmatrix or export full qmatrix with each iteration?
    , outputEveryN = 1
    -- ^ For complete reporting of Q-values, how often should values be exported?
      , threshold = 1000000
    -- ^ Stopping criterion: After how many runs should the computation be stopped?
    , mapStagesM_ = Scenario.mapStagesMFinalResult Scenario.configQL Scenario.configQL parameters
    , initial = Scenario.initialStrat parameters (Scenario.initialArray1 parameters) (Scenario.initialArray2 parameters)  >>= Scenario.sequenceL
    , ctable1 = Scenario.actionSpace1 parameters
    , ctable2 = Scenario.actionSpace2 parameters
    , runName = name
    , players = 2
    }

-- Configuration of run and export parameters for rematching phase
exportConfigGameRematchingPhase2 name parameters arr1 arr2 = ExportAsymmetricLearners.ExportConfig
    { iterations = 1000
    -- ^ how many iterations?
    , qValueExportMode = ExportAsymmetricLearners.LastOnly
    -- ^ report incremental changes to qmatrix or export full qmatrix with each iteration?
    , outputEveryN = 1
    -- ^ For complete reporting of Q-values, how often should values be exported?
      , threshold = 100000 -- NOTE this is a hack, as we avoid stopping the execution too early
    -- ^ Stopping criterion: After how many runs should the computation be stopped?
    , mapStagesM_ = Scenario.mapStagesMFinalResult Scenario.configQL Scenario.configQL parameters
    , initial = Scenario.initialStrat parameters arr1 arr2  >>= Scenario.sequenceL
    , ctable1 = Scenario.actionSpace1 parameters
    , ctable2 = Scenario.actionSpace2 parameters
    , runName = name
    , players = 2
    }

-- Configuration of run and export parameters for rematching phase
exportConfigGameRematchingPhase3 name parameters arr1 arr2  = ExportAsymmetricLearners.ExportConfig
    { iterations = 1000
    -- ^ how many iterations?
    , qValueExportMode = ExportAsymmetricLearners.LastOnly
    -- ^ report incremental changes to qmatrix or export full qmatrix with each iteration?
    , outputEveryN = 1
    -- ^ For complete reporting of Q-values, how often should values be exported?
      , threshold = 100000 -- NOTE this is a hack, as we avoid stopping the execution too early
    -- ^ Stopping criterion: After how many runs should the computation be stopped?
    , mapStagesM_ = Scenario.mapStagesMFinalResult Scenario.configQL Scenario.configQL parameters
    , initial = Scenario.initialStrat parameters arr1 arr2 >>= Scenario.sequenceL
    , ctable1 = Scenario.actionSpace1 parameters
    , ctable2 = Scenario.actionSpace2 parameters
    , runName = name
    , players = 2
    }

----------------
-- Main run file
----------------

lsGammas = [(0.1,0),(0.2,250),(0.3,500),(0.4,750),(0.5,1000),(0.6,1250),(0.7,1500),(0.8,1750),(0.9,2000),(1.0,2250)]

lsRuns = [(x,y,z)|(x,y) <- lsGammas, z <- [1..numberOfRuns]]

main ::  IO ()
main  = do
  sequence_ $
    fmap (\(gamma,shifter,name) -> Scenario.executeAndRematchSingleRun
                    (name + shifter)
                    exportConfigGameLearning
                    (parametersMap gamma)
                    keepOnlyNLastIterations
                    (parametersGameRematchingMap gamma 0 0)
                    (parametersGameRematchingMap gamma 0 0)
                    exportConfigGameRematchingPhase2
                    exportConfigGameRematchingPhase3
                    (expIds gamma)
                    rematchIds
                    rematchIds)
           lsRuns
