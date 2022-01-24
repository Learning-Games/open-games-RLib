{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# LANGUAGE DuplicateRecordFields, GADTs #-}


import           Data.HashMap
import           System.Random

import qualified Engine.QLearningBoltzmann.ExportAsymmetricLearnersLogReduced as ExportAsymmetricLearners
import qualified Examples.QLearning.CournotBoltzmann as Scenario

-- This experiment considers a simple Cournot setting with linear demand and two players
-- This is a replication of Waltman Kaymak 2008

-------------------------
-- Fix variables for Game
-------------------------
-- Player parameters for learning phase
-- Game with low costs
-- Denoted as e1
parametersGame :: StdGen -> StdGen -> StdGen -> StdGen -> Scenario.Parameters
parametersGame gEnv1 gEnv2 gObs1 gObs2 = Scenario.Parameters
  { pBeta =  0.99999
  , pInitialExploreRate1 = 1000*0.99999
  , pInitialExploreRate2 = 1000*0.99999
  , pLowerQuantity1 = 0
  , pUpperQuantity1 = 40
  , pLowerQuantity2 = 0
  , pUpperQuantity2 = 40
  , pGamma = 0.90
  , pLearningRate = 0.15
  , pA = 40
  , pB = 1
  , pC1 = 4
  , pC2 = 4
  , pM1 = 40 -- NOTE: Due to the construction, we need to take the orginial value of Calvano and take -1
  , pM2 = 40 -- NOTE: Due to the construction, we need to take the orginial value of Calvano and take -1
  , pGeneratorEnv1 = gEnv1
  , pGeneratorEnv2 = gEnv2
  , pGeneratorObs1 = gObs1
  , pGeneratorObs2 = gObs2
  }

-- Create rematching map
-- NOTE Keys have to follow the pattern ei
parametersMap = fromList [ ("e11",parametersGame)
                         , ("e12",parametersGame)]


expIds = keys parametersMap



--------------------
-- Symmetric matches

p1e1p2e1 :: Double -> Double -> StdGen -> StdGen -> StdGen -> StdGen -> Scenario.Parameters
p1e1p2e1 explore1 explore2 gEnv1 gEnv2 gObs1 gObs2 = Scenario.Parameters
  { pBeta =  0.99999
  , pInitialExploreRate1 = explore1
  , pInitialExploreRate2 = explore2
  , pLowerQuantity1 = 0
  , pUpperQuantity1 = 40
  , pLowerQuantity2 = 0
  , pUpperQuantity2 = 40
  , pGamma = 0.90
  , pLearningRate = 0.15
  , pA = 40
  , pB = 1
  , pC1 = 4
  , pC2 = 4
  , pM1 = 40 -- NOTE: Due to the construction, we need to take the orginial value of Calvano and take -1
  , pM2 = 40 -- NOTE: Due to the construction, we need to take the orginial value of Calvano and take -1
  , pGeneratorEnv1 = gEnv1
  , pGeneratorEnv2 = gEnv2
  , pGeneratorObs1 = gObs1
  , pGeneratorObs2 = gObs2

  }



----------------
-- Create the map which contains all the parameterizations
-- NOTE: the keys have to follow the pattern "exey"
parametersGameRematchingMap explore1 explore2 =
   fromList [ (("e11","e12"),p1e1p2e1 explore1 explore2)
            , (("e12","e11"),p1e1p2e1 explore1 explore2)]


rematchIds = [ Scenario.ReMatchType "e11" "e12" True
             , Scenario.ReMatchType "e12" "e11" True]

-----------------------------------
-- Fix variables for Run and Export
-----------------------------------



-- Number of runs to be executed
numberOfRuns :: Int
numberOfRuns = 1

-- How many of the last iterations should be exported
keepOnlyNLastIterations :: Int
keepOnlyNLastIterations = 100


-- Configuration of run and export parameters for initial learning run
exportConfigGameLearning name parameters = ExportAsymmetricLearners.ExportConfig
    { iterations = 1000000
    -- ^ how many iterations?
    , qValueExportMode = ExportAsymmetricLearners.LastOnly
    -- ^ report incremental changes to qmatrix or export full qmatrix with each iteration?
    , outputEveryN = 1
    -- ^ For complete reporting of Q-values, how often should values be exported?
      , threshold = 1000000
    -- ^ Stopping criterion: After how many runs should the computation be stopped?
    , mapStagesM_ = Scenario.mapStagesMFinalResult Scenario.configQL Scenario.configQL parameters
    , initial = Scenario.initialStrat parameters (Scenario.initialArray1 parameters) (Scenario.initialArray2 parameters) (Scenario.randomInitialObservation parameters) >>= Scenario.sequenceL
    , ctable1 = Scenario.actionSpace1 parameters
    , ctable2 = Scenario.actionSpace2 parameters
    , mkObservation = \a b -> Scenario.Obs (a, b)
    , runName = name
    , players = 2
    }

-- Configuration of run and export parameters for rematching phase
exportConfigGameRematchingPhase2 name parameters arr1 arr2 obs = ExportAsymmetricLearners.ExportConfig
    { iterations = 1000
    -- ^ how many iterations?
    , qValueExportMode = ExportAsymmetricLearners.LastOnly
    -- ^ report incremental changes to qmatrix or export full qmatrix with each iteration?
    , outputEveryN = 1
    -- ^ For complete reporting of Q-values, how often should values be exported?
      , threshold = 100000 -- NOTE this is a hack, as we avoid stopping the execution too early
    -- ^ Stopping criterion: After how many runs should the computation be stopped?
    , mapStagesM_ = Scenario.mapStagesMFinalResult Scenario.configQLNoLearning Scenario.configQLNoLearning parameters
    , initial = Scenario.initialStrat parameters arr1 arr2 obs >>= Scenario.sequenceL
    , ctable1 = Scenario.actionSpace1 parameters
    , ctable2 = Scenario.actionSpace2 parameters
    , mkObservation = \a b -> Scenario.Obs (a, b)
    , runName = name
    , players = 2
    }

-- Configuration of run and export parameters for rematching phase
exportConfigGameRematchingPhase3 name parameters arr1 arr2 obs = ExportAsymmetricLearners.ExportConfig
    { iterations = 1000000
    -- ^ how many iterations?
    , qValueExportMode = ExportAsymmetricLearners.LastOnly
    -- ^ report incremental changes to qmatrix or export full qmatrix with each iteration?
    , outputEveryN = 1
    -- ^ For complete reporting of Q-values, how often should values be exported?
      , threshold = 100000 -- NOTE this is a hack, as we avoid stopping the execution too early
    -- ^ Stopping criterion: After how many runs should the computation be stopped?
    , mapStagesM_ = Scenario.mapStagesMFinalResult Scenario.configQLNoLearning Scenario.configQLNoLearning parameters
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
                    (parametersGameRematchingMap 0 0)
                    (parametersGameRematchingMap 0 0)
                    exportConfigGameRematchingPhase2
                    exportConfigGameRematchingPhase3
                    expIds
                    rematchIds
                    rematchIds)
           [1..numberOfRuns]
