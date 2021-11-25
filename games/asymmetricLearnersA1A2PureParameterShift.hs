    {-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# LANGUAGE DuplicateRecordFields, GADTs #-}


import           Data.HashMap
import           System.Random

import qualified Engine.QLearning.ExportAsymmetricLearnersLogReduced as ExportAsymmetricLearners
import qualified Examples.QLearning.AsymmetricLearners3Phases as Scenario


-- This experiment analyzes variation in parameters a1 a2, as in a_i = c_i + x \in [0.8,1.2]
-------------------------
-- Fix variables for Game
-------------------------
-- Player parameters for learning phase


parametersGame1 :: StdGen -> StdGen -> StdGen -> StdGen -> Scenario.Parameters
parametersGame1 gEnv1 gEnv2 gObs1 gObs2 = Scenario.Parameters
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
  , pA1 = 2.1
  , pA2 = 2.1
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

parametersGame2 :: StdGen -> StdGen -> StdGen -> StdGen -> Scenario.Parameters
parametersGame2 gEnv1 gEnv2 gObs1 gObs2 = Scenario.Parameters
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
  , pA1 = 2.2
  , pA2 = 2.2
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
-- This is the baseline run
parametersGame3 :: StdGen -> StdGen -> StdGen -> StdGen -> Scenario.Parameters
parametersGame3 gEnv1 gEnv2 gObs1 gObs2 = Scenario.Parameters
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



parametersGame4 :: StdGen -> StdGen -> StdGen -> StdGen -> Scenario.Parameters
parametersGame4 gEnv1 gEnv2 gObs1 gObs2 = Scenario.Parameters
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
  , pA1 = 2.4
  , pA2 = 2.4
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

parametersGame5 :: StdGen -> StdGen -> StdGen -> StdGen -> Scenario.Parameters
parametersGame5 gEnv1 gEnv2 gObs1 gObs2 = Scenario.Parameters
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
  , pA1 = 2.5
  , pA2 = 2.5
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

-- Create rematching map
-- NOTE Keys have to follow the pattern ei
parametersMap = fromList [ ("e11",parametersGame1)
                         , ("e12",parametersGame1)
                         , ("e13",parametersGame1)
                         , ("e14",parametersGame1)
                         , ("e15",parametersGame1)
                         , ("e21",parametersGame2)
                         , ("e22",parametersGame2)
                         , ("e23",parametersGame2)
                         , ("e24",parametersGame2)
                         , ("e25",parametersGame2)
                         , ("e31",parametersGame3)
                         , ("e32",parametersGame3)
                         , ("e33",parametersGame3)
                         , ("e34",parametersGame3)
                         , ("e35",parametersGame3)
                         , ("e41",parametersGame4)
                         , ("e42",parametersGame4)
                         , ("e43",parametersGame4)
                         , ("e44",parametersGame4)
                         , ("e45",parametersGame4)
                         , ("e51",parametersGame5)
                         , ("e52",parametersGame5)
                         , ("e53",parametersGame5)
                         , ("e54",parametersGame5)
                         , ("e55",parametersGame5)
                         ]
expIds = keys parametersMap



--------------------
-- Symmetric matches

p1e1p2e1 :: Double -> Double -> StdGen -> StdGen -> StdGen -> StdGen -> Scenario.Parameters
p1e1p2e1 explore1 explore2 gEnv1 gEnv2 gObs1 gObs2 = Scenario.Parameters
  { pKsi = 0.1
  , pBeta =  0.000004
  , pInitialExploreRate1 = explore1
  , pInitialExploreRate2 = explore2
  , pBertrandPrice1 = 1.47
  , pBertrandPrice2 = 1.47
  , pMonopolyPrice1 = 2.62
  , pMonopolyPrice2 = 2.62
  , pGamma = 0.95
  , pLearningRate = 0.15
  , pMu = 0.25
  , pA1 = 2.1
  , pA2 = 2.1
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

p1e2p2e2 :: Double -> Double -> StdGen -> StdGen -> StdGen -> StdGen -> Scenario.Parameters
p1e2p2e2 explore1 explore2 gEnv1 gEnv2 gObs1 gObs2 = Scenario.Parameters
  { pKsi = 0.1
  , pBeta =  0.000004
  , pInitialExploreRate1 = explore1
  , pInitialExploreRate2 = explore2
  , pBertrandPrice1 = 1.47
  , pBertrandPrice2 = 1.47
  , pMonopolyPrice1 = 2.62
  , pMonopolyPrice2 = 2.62
  , pGamma = 0.95
  , pLearningRate = 0.15
  , pMu = 0.25
  , pA1 = 2.2
  , pA2 = 2.2
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

p1e3p2e3 :: Double -> Double -> StdGen -> StdGen -> StdGen -> StdGen -> Scenario.Parameters
p1e3p2e3 explore1 explore2 gEnv1 gEnv2 gObs1 gObs2 = Scenario.Parameters
  { pKsi = 0.1
  , pBeta =  0.000004
  , pInitialExploreRate1 = explore1
  , pInitialExploreRate2 = explore2
  , pBertrandPrice1 = 1.47
  , pBertrandPrice2 = 1.47
  , pMonopolyPrice1 = 2.62
  , pMonopolyPrice2 = 2.62
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


p1e4p2e4 :: Double -> Double -> StdGen -> StdGen -> StdGen -> StdGen -> Scenario.Parameters
p1e4p2e4 explore1 explore2 gEnv1 gEnv2 gObs1 gObs2 = Scenario.Parameters
  { pKsi = 0.1
  , pBeta =  0.000004
  , pInitialExploreRate1 = explore1
  , pInitialExploreRate2 = explore2
  , pBertrandPrice1 = 1.47
  , pBertrandPrice2 = 1.47
  , pMonopolyPrice1 = 2.62
  , pMonopolyPrice2 = 2.62
  , pGamma = 0.95
  , pLearningRate = 0.15
  , pMu = 0.25
  , pA1 = 2.4
  , pA2 = 2.4
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


p1e5p2e5 :: Double -> Double -> StdGen -> StdGen -> StdGen -> StdGen -> Scenario.Parameters
p1e5p2e5 explore1 explore2 gEnv1 gEnv2 gObs1 gObs2 = Scenario.Parameters
  { pKsi = 0.1
  , pBeta =  0.000004
  , pInitialExploreRate1 = explore1
  , pInitialExploreRate2 = explore2
  , pBertrandPrice1 = 1.47
  , pBertrandPrice2 = 1.47
  , pMonopolyPrice1 = 2.62
  , pMonopolyPrice2 = 2.62
  , pGamma = 0.95
  , pLearningRate = 0.15
  , pMu = 0.25
  , pA1 = 2.5
  , pA2 = 2.5
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



----------------
-- Create the map which contains all the parameterizations
-- NOTE: the keys have to follow the pattern "exey"
parametersGameRematchingMap explore1 explore2 =
   fromList [ (("e11","e11"),p1e1p2e1 explore1 explore2)
            , (("e12","e12"),p1e2p2e2 explore1 explore2)
            , (("e13","e13"),p1e3p2e3 explore1 explore2)
            , (("e14","e14"),p1e4p2e4 explore1 explore2)
            , (("e15","e15"),p1e5p2e5 explore1 explore2)
            , (("e21","e21"),p1e2p2e2 explore1 explore2)
            , (("e22","e22"),p1e2p2e2 explore1 explore2)
            , (("e23","e23"),p1e3p2e3 explore1 explore2)
            , (("e24","e24"),p1e4p2e4 explore1 explore2)
            , (("e25","e25"),p1e5p2e5 explore1 explore2)
            , (("e31","e31"),p1e2p2e2 explore1 explore2)
            , (("e32","e32"),p1e2p2e2 explore1 explore2)
            , (("e33","e33"),p1e3p2e3 explore1 explore2)
            , (("e34","e34"),p1e4p2e4 explore1 explore2)
            , (("e35","e35"),p1e5p2e5 explore1 explore2)
            , (("e41","e41"),p1e2p2e2 explore1 explore2)
            , (("e42","e42"),p1e2p2e2 explore1 explore2)
            , (("e43","e43"),p1e3p2e3 explore1 explore2)
            , (("e44","e44"),p1e4p2e4 explore1 explore2)
            , (("e45","e45"),p1e5p2e5 explore1 explore2)
            , (("e51","e51"),p1e2p2e2 explore1 explore2)
            , (("e52","e52"),p1e2p2e2 explore1 explore2)
            , (("e53","e53"),p1e3p2e3 explore1 explore2)
            , (("e54","e54"),p1e4p2e4 explore1 explore2)
            , (("e55","e55"),p1e5p2e5 explore1 explore2)]

rematchIds = [ Scenario.ReMatchType "e3" "e1" True
             , Scenario.ReMatchType "e3" "e2" True
             , Scenario.ReMatchType "e3" "e3" True
             , Scenario.ReMatchType "e3" "e4" True
             , Scenario.ReMatchType "e3" "e5" True]


-----------------------------------
-- Fix variables for Run and Export
-----------------------------------



-- Number of runs to be executed
numberOfRuns :: Int
numberOfRuns = 50

-- How many of the last iterations should be exported
keepOnlyNLastIterations :: Int
keepOnlyNLastIterations = 100


-- Configuration of run and export parameters for initial learning run
exportConfigGameLearning name parameters = ExportAsymmetricLearners.ExportConfig
    { iterations = 1000000000
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
    { iterations = 1000000000
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
