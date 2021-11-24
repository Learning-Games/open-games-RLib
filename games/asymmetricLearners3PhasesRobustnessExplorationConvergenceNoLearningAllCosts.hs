 {-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# LANGUAGE DuplicateRecordFields, GADTs #-}


import           Data.HashMap
import           System.Random

import qualified Engine.QLearning.ExportAsymmetricLearnersLogReduced as ExportAsymmetricLearners
import qualified Examples.QLearning.AsymmetricLearners3Phases as Scenario


-------------------------
-- Fix variables for Game
-------------------------
-- Player parameters for learning phase
-- Game with low costs
-- Denoted as e1
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

-- Denoted as e2
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
  , pA1 = 2.1
  , pA2 = 2.1
  , pA0 = 0
  , pC1 = 1.1
  , pC2 = 1.1
  , pM1 = 19 -- NOTE: Due to the construction, we need to take the orginial value of Calvano and take -1
  , pM2 = 19 -- NOTE: Due to the construction, we need to take the orginial value of Calvano and take -1
  , pGeneratorEnv1 = gEnv1
  , pGeneratorEnv2 = gEnv2
  , pGeneratorObs1 = gObs1
  , pGeneratorObs2 = gObs2
  }


-- Denoted as e3
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
  , pA1 = 2.2
  , pA2 = 2.2
  , pA0 = 0
  , pC1 = 1.2
  , pC2 = 1.2
  , pM1 = 19 -- NOTE: Due to the construction, we need to take the orginial value of Calvano and take -1
  , pM2 = 19 -- NOTE: Due to the construction, we need to take the orginial value of Calvano and take -1
  , pGeneratorEnv1 = gEnv1
  , pGeneratorEnv2 = gEnv2
  , pGeneratorObs1 = gObs1
  , pGeneratorObs2 = gObs2
  }

-- Denoted as e4
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

-- Denoted as e5
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
  , pA1 = 2.4
  , pA2 = 2.4
  , pA0 = 0
  , pC1 = 1.4
  , pC2 = 1.4
  , pM1 = 19 -- NOTE: Due to the construction, we need to take the orginial value of Calvano and take -1
  , pM2 = 19 -- NOTE: Due to the construction, we need to take the orginial value of Calvano and take -1
  , pGeneratorEnv1 = gEnv1
  , pGeneratorEnv2 = gEnv2
  , pGeneratorObs1 = gObs1
  , pGeneratorObs2 = gObs2
  }


-- Denoted as e6
parametersGame6 :: StdGen -> StdGen -> StdGen -> StdGen -> Scenario.Parameters
parametersGame6 gEnv1 gEnv2 gObs1 gObs2 = Scenario.Parameters
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
  , pC1 = 1.5
  , pC2 = 1.5
  , pM1 = 19 -- NOTE: Due to the construction, we need to take the orginial value of Calvano and take -1
  , pM2 = 19 -- NOTE: Due to the construction, we need to take the orginial value of Calvano and take -1
  , pGeneratorEnv1 = gEnv1
  , pGeneratorEnv2 = gEnv2
  , pGeneratorObs1 = gObs1
  , pGeneratorObs2 = gObs2
  }


-- Denoted as e7
parametersGame7 :: StdGen -> StdGen -> StdGen -> StdGen -> Scenario.Parameters
parametersGame7 gEnv1 gEnv2 gObs1 gObs2 = Scenario.Parameters
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


-- Denoted as e8
parametersGame8 :: StdGen -> StdGen -> StdGen -> StdGen -> Scenario.Parameters
parametersGame8 gEnv1 gEnv2 gObs1 gObs2 = Scenario.Parameters
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
  , pA1 = 2.7
  , pA2 = 2.7
  , pA0 = 0
  , pC1 = 1.7
  , pC2 = 1.7
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
                         , ("e21",parametersGame2)
                         , ("e22",parametersGame2)
                         , ("e31",parametersGame3)
                         , ("e32",parametersGame3)
                         , ("e41",parametersGame4)
                         , ("e42",parametersGame4)
                         , ("e51",parametersGame5)
                         , ("e52",parametersGame5)
                         , ("e61",parametersGame6)
                         , ("e62",parametersGame6)
                         , ("e71",parametersGame7)
                         , ("e72",parametersGame7)
                         , ("e81",parametersGame8)
                         , ("e82",parametersGame8)]


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

-- Denoted as e2
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
  , pA1 = 2.1
  , pA2 = 2.1
  , pA0 = 0
  , pC1 = 1.1
  , pC2 = 1.1
  , pM1 = 19 -- NOTE: Due to the construction, we need to take the orginial value of Calvano and take -1
  , pM2 = 19 -- NOTE: Due to the construction, we need to take the orginial value of Calvano and take -1
  , pGeneratorEnv1 = gEnv1
  , pGeneratorEnv2 = gEnv2
  , pGeneratorObs1 = gObs1
  , pGeneratorObs2 = gObs2
  }


-- Denoted as e3
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
  , pA1 = 2.2
  , pA2 = 2.2
  , pA0 = 0
  , pC1 = 1.2
  , pC2 = 1.2
  , pM1 = 19 -- NOTE: Due to the construction, we need to take the orginial value of Calvano and take -1
  , pM2 = 19 -- NOTE: Due to the construction, we need to take the orginial value of Calvano and take -1
  , pGeneratorEnv1 = gEnv1
  , pGeneratorEnv2 = gEnv2
  , pGeneratorObs1 = gObs1
  , pGeneratorObs2 = gObs2
  }

-- Denoted as e4
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

-- Denoted as e5
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
  , pA1 = 2.4
  , pA2 = 2.4
  , pA0 = 0
  , pC1 = 1.4
  , pC2 = 1.4
  , pM1 = 19 -- NOTE: Due to the construction, we need to take the orginial value of Calvano and take -1
  , pM2 = 19 -- NOTE: Due to the construction, we need to take the orginial value of Calvano and take -1
  , pGeneratorEnv1 = gEnv1
  , pGeneratorEnv2 = gEnv2
  , pGeneratorObs1 = gObs1
  , pGeneratorObs2 = gObs2
  }


-- Denoted as e6
p1e6p2e6 :: Double -> Double -> StdGen -> StdGen -> StdGen -> StdGen -> Scenario.Parameters
p1e6p2e6 explore1 explore2 gEnv1 gEnv2 gObs1 gObs2 = Scenario.Parameters
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
  , pC1 = 1.5
  , pC2 = 1.5
  , pM1 = 19 -- NOTE: Due to the construction, we need to take the orginial value of Calvano and take -1
  , pM2 = 19 -- NOTE: Due to the construction, we need to take the orginial value of Calvano and take -1
  , pGeneratorEnv1 = gEnv1
  , pGeneratorEnv2 = gEnv2
  , pGeneratorObs1 = gObs1
  , pGeneratorObs2 = gObs2
  }


-- Denoted as e7
p1e7p2e7 :: Double -> Double -> StdGen -> StdGen -> StdGen -> StdGen -> Scenario.Parameters
p1e7p2e7 explore1 explore2 gEnv1 gEnv2 gObs1 gObs2 = Scenario.Parameters
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


-- Denoted as e8
p1e8p2e8 :: Double -> Double -> StdGen -> StdGen -> StdGen -> StdGen -> Scenario.Parameters
p1e8p2e8 explore1 explore2 gEnv1 gEnv2 gObs1 gObs2 = Scenario.Parameters
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
  , pA1 = 2.7
  , pA2 = 2.7
  , pA0 = 0
  , pC1 = 1.7
  , pC2 = 1.7
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
   fromList [ (("e11","e12"),p1e1p2e1 explore1 explore2)
            , (("e12","e11"),p1e1p2e1 explore1 explore2)
            , (("e21","e22"),p1e2p2e2 explore1 explore2)
            , (("e22","e21"),p1e2p2e2 explore1 explore2)
            , (("e31","e32"),p1e3p2e3 explore1 explore2)
            , (("e32","e31"),p1e3p2e3 explore1 explore2)
            , (("e41","e42"),p1e4p2e4 explore1 explore2)
            , (("e42","e41"),p1e4p2e4 explore1 explore2)
            , (("e51","e52"),p1e5p2e5 explore1 explore2)
            , (("e52","e51"),p1e5p2e5 explore1 explore2)
            , (("e61","e62"),p1e6p2e6 explore1 explore2)
            , (("e62","e61"),p1e6p2e6 explore1 explore2)
            , (("e71","e72"),p1e7p2e7 explore1 explore2)
            , (("e72","e71"),p1e7p2e7 explore1 explore2)
            , (("e81","e82"),p1e8p2e8 explore1 explore2)
            , (("e82","e81"),p1e8p2e8 explore1 explore2)]

rematchIds = [ Scenario.ReMatchType "e11" "e12" True
             , Scenario.ReMatchType "e12" "e11" True
             , Scenario.ReMatchType "e21" "e22" True
             , Scenario.ReMatchType "e22" "e21" True
             , Scenario.ReMatchType "e31" "e32" True
             , Scenario.ReMatchType "e32" "e31" True
             , Scenario.ReMatchType "e41" "e42" True
             , Scenario.ReMatchType "e42" "e41" True
             , Scenario.ReMatchType "e51" "e52" True
             , Scenario.ReMatchType "e52" "e51" True
             , Scenario.ReMatchType "e61" "e62" True
             , Scenario.ReMatchType "e62" "e61" True
             , Scenario.ReMatchType "e71" "e72" True
             , Scenario.ReMatchType "e72" "e71" True
             , Scenario.ReMatchType "e81" "e82" True
             , Scenario.ReMatchType "e82" "e81" True]


-----------------------------------
-- Fix variables for Run and Export
-----------------------------------



-- Number of runs to be executed
numberOfRuns :: Int
numberOfRuns = 250

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
