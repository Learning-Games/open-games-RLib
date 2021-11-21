{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}


import           Data.Array.IO as A
import qualified Data.Vector as V
import qualified Data.Vector.Sized as SV
import           Data.List (maximumBy)
import           RIO hiding (preview)
import           System.Random
import           Test.Hspec


import Engine.QLearningVerboseOutput
import Engine.Engine
import qualified Examples.QLearning.AsymmetricLearners.InternalVerboseOutput as AL
import qualified Engine.QLearning.ExportAsymmetricLearnersLogReduced as ExportAsymmetricLearners
import qualified Engine.Memory as Memory

-- TODO REDO PROPER TESTING

-------------
-- 0. Types


---------------------
-- 1. Top-level file
-- Uses just two actions for simplicity
main :: IO ()
main = do
  gEnv1   <- newStdGen
  gEnv2   <- newStdGen
  gObs1   <- newStdGen
  gObs2   <- newStdGen
  let parametersTestInitial  gEnv1 gEnv2 gObs1 gObs2=
        AL.Parameters
          { pKsi = 0.1
                , pBeta =  0.000004
                , pInitialExploreRate1 = ((exp 1) ** 0)
                , pInitialExploreRate2 = ((exp 1) ** 0)
                , pBertrandPrice1 = 1.47
                , pBertrandPrice2 = 1.47
                , pMonopolyPrice1 = 1.92
                , pMonopolyPrice2 = 1.92
                , pGamma = 0.95
                , pLearningRate = 0.15
                , pMu = 0.25
                , pA1 = 2
                , pA2 = 2
                , pA0 = 0
                , pC1 = 1
                , pC2 = 1
                , pM1 = 14 -- NOTE: Due to the construction, we need to take the orginial value of Calvano and take -1
                , pM2 = 14 -- NOTE: Due to the construction, we need to take the orginial value of Calvano and take -1
                , pGeneratorEnv1 = gEnv1
                , pGeneratorEnv2 = gEnv2
                , pGeneratorObs1 = gObs1
                , pGeneratorObs2 = gObs2
                }
      exportConfigGameLearning name parameters = ExportAsymmetricLearners.ExportConfig
                { iterations = 100
                -- ^ how many iterations?
                , qValueExportMode = ExportAsymmetricLearners.LastOnly
                -- ^ report incremental changes to qmatrix or export full qmatrix with each iteration?
                , outputEveryN = 1
                -- ^ For complete reporting of Q-values, how often should values be exported?
                  , threshold = 1000000
                -- ^ Stopping criterion: After how many runs should the computation be stopped?
                , mapStagesM_ = AL.mapStagesMFinalResult AL.configQLNoLearning AL.configQLNoLearning parameters
                , initial = AL.initialStrat parameters (AL.initialArray1 parameters) (AL.initialArray2 parameters) (AL.randomInitialObservation parameters) >>= AL.sequenceL
                , ctable1 = AL.actionSpace1 parameters
                , ctable2 = AL.actionSpace2 parameters
                , mkObservation = \a b -> AL.Obs (a, b)
                , runName = name
                , players = 2
                }
  learningSingleStage "test" 1 10 exportConfigGameLearning parametersTestInitial gEnv1 gEnv2 gObs1 gObs2
-- Recreates an initial run
learningSingleStage name runNo keepOnlyNLastIterations exportConfigFunction parametersFunction gEnv1 gEnv2 gObs1 gObs2 = do
  let parameters = parametersFunction gEnv1 gEnv2 gObs1 gObs2
      exportConfig = exportConfigFunction name parameters
  list <- ExportAsymmetricLearners.runQLearningExportingDiagnostics exportConfig runNo
  let (x1 ::- x2 ::- Nil) = list
      (p1,env1) = x1
      (p2,env2) = x2
      q1        = _qTable env1
      q2        = _qTable env2
      lastObs      = AL.Obs (p1,p2)
  pure ()
