module Examples.QLearning.AsymmetricLearners2PhasesExogAction
  ( initialStrat
  , initialArray1
  , initialArray2
  , randomInitialObservation
  , actionSpace1
  , actionSpace2
  , csvParameters
  , configQL
  , configQLNoLearning
  , sequenceL
  , mapStagesM_
  , mapStagesMFinalResult
  , firstStageLearningMap
  , rematchedLearning
  , rematchedLearningWithName
  , executeAndRematchSingleRun
  , PriceSpace(..)
  , Observation(..)
  , Parameters(..)
  , ReMatchType(..)
  , ReMatchTypeExog(..)
  ) where

import           Examples.QLearning.AsymmetricLearners.Internal
import           Data.HashMap

-- Model for paper: three cost levels
{-# INLINE executeAndRematchSingleRun #-}
executeAndRematchSingleRun runNo exportConfigGameLearning parametersMap keepOnlyNLastIterations parametersGameRematchingMapPhase2  exportConfigGameRematchingPhase2 expIds rematchTypeIdsPhase2PlusManualObs = do
          qTablesMapPhase1 <- mapM (firstStageLearningMap ("_phase1_run_" ++ name) runNo  keepOnlyNLastIterations exportConfigGameLearning parametersMap) expIds
          let aggMap1Phase1 = unions $ fmap fst qTablesMapPhase1
              aggMap2Phase1 = unions $ fmap snd qTablesMapPhase1
          mapM_ (rematchedLearningWithExogObs ("_phase2_run_" ++ name) runNo keepOnlyNLastIterations parametersGameRematchingMapPhase2 exportConfigGameRematchingPhase2 (aggMap1Phase1,aggMap2Phase1)) rematchTypeIdsPhase2PlusManualObs
   where name = show runNo
