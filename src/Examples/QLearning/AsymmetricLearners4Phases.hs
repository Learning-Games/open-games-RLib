module Examples.QLearning.AsymmetricLearners4Phases
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
  ) where

import           Examples.QLearning.AsymmetricLearners.Internal
import           Data.HashMap

-- Model for paper: three cost levels
{-# INLINE executeAndRematchSingleRun #-}
executeAndRematchSingleRun runNo  exportConfigGameLearning parametersMap keepOnlyNLastIterations parametersGameRematchingMapPhase2 parametersGameRematchingMapPhase3 parametersGameRematchingMapPhase4 exportConfigGameRematchingPhase2 exportConfigGameRematchingPhase3 exportConfigGameRematchingPhase4 expIds rematchTypeIdsPhase2 rematchTypeIdsPhase3 rematchTypeIdsPhase4= do
          qTablesMapPhase1 <- mapM (firstStageLearningMap ("_phase1_run_" ++ name) runNo keepOnlyNLastIterations exportConfigGameLearning parametersMap) expIds
          let aggMap1Phase1 = unions $ fmap fst qTablesMapPhase1
              aggMap2Phase1 = unions $ fmap snd qTablesMapPhase1
          mapM_ (rematchedLearning ("_phase2_run_" ++ name) runNo keepOnlyNLastIterations parametersGameRematchingMapPhase2 exportConfigGameRematchingPhase2 (aggMap1Phase1,aggMap2Phase1)) rematchTypeIdsPhase2
          qTablesMapPhase3 <- mapM (rematchedLearning ("_phase3_run_" ++ name) runNo keepOnlyNLastIterations parametersGameRematchingMapPhase3 exportConfigGameRematchingPhase3 (aggMap1Phase1,aggMap2Phase1)) rematchTypeIdsPhase3
          let aggMap1Phase3 = unions $ fmap fst qTablesMapPhase3
              aggMap2Phase3 = unions $ fmap snd qTablesMapPhase3
          mapM_ (rematchedLearning ("_phase4_run_" ++ name) runNo keepOnlyNLastIterations parametersGameRematchingMapPhase4 exportConfigGameRematchingPhase4 (aggMap1Phase3,aggMap2Phase3)) rematchTypeIdsPhase4
  where name = show runNo
