module Examples.QLearning.Cournot
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
  , evalStageM
  , mapStagesM_
  , mapStagesMFinalResult
  , firstStageLearningMap
  , rematchedLearning
  , rematchedLearningWithName
  , executeAndRematchSingleRun
  , QuantitySpace(..)
  , Observation(..)
  , Parameters(..)
  , ReMatchType(..)
  ) where

import           Examples.QLearning.Cournot.Internal
import           Data.HashMap

-- Basic scenario with learning, rematching, and reconvergence
{-# INLINE executeAndRematchSingleRun #-}
executeAndRematchSingleRun runNo exportConfigGameLearning parametersMap keepOnlyNLastIterations parametersGameRematchingMapPhase2 parametersGameRematchingMapPhase3 exportConfigGameRematchingPhase2 exportConfigGameRematchingPhase3 expIds rematchTypeIdsPhase2 rematchTypeIdsPhase3= do
          qTablesMapPhase1 <- mapM (firstStageLearningMap ("_phase1_run_" ++ name) runNo  keepOnlyNLastIterations exportConfigGameLearning parametersMap) expIds
          let aggMap1Phase1 = unions $ fmap fst qTablesMapPhase1
              aggMap2Phase1 = unions $ fmap snd qTablesMapPhase1
          mapM_ (rematchedLearning ("_phase2_run_" ++ name) runNo keepOnlyNLastIterations parametersGameRematchingMapPhase2 exportConfigGameRematchingPhase2 (aggMap1Phase1,aggMap2Phase1)) rematchTypeIdsPhase2
          mapM_ (rematchedLearning ("_phase3_run_" ++ name) runNo keepOnlyNLastIterations parametersGameRematchingMapPhase3 exportConfigGameRematchingPhase3 (aggMap1Phase1,aggMap2Phase1)) rematchTypeIdsPhase3
  where name = show runNo
