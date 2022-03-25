module Examples.QLearning.NoMemory3Phases
  ( initialStrat
  , initialArray1
  , initialArray2
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
  , Parameters(..)
  , ReMatchType(..)
  ) where

import           Examples.QLearning.NoMemory.Internal
import           Data.HashMap

-- Model for paper: three cost levels
{-# INLINE executeAndRematchSingleRun #-}
executeAndRematchSingleRun runNo exportConfigGameLearning parametersMap keepOnlyNLastIterations parametersGameRematchingMapPhase2 parametersGameRematchingMapPhase3 exportConfigGameRematchingPhase2 exportConfigGameRematchingPhase3 expIds rematchTypeIdsPhase2 rematchTypeIdsPhase3= do
          qTablesMapPhase1 <- mapM (firstStageLearningMap ("_phase1_run_" ++ name) runNo  keepOnlyNLastIterations exportConfigGameLearning parametersMap) expIds
          let aggMap1Phase1 = unions $ fmap fst qTablesMapPhase1
              aggMap2Phase1 = unions $ fmap snd qTablesMapPhase1
          mapM_ (rematchedLearning ("_phase2_run_" ++ name) runNo keepOnlyNLastIterations parametersGameRematchingMapPhase2 exportConfigGameRematchingPhase2 (aggMap1Phase1,aggMap2Phase1)) rematchTypeIdsPhase2
          mapM_ (rematchedLearning ("_phase3_run_" ++ name) runNo keepOnlyNLastIterations parametersGameRematchingMapPhase3 exportConfigGameRematchingPhase3 (aggMap1Phase1,aggMap2Phase1)) rematchTypeIdsPhase3
  where name = show runNo