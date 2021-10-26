module Examples.QLearning.AsymmetricLearners8Phases
  ( initialStrat
  , initialArray1
  , initialArray2
  , randomInitialObservation
  , actionSpace1
  , actionSpace2
  , csvParameters
  , sequenceL
  , evalStageM
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
----------------------------------------------------
{-# INLINE executeAndRematchSingleRun #-}
executeAndRematchSingleRun runNo exportConfigGameLearning parametersMap keepOnlyNLastIterations parametersGameRematchingMap parametersGameRematchingMapPhase3 parametersGameRematchingMapPhase5 parametersGameRematchingMapPhase7 parametersGameRematchingMapPhase9 parametersGameRematchingMapPhase11 parametersGameRematchingMapPhase13 parametersGameRematchingMapPhase15  exportConfigGameRematchingPhase exportConfigGameRematchingRelearning  expIds' rematchTypeIdsPhase rematchTypeIdsPhase3  rematchTypeIdsPhase5 rematchTypeIdsPhase7 rematchTypeIdsPhase9 rematchTypeIdsPhase11 rematchTypeIdsPhase13 rematchTypeIdsPhase15 = do
  qTablesMapPhase1 <- mapM (firstStageLearningMap ("_phase1_run_" ++ name)  runNo keepOnlyNLastIterations exportConfigGameLearning parametersMap) expIds'
  let aggMap1Phase1 = unions $ fmap fst qTablesMapPhase1
      aggMap2Phase1 = unions $ fmap snd qTablesMapPhase1
  -- ^ First initial run for all eight symmetric scenarios
  mapM_ (rematchedLearning ("_phase2_run_" ++ name) runNo keepOnlyNLastIterations parametersGameRematchingMap exportConfigGameRematchingPhase (aggMap1Phase1,aggMap2Phase1)) rematchTypeIdsPhase
  print "Phase 2 complete"
  -- ^ Rematch player 1 from costlevel 4 with all other players including himself
  qTablesMapPhase3 <- mapM (rematchedLearningWithName ("_phase3_run_" ++ name) runNo keepOnlyNLastIterations parametersGameRematchingMapPhase3 exportConfigGameRematchingRelearning (aggMap1Phase1,aggMap2Phase1) "e4") rematchTypeIdsPhase3
  let aggMap1Phase3 = unions $ fmap fst qTablesMapPhase3
  print "Phase 3 complete"
  -- ^ Rerun setting with player 1 and player 2 cost level 1
  mapM_ (rematchedLearning ("_phase4_run_" ++ name) runNo keepOnlyNLastIterations parametersGameRematchingMap exportConfigGameRematchingPhase (aggMap1Phase3,aggMap2Phase1)) rematchTypeIdsPhase
  print "Phase 4 complete"
  -- ^ Rematch player 1 from costlevel 4 with all other players including himself
  qTablesMapPhase5 <- mapM (rematchedLearningWithName ("_phase5_run_" ++ name) runNo keepOnlyNLastIterations parametersGameRematchingMapPhase5 exportConfigGameRematchingRelearning (aggMap1Phase1,aggMap2Phase1) "e4") rematchTypeIdsPhase5
  let aggMap1Phase5 = unions $ fmap fst qTablesMapPhase5
  -- ^ Rerun setting with player 1 and player 2 cost level 2
  print "Phase 5 complete"
  mapM_ (rematchedLearning ("_phase6_run_" ++ name) runNo keepOnlyNLastIterations parametersGameRematchingMap exportConfigGameRematchingPhase (aggMap1Phase5,aggMap2Phase1)) rematchTypeIdsPhase
  print "Phase 6 complete"
   -- ^ Rematch player 1 from costlevel 4 with all other players including himself
  qTablesMapPhase7 <- mapM (rematchedLearningWithName ("_phase7_run_" ++ name) runNo keepOnlyNLastIterations parametersGameRematchingMapPhase7 exportConfigGameRematchingRelearning (aggMap1Phase1,aggMap2Phase1) "e4") rematchTypeIdsPhase7
  let aggMap1Phase7 = unions $ fmap fst qTablesMapPhase7
  -- ^ Rerun setting with player 1 and player 2 cost level 3
  print "Phase 7 complete"
  mapM_ (rematchedLearning ("_phase8_run_" ++ name) runNo keepOnlyNLastIterations parametersGameRematchingMap exportConfigGameRematchingPhase (aggMap1Phase7,aggMap2Phase1)) rematchTypeIdsPhase
  -- ^ Rematch player 1 from costlevel 4 with all other players including himself
  print "Phase 8 complete"
  qTablesMapPhase9 <- mapM (rematchedLearningWithName ("_phase9_run_" ++ name) runNo keepOnlyNLastIterations parametersGameRematchingMapPhase9 exportConfigGameRematchingRelearning (aggMap1Phase1,aggMap2Phase1) "e4") rematchTypeIdsPhase9
  let aggMap1Phase9 = unions $ fmap fst qTablesMapPhase9
  -- ^ Rerun setting with player 1 and player 2 cost level 5
  print "Phase 9 complete"
  mapM_ (rematchedLearning ("_phase10_run_" ++ name) runNo keepOnlyNLastIterations parametersGameRematchingMap exportConfigGameRematchingPhase (aggMap1Phase9,aggMap2Phase1)) rematchTypeIdsPhase
  -- ^ Rematch player 1 from costlevel 4 with all other players including himself
  print "Phase 10 complete"
  qTablesMapPhase11 <- mapM (rematchedLearningWithName ("_phase11_run_" ++ name) runNo keepOnlyNLastIterations parametersGameRematchingMapPhase11 exportConfigGameRematchingRelearning (aggMap1Phase1,aggMap2Phase1) "e4") rematchTypeIdsPhase11
  let aggMap1Phase11 = unions $ fmap fst qTablesMapPhase11
  -- ^ Rerun setting with player 1 and player 2 cost level 6
  print "Phase 11 complete"
  mapM_ (rematchedLearning ("_phase12_run_" ++ name) runNo keepOnlyNLastIterations parametersGameRematchingMap exportConfigGameRematchingPhase (aggMap1Phase11,aggMap2Phase1)) rematchTypeIdsPhase
  -- ^ Rematch player 1 from costlevel 4 with all other players including himself
  print "Phase 12 complete"
  qTablesMapPhase13 <- mapM (rematchedLearningWithName ("_phase13_run_" ++ name) runNo keepOnlyNLastIterations parametersGameRematchingMapPhase13 exportConfigGameRematchingRelearning (aggMap1Phase1,aggMap2Phase1) "e4") rematchTypeIdsPhase13
  let aggMap1Phase13 = unions $ fmap fst qTablesMapPhase13
  -- ^ Rerun setting with player 1 and player 2 cost level 7
  print "Phase 13 complete"
  mapM_ (rematchedLearning ("_phase14_run_" ++ name) runNo keepOnlyNLastIterations parametersGameRematchingMap exportConfigGameRematchingPhase (aggMap1Phase13,aggMap2Phase1)) rematchTypeIdsPhase
  -- ^ Rematch player 1 from costlevel 4 with all other players including himself
  print "Phase 14 complete"
  qTablesMapPhase15 <- mapM (rematchedLearningWithName ("_phase15_run_" ++ name) runNo keepOnlyNLastIterations parametersGameRematchingMapPhase15 exportConfigGameRematchingRelearning (aggMap1Phase1,aggMap2Phase1) "e4") rematchTypeIdsPhase15
  let aggMap1Phase15 = unions $ fmap fst qTablesMapPhase15
  -- ^ Rerun setting with player 1 and player 2 cost level 8
  print "Phase 15 complete"
  mapM_ (rematchedLearning ("_phase16_run_" ++ name) runNo keepOnlyNLastIterations parametersGameRematchingMap exportConfigGameRematchingPhase (aggMap1Phase15,aggMap2Phase1)) rematchTypeIdsPhase
  -- ^ Rematch player 1 from costlevel 4 with all other players including himself
  print "Phase 16 complete"
 where name = show runNo
