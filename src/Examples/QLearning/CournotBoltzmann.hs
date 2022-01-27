module Examples.QLearning.CournotBoltzmann
  ( initialStrat
  , initialArray1
  , initialArray2
  , lsZeros
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
  , QuantitySpace(..)
  , Observation(..)
  , Parameters(..)
  , ReMatchType(..)
  ) where

import           Examples.QLearning.CournotBoltzmann.Internal
import           Data.HashMap

-- Basic scenario with learning, rematching, and reconvergence
{-# INLINE executeAndRematchSingleRun #-}
executeAndRematchSingleRun runNo exportConfigGameLearning parametersMap keepOnlyNLastIterations expIds = do
          mapM (firstStageLearningMap ("_phase1_run_" ++ name) runNo  keepOnlyNLastIterations exportConfigGameLearning parametersMap) expIds
  where name = show runNo
