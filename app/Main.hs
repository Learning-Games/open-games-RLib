{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# LANGUAGE EmptyCase, DuplicateRecordFields #-}

import qualified Data.ByteString.Lazy as BS
import qualified Engine.QLearning.Export as QLearning
import qualified Examples.QLearning.CalvanoReplicationExportParameters as Scenario
import qualified Examples.QLearning.CalvanoReplication as ScenarioOld



parameters = Scenario.Parameters
  { pKsi = 0.1
  , pBeta = (- 0.00001)
  , pBertrandPrice = 1.47
  , pMonopolyPrice = 1.92
  , pGamma = 0.95
  , pLearningRate = 0.15
  , pMu = 0.25
  , pA1 = 2
  , pA2 = 2
  , pA0 = 0
  , pC1 = 1
  , pM  = 14 -- NOTE: Due to the construction, we need to take the orginial value of Calvano and take -1
  , pGeneratorEnv1 = 3
  , pGeneratorEnv2 = 100
  , pGeneratorPrice1 = 90
  , pGeneratorPrice2 = 39
  , pGeneratorObs1 = 2
  , pGeneratorObs2 = 400
  }


exportConfig = QLearning.ExportConfig
  { iterations = 100
  , outputEveryN = 1
  , incrementalMode = True
  , mapStagesM_ = Scenario.mapStagesM_ parameters
  , initial = Scenario.initialStrat parameters >>= Scenario.sequenceL
  , ctable = Scenario.actionSpace parameters
  , mkObservation = \a b -> Scenario.Obs (a, b)
  }


main :: IO ()
main = do
  BS.writeFile "parameters.csv" $ Scenario.csvParameters parameters
  QLearning.runQLearningExporting exportConfig
  putStrLn "completed task"
