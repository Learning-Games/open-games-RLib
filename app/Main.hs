{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# LANGUAGE EmptyCase, DuplicateRecordFields #-}

import qualified Data.ByteString.Lazy as BS
import qualified Engine.QLearning.Export as QLearning
import qualified Examples.QLearning.CalvanoReplication as Scenario



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
  gEnv1   <- newStdGen
  gEnv2   <- newStdGen
  gPrice1 <- newStdGen
  gPrice2 <- newStdGen
  gObs1   <- newStdGen
  gObs2   <- newStdGen
  let parameters = Scenario.Parameters
       0.1
       (- 0.000004)
       1.47
       1.92
       0.95
       0.15
       0.25
       2
       2
       0
       1
       14 -- NOTE: Due to the construction, we need to take the orginial value of Calvano and take -1
       gEnv1
       gEnv2
       gPrice1
       gPrice2
       gObs1
       gObs2
  BS.writeFile "parameters.csv" $ Scenario.csvParameters parameters
  QLearning.runQLearningExporting exportConfig
  putStrLn "completed task"
