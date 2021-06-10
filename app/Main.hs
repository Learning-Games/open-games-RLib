{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# LANGUAGE EmptyCase, DuplicateRecordFields #-}

import qualified Data.ByteString.Lazy as BS
import qualified Engine.QLearning.Export as QLearning
import qualified Examples.QLearning.CalvanoReplication as Scenario

main :: IO ()
main = do
  BS.writeFile "parameters.csv" $ Scenario.csvParameters
  QLearning.runQLearningExporting
    QLearning.ExportConfig
      { iterations = 100
      , outputEveryN = 1
      , incrementalMode = True
      , mapStagesM_ = Scenario.mapStagesM_
      , initial = Scenario.initialStrat >>= Scenario.sequenceL
      , ctable = Scenario.actionSpace
      , mkObservation = \a b -> Scenario.Obs (a, b)
      }
  putStrLn "completed"
