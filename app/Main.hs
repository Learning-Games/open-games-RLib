{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# LANGUAGE EmptyCase #-}

import qualified Data.ByteString.Lazy as BS
import qualified Engine.QLearning.Export as QLearning
import qualified Examples.QLearning.CalvanoReplication as Scenario

main :: IO ()
main = do
  BS.writeFile "parameters.csv" $ Scenario.csvParameters
  results <-
    QLearning.exportingRewardsSqlite
      (do strat <- Scenario.initialStrat >>= Scenario.sequenceL
          results <- Scenario.evalStageM strat 100
          pure results)
  QLearning.exportQValuesSqlite results Scenario.actionSpace
  putStrLn "output completed"
