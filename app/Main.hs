{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# LANGUAGE EmptyCase #-}

import qualified Data.ByteString.Lazy as BS
import qualified Engine.QLearning.Export as QLearning
import qualified Examples.QLearning.CalvanoReplication as Scenario

iters :: Int
iters = 100

main :: IO ()
main = do
  BS.writeFile "parameters.csv" $ Scenario.csvParameters
  QLearning.exportingRewardsCsv
    (do strat <- Scenario.initialStrat >>= Scenario.sequenceL
        QLearning.exportQValuesCsv
          iters
          Scenario.mapStagesM_
          strat
          Scenario.actionSpace
          (\a b -> Scenario.Obs (a,b)))
  putStrLn "completed"
