{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# LANGUAGE EmptyCase, DuplicateRecordFields, TemplateHaskell, QuasiQuotes #-}


import qualified Data.ByteString.Lazy as BS
import           Path
import qualified Path.IO as IO
import           System.Process.Typed
import           System.Random

import qualified Engine.QLearning.Export as QLearning
import qualified Examples.QLearning.CalvanoReplication as Scenario



------------------------
-- Fix variables for run
------------------------

-- Number of runs to be executed
numberOfRuns :: Int
numberOfRuns = 1000

-- How many of the last iterations should be exported
keepOnlyNLastIterations :: Int
keepOnlyNLastIterations = 100

-- Game parameters
parametersGame :: StdGen -> StdGen -> StdGen -> StdGen -> StdGen -> StdGen -> Scenario.Parameters
parametersGame gEnv1 gEnv2 gPrice1 gPrice2 gObs1 gObs2 = Scenario.Parameters
  { pKsi = 0.1
  , pBeta =  0.000004
  , pBertrandPrice = 1.47
  , pMonopolyPrice = 1.92
  , pGamma = 0.95
  , pLearningRate = 0.15
  , pMu = 0.25
  , pA1 = 2
  , pA2 = 2
  , pA0 = 0
  , pC1 = 1
  , pM = 14 -- NOTE: Due to the construction, we need to take the orginial value of Calvano and take -1
  , pGeneratorEnv1 = gEnv1
  , pGeneratorEnv2 = gEnv2
  , pGeneratorPrice1 =  gPrice1
  , pGeneratorPrice2 = gPrice2
  , pGeneratorObs1 = gObs1
  , pGeneratorObs2 = gObs2
  }

-- Configuration of run and export parameters
exportConfigGame name parameters = QLearning.ExportConfig
  { iterations =  1000000000
  -- ^ how many iterations?
  , incrementalMode = True
  -- ^ report incremental changes to qmatrix or export full qmatrix with each iteration?
  , outputEveryN = 1
  -- ^ For complete reporting of Q-values, how often should values be exported?
  , threshold = 100000
  -- ^ Stopping criterion: After how many runs should the computation be stopped?
  , mapStagesM_ = Scenario.mapStagesM_ parameters
  , initial = Scenario.initialStrat parameters >>= Scenario.sequenceL
  , ctable = Scenario.actionSpace parameters
  , mkObservation = \a b -> Scenario.Obs (a, b)
  , runName = name
  , players = 2
  }

-- File names for results
parametersFile          = [relfile|parameters.csv|]
rewardsExtendedFile     = [relfile|rewardsExtended.csv|]
rewardsExtendedEndNFile = [relfile|rewardsExtendedEndNLines.csv|]


----------------
-- Main run file
----------------

main :: IO ()
main = do
  sequence_ $ fmap specification $ fmap ("run" ++) $ fmap show [1..numberOfRuns]
  putStrLn "all tasks completed"

specification :: String -> IO ()
specification name = do
  gEnv1   <- mkStdGen 1-- newStdGen
  gEnv2   <- mkStdGen 2-- newStdGen
  gPrice1 <- mkStdGen 3-- newStdGen
  gPrice2 <- mkStdGen 4-- newStdGen
  gObs1   <- mkStdGen 5-- newStdGen
  gObs2   <- mkStdGen 6-- newStdGen
  let parameters = parametersGame gEnv1 gEnv2 gPrice1 gPrice2 gObs1 gObs2
      exportConfig = exportConfigGame name parameters
  dirResultIteration <- parseRelDir name
  IO.createDirIfMissing True dirResultIteration
  BS.writeFile (fromRelDir  (dirResultIteration </> parametersFile)) (Scenario.csvParameters parameters)
  QLearning.runQLearningExportingDiagnostics exportConfig
  runProcess_
    (proc
        "touch"
        [fromRelDir (dirResultIteration </> rewardsExtendedEndNFile)]
       )
  runProcess_
    (shell
        ( "tail "
         ++ "-"
         ++ (show $  keepOnlyNLastIterations * QLearning.players exportConfig)
         ++ " "
         ++ fromRelDir (dirResultIteration </> rewardsExtendedFile)
         ++ " > "
         ++ fromRelDir (dirResultIteration </> rewardsExtendedEndNFile)
        ))
  putStrLn ("completed task: " ++ name)

 
