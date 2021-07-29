{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE BangPatterns, OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DataKinds #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# LANGUAGE EmptyCase, DuplicateRecordFields, TemplateHaskell, QuasiQuotes, GADTs #-}



module Main where

import           Control.DeepSeq
import           Control.Exception
import           Criterion
import           Criterion.Main
import           Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as S8
import           Data.Foldable
import           Data.Functor.Identity
import           Data.Maybe
import           Engine.QLearning
import           Engine.TLL
import qualified Examples.QLearning.BestReply as BestReply
import qualified Examples.QLearning.CalvanoReplication as CalvanoReplication
import qualified Examples.QLearning.PDDeterministicPlayer as PDDeterministicPlayer
import           System.Environment
import           Text.Read

import qualified Data.ByteString.Lazy as BS
import           Path
import qualified Path.IO as IO
import           System.Random
import           System.Process.Typed



import qualified Engine.Engine as E
import qualified Data.Vector.Sized as SV
import qualified Engine.Memory as Memory
import qualified Engine.QLearning.ExportAsymmetric as QLearning
import qualified Examples.QLearning.AsymmetricPrices as Scenario
import qualified Engine.QLearning as Q
import qualified Examples.QLearning.AsymmetricPrices.Internal as Internal

import qualified RIO as RIO

-- Parameters for Calvano 
parametersCalvanoReplication :: StdGen -> StdGen -> StdGen -> StdGen -> CalvanoReplication.Parameters
parametersCalvanoReplication gEnv1 gEnv2 gObs1 gObs2 = CalvanoReplication.Parameters
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
  , pGeneratorObs1 = gObs1
  , pGeneratorObs2 = gObs2
  }



main = do
  iters <- fmap (fromMaybe [5 :: Int, 10, 20] . (>>=readMaybe)) (lookupEnv "ITERS")
  gEnv1   <- newStdGen
  gEnv2   <- newStdGen
  gObs1   <- newStdGen
  gObs2   <- newStdGen
  defaultMain
    [ bgroup
        "BestReply"
        [ bench
          ("iters/" ++ show i)
          (nf
             (\i ->
                BestReply.evalStageLS
                  (BestReply.initiateStrat BestReply.actionSpace 6)
                  i)
             i)
        | i <- iters
        ]
    , bgroup
        "PDDeterministicPlayer"
        [ Criterion.Main.env
          (fmap
             SkipNF
             (RIO.runRIO
                mempty
                (do initial <- PDDeterministicPlayer.initiateStrat
                    xs <- PDDeterministicPlayer.sequenceL initial
                    pure xs)))
          (\(SkipNF st) ->
             bench
               ("iters/" ++ show i)
               (nfIO  (RIO.runRIO
                        mempty
                        (do RIO.liftRIO $ (PDDeterministicPlayer.evalStageM st i)))))
        | i <- iters
        ]
    , bgroup
        "CalvanoReplication"
        [ Criterion.Main.env
          (fmap
             SkipNF
             (RIO.runRIO
                mempty
                (do initial <- CalvanoReplication.initialStrat (parametersCalvanoReplication gEnv1 gEnv2 gObs1 gObs2)
                    xs <- CalvanoReplication.sequenceL initial
                    pure xs)))
          (\(SkipNF st) ->
             bench
               ("iters/" ++ show i)
               (nfIO (RIO.runRIO
                        mempty
                        (do RIO.liftRIO $ (CalvanoReplication.evalStageM (parametersCalvanoReplication gEnv1 gEnv2 gObs1 gObs2) st i))))) 
        | i <- iters
        ]
    , bgroup
        "AsymmetricLearners"
        [bench
          ("iters/" ++ show i)
          (nfIO
             (
                let
              -------------------------
              -- Fix variables for Game
              -------------------------
              -- Player parameters for learning phase
              -- Game with low costs
              parametersGameLowC :: StdGen -> StdGen -> StdGen -> StdGen -> Scenario.Parameters
              parametersGameLowC gEnv1 gEnv2 gObs1 gObs2 = Scenario.Parameters
                { pKsi = 0.1
                , pBeta =  0.000004
                , pInitialExploreRate1 = ((exp 1) ** 0)
                , pInitialExploreRate2 = ((exp 1) ** 0)
                , pBertrandPrice1 = 1.47
                , pBertrandPrice2 = 1.47
                , pMonopolyPrice1 = 1.92
                , pMonopolyPrice2 = 1.92
                , pGamma = 0.95
                , pLearningRate = 0.15
                , pMu = 0.25
                , pA1 = 2
                , pA2 = 2
                , pA0 = 0
                , pC1 = 1
                , pC2 = 1
                , pM1 = 14 -- NOTE: Due to the construction, we need to take the orginial value of Calvano and take -1
                , pM2 = 14 -- NOTE: Due to the construction, we need to take the orginial value of Calvano and take -1
                , pGeneratorEnv1 = gEnv1
                , pGeneratorEnv2 = gEnv2
                , pGeneratorObs1 = gObs1
                , pGeneratorObs2 = gObs2
                }

              -- Game with high costs
              parametersGameHighC :: StdGen -> StdGen -> StdGen -> StdGen -> Scenario.Parameters
              parametersGameHighC gEnv1 gEnv2 gObs1 gObs2 = Scenario.Parameters
                { pKsi = 0.1
                , pBeta =  0.000004
                , pInitialExploreRate1 = ((exp 1) ** 0)
                , pInitialExploreRate2 = ((exp 1) ** 0)
                , pBertrandPrice1 = 1.77
                , pBertrandPrice2 = 1.77
                , pMonopolyPrice1 = 2.22
                , pMonopolyPrice2 = 2.22
                , pGamma = 0.95
                , pLearningRate = 0.15
                , pMu = 0.25
                , pA1 = 2.3
                , pA2 = 2.3
                , pA0 = 0
                , pC1 = 1.3
                , pC2 = 1.3
                , pM1 = 14 -- NOTE: Due to the construction, we need to take the orginial value of Calvano and take -1
                , pM2 = 14 -- NOTE: Due to the construction, we need to take the orginial value of Calvano and take -1
                , pGeneratorEnv1 = gEnv1
                , pGeneratorEnv2 = gEnv2
                , pGeneratorObs1 = gObs1
                , pGeneratorObs2 = gObs2
                }

              -- Paramters for rematching phase

              -- Parameters after learning took place
              -- player1 of experiment 1 is matched with player 2 experiment 2
              parametersGameRematchingP1E1P2E2 :: StdGen -> StdGen -> StdGen -> StdGen -> Q.ExploreRate -> Q.ExploreRate -> Scenario.Parameters
              parametersGameRematchingP1E1P2E2 gEnv1 gEnv2 gObs1 gObs2 exploreRate1 exploreRate2 = Scenario.Parameters
                { pKsi = 0.1
                , pBeta =  0.000004
                , pInitialExploreRate1 = exploreRate1
                , pInitialExploreRate2 = exploreRate2
                , pBertrandPrice1 = 1.47
                , pBertrandPrice2 = 1.77
                , pMonopolyPrice1 = 1.92
                , pMonopolyPrice2 = 2.22
                , pGamma = 0.95
                , pLearningRate = 0.15
                , pMu = 0.25
                , pA1 = 2
                , pA2 = 2.3
                , pA0 = 0
                , pC1 = 1
                , pC2 = 1.3
                , pM1 = 14 -- NOTE: Due to the construction, we need to take the orginial value of Calvano and take -1
                , pM2 = 14 -- NOTE: Due to the construction, we need to take the orginial value of Calvano and take -1
                , pGeneratorEnv1 = gEnv1
                , pGeneratorEnv2 = gEnv2
                , pGeneratorObs1 = gObs1
                , pGeneratorObs2 = gObs2
                }

              -- player1 of experiment 2 is matched with player 2 experiment 1
              parametersGameRematchingP1E2P2E1 :: StdGen -> StdGen -> StdGen -> StdGen -> Q.ExploreRate -> Q.ExploreRate -> Scenario.Parameters
              parametersGameRematchingP1E2P2E1 gEnv1 gEnv2 gObs1 gObs2 exploreRate1 exploreRate2 = Scenario.Parameters
                { pKsi = 0.1
                , pBeta =  0.000004
                , pInitialExploreRate1 = exploreRate1
                , pInitialExploreRate2 = exploreRate2
                , pBertrandPrice1 = 1.77
                , pBertrandPrice2 = 1.47
                , pMonopolyPrice1 = 2.22
                , pMonopolyPrice2 = 1.92
                , pGamma = 0.95
                , pLearningRate = 0.15
                , pMu = 0.25
                , pA1 = 2.3
                , pA2 = 2
                , pA0 = 0
                , pC1 = 1.3
                , pC2 = 1
                , pM1 = 14 -- NOTE: Due to the construction, we need to take the orginial value of Calvano and take -1
                , pM2 = 14 -- NOTE: Due to the construction, we need to take the orginial value of Calvano and take -1
                , pGeneratorEnv1 = gEnv1
                , pGeneratorEnv2 = gEnv2
                , pGeneratorObs1 = gObs1
                , pGeneratorObs2 = gObs2
                }

              -- Control rematching
              -- player1 of experiment 1 is matched with player 2 experiment 1
              parametersGameRematchingP1E1P2E1 :: StdGen -> StdGen -> StdGen -> StdGen -> Q.ExploreRate -> Q.ExploreRate -> Scenario.Parameters
              parametersGameRematchingP1E1P2E1 gEnv1 gEnv2 gObs1 gObs2 exploreRate1 exploreRate2 = Scenario.Parameters
                { pKsi = 0.1
                , pBeta =  0.000004
                , pInitialExploreRate1 = exploreRate1
                , pInitialExploreRate2 = exploreRate2
                , pBertrandPrice1 = 1.47
                , pBertrandPrice2 = 1.47
                , pMonopolyPrice1 = 1.92
                , pMonopolyPrice2 = 1.92
                , pGamma = 0.95
                , pLearningRate = 0.15
                , pMu = 0.25
                , pA1 = 2
                , pA2 = 2
                , pA0 = 0
                , pC1 = 1
                , pC2 = 1
                , pM1 = 14 -- NOTE: Due to the construction, we need to take the orginial value of Calvano and take -1
                , pM2 = 14 -- NOTE: Due to the construction, we need to take the orginial value of Calvano and take -1
                , pGeneratorEnv1 = gEnv1
                , pGeneratorEnv2 = gEnv2
                , pGeneratorObs1 = gObs1
                , pGeneratorObs2 = gObs2
                }

              -- Control rematching
              -- player1 of experiment 2 is matched with player 2 experiment 2
              parametersGameRematchingP1E2P2E2 :: StdGen -> StdGen -> StdGen -> StdGen -> Q.ExploreRate -> Q.ExploreRate -> Scenario.Parameters
              parametersGameRematchingP1E2P2E2 gEnv1 gEnv2 gObs1 gObs2 exploreRate1 exploreRate2 = Scenario.Parameters
                { pKsi = 0.1
                , pBeta =  0.000004
                , pInitialExploreRate1 = exploreRate1
                , pInitialExploreRate2 = exploreRate2
                , pBertrandPrice1 = 1.77
                , pBertrandPrice2 = 1.77
                , pMonopolyPrice1 = 2.22
                , pMonopolyPrice2 = 2.22
                , pGamma = 0.95
                , pLearningRate = 0.15
                , pMu = 0.25
                , pA1 = 2.3
                , pA2 = 2.3
                , pA0 = 0
                , pC1 = 1.3
                , pC2 = 1.3
                , pM1 = 14 -- NOTE: Due to the construction, we need to take the orginial value of Calvano and take -1
                , pM2 = 14 -- NOTE: Due to the construction, we need to take the orginial value of Calvano and take -1
                , pGeneratorEnv1 = gEnv1
                , pGeneratorEnv2 = gEnv2
                , pGeneratorObs1 = gObs1
                , pGeneratorObs2 = gObs2
                }




              parametersGameRematching :: StdGen -> StdGen -> StdGen -> StdGen -> Q.ExploreRate -> Q.ExploreRate -> Scenario.Parameters
              parametersGameRematching gEnv1 gEnv2 gObs1 gObs2 exploreRate1 exploreRate2 = Scenario.Parameters
                { pKsi = 0.1
                , pBeta =  0.000004
                , pInitialExploreRate1 = exploreRate1
                , pInitialExploreRate2 = exploreRate2
                , pBertrandPrice1 = 1.47
                , pBertrandPrice2 = 1.77
                , pMonopolyPrice1 = 1.92
                , pMonopolyPrice2 = 2.00
                , pGamma = 0.95
                , pLearningRate = 0.15
                , pMu = 0.25
                , pA1 = 2
                , pA2 = 2.3
                , pA0 = 0
                , pC1 = 1
                , pC2 = 1.3
                , pM1 = 14 -- NOTE: Due to the construction, we need to take the orginial value of Calvano and take -1
                , pM2 = 14 -- NOTE: Due to the construction, we need to take the orginial value of Calvano and take -1
                , pGeneratorEnv1 = gEnv1
                , pGeneratorEnv2 = gEnv2
                , pGeneratorObs1 = gObs1
                , pGeneratorObs2 = gObs2
                }



              -----------------------------------
              -- Fix variables for Run and Export
              -----------------------------------
              -- Number of runs to be executed
              numberOfRuns :: Int
              numberOfRuns = i

              -- How many of the last iterations should be exported
              keepOnlyNLastIterations :: Int
              keepOnlyNLastIterations = 100


              -- Configuration of run and export parameters for initial learning run
              exportConfigGameLearning :: String
                                      -> Internal.Parameters
                                      -> QLearning.ExportConfig
                                            Internal.Player1N
                                            Internal.Observation
                                            Internal.PriceSpace
                                            (RIO.RIO
                                              (RIO.GLogFunc
                                                  (Q.QLearningMsg
                                                    Internal.Player1N Internal.Observation Internal.PriceSpace)))
              exportConfigGameLearning name parameters = QLearning.ExportConfig
                { iterations = 10
                -- ^ how many iterations?
                , incrementalMode = True
                -- ^ report incremental changes to qmatrix or export full qmatrix with each iteration?
                , outputEveryN = 1
                -- ^ For complete reporting of Q-values, how often should values be exported?
                  , threshold = 100000
                -- ^ Stopping criterion: After how many runs should the computation be stopped?
                , mapStagesM_ = Scenario.mapStagesM_ parameters
                , initial = Scenario.initialStrat parameters (Scenario.initialArray1 parameters) (Scenario.initialArray2 parameters)  >>= Scenario.sequenceL
                , ctable1 = Scenario.actionSpace1 parameters
                , ctable2 = Scenario.actionSpace2 parameters
                , mkObservation = \a b -> Scenario.Obs (a, b)
                , runName = name
                , players = 2
                }

              -- Configuration of run and export parameters for rematching phase
              exportConfigGameRematching :: String
                                        -> Internal.Parameters
                                        -> IO
                                              (Q.QTable
                                                Internal.Player1N Internal.Observation Internal.PriceSpace)
                                        -> IO
                                              (Q.QTable
                                                Internal.Player2N Internal.Observation Internal.PriceSpace)
                                        -> QLearning.ExportConfig
                                              Internal.Player1N
                                              Internal.Observation
                                              Internal.PriceSpace
                                              (RIO.RIO
                                                (RIO.GLogFunc
                                                    (Q.QLearningMsg
                                                      Internal.Player1N Internal.Observation Internal.PriceSpace)))
              exportConfigGameRematching name parameters arr1 arr2 = QLearning.ExportConfig
                      { iterations = 1000
                      -- ^ how many iterations?
                      , incrementalMode = True
                      -- ^ report incremental changes to qmatrix or export full qmatrix with each iteration?
                      , outputEveryN = 1
                      -- ^ For complete reporting of Q-values, how often should values be exported?
                        , threshold = 100000 -- NOTE this is a hack, as we avoid stopping the execution too early
                      -- ^ Stopping criterion: After how many runs should the computation be stopped?
                      , mapStagesM_ = Scenario.mapStagesM_ parameters
                      , initial = Scenario.initialStrat parameters arr1 arr2 >>= Scenario.sequenceL
                      , ctable1 = Scenario.actionSpace1 parameters
                      , ctable2 = Scenario.actionSpace2 parameters
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

              --------------------------------------------------------------------
              -- Experiment comprised of a number of runs of a given specification
              --------------------------------------------------------------------
              -- One single run for both experiments and then rematched
              {-# INLINE executeAndRematchSingleRun #-}
              executeAndRematchSingleRun name = do
                expLowC  <- firstStageLearning name parametersGameLowC exportConfigGameLearning
                -- ^ Instantiate run with low costs
                expHighC <- firstStageLearning name parametersGameHighC exportConfigGameLearning
                -- ^ Instantiate run with high costs
                rematchedLearning name expLowC expHighC
                -- ^ Rematching

              ------------------------
              -- Single learning stage
              ------------------------
              -- NOTE the difference is the input by the initial games parameters _parametersGameLowC_
              {-# INLINE firstStageLearning #-}
              firstStageLearning :: String
                                -> (StdGen -> StdGen -> StdGen -> StdGen -> Scenario.Parameters)
                                -- ^ The specific parameter function for that learning run
                                -> (String
                                      -> Internal.Parameters
                                      -> QLearning.ExportConfig
                                            Internal.Player1N
                                            Internal.Observation
                                            Internal.PriceSpace
                                            (RIO.RIO
                                              (RIO.GLogFunc
                                                  (Q.QLearningMsg
                                                    Internal.Player1N Internal.Observation Internal.PriceSpace))))
                                -- ^ The specific run configuration for that learning run
                                -> IO ((Q.QTable
                                        Internal.Player1N Internal.Observation Internal.PriceSpace,
                                      Q.ExploreRate),
                                      (Q.QTable
                                        Internal.Player2N Internal.Observation Internal.PriceSpace,
                                      Q.ExploreRate))
              firstStageLearning name parametersFunction exportConfigFunction = do
                gEnv1   <- newStdGen
                gEnv2   <- newStdGen
                gObs1   <- newStdGen
                gObs2   <- newStdGen
                let parameters = parametersFunction gEnv1 gEnv2 gObs1 gObs2
                    exportConfig = exportConfigFunction name parameters
                RIO.runRIO
                        mempty
                        (do strat <- QLearning.initial exportConfig
                            list <-
                              Scenario.evalStageM
                                parameters
                                strat
                                (QLearning.iterations exportConfig)
                            let (x1 E.::- x2 E.::- E.Nil) = last list 
                                (_,env1) = x1
                                (_,env2) = x2
                                q1       = Q._qTable env1
                                q2       = Q._qTable env2
                                exploreRate1 = Q._exploreRate env1
                                exploreRate2 = Q._exploreRate env2
                            RIO.liftIO $ pure ((q1,exploreRate1),(q2,exploreRate2)))

              ----------------------------------
              -- Single rematched Learning stage
              ----------------------------------
              {-# INLINE rematchedLearning #-}
              -- Takes the information from the learning phase runs (here two different experiments) and computes the exploration output
              rematchedLearning :: String
                                -> ((Q.QTable
                                            Internal.Player1N Internal.Observation Internal.PriceSpace,
                                          Q.ExploreRate),
                                        (Q.QTable
                                            Internal.Player2N Internal.Observation Internal.PriceSpace,
                                          Q.ExploreRate))
                                -> ((Q.QTable
                                            Internal.Player1N Internal.Observation Internal.PriceSpace,
                                          Q.ExploreRate),
                                        (Q.QTable
                                            Internal.Player2N Internal.Observation Internal.PriceSpace,
                                          Q.ExploreRate))
                                -> IO ()
              rematchedLearning name (x1,x2) (y1,y2) = do
                pairing2 ("p1e1-p2e2_run" ++ name) parametersGameRematchingP1E1P2E2 exportConfigGameRematching (x1,y2)
                pairing2 ("p1e2-p2e1_run" ++ name) parametersGameRematchingP1E1P2E2 exportConfigGameRematching (y1,x2)
                pairing2 ("p1e1-p2e1_run" ++ name) parametersGameRematchingP1E1P2E2 exportConfigGameRematching (x1,x2)
                pairing2 ("p1e2-p2e2_run" ++ name) parametersGameRematchingP1E1P2E2 exportConfigGameRematching (y1,y2)

              -- Reinitiates the exploitation phase for the starting conditions of two players
              {-# INLINE pairing2 #-}
              pairing2 :: String
                      -> (StdGen -> StdGen -> StdGen -> StdGen -> Q.ExploreRate -> Q.ExploreRate -> Scenario.Parameters)
                      -- ^ Parameters specific for the rematching players
                      -> (String
                          -> Internal.Parameters
                          -> IO
                                (Q.QTable
                                  Internal.Player1N Internal.Observation Internal.PriceSpace)
                          -> IO
                                (Q.QTable
                                  Internal.Player2N Internal.Observation Internal.PriceSpace)
                          -> QLearning.ExportConfig
                                Internal.Player1N
                                Internal.Observation
                                Internal.PriceSpace
                                (RIO.RIO
                                  (RIO.GLogFunc
                                      (Q.QLearningMsg
                                        Internal.Player1N Internal.Observation Internal.PriceSpace))))
                      -- ^ Parameters for the rematched runs
                      -> ((Q.QTable
                                Internal.Player1N Internal.Observation Internal.PriceSpace,
                              Q.ExploreRate),
                            (Q.QTable
                                Internal.Player2N Internal.Observation Internal.PriceSpace,
                              Q.ExploreRate))
                      -- ^ Relevant restarting conditions for players
                      -> IO ()
              pairing2 name parametersGameRematchingFunction exportConfigGameRematchingFunction ((qt1,exploreRate1),(qt2,exploreRate2)) = do
                gEnv1   <- newStdGen
                gEnv2   <- newStdGen
                gObs1   <- newStdGen
                gObs2   <- newStdGen
                let newParameters = parametersGameRematchingFunction gEnv1 gEnv2 gObs1 gObs2 exploreRate1 exploreRate2
                    newExportConfig = exportConfigGameRematchingFunction name newParameters (pure qt1) (pure qt2)
                dirResultIteration <- parseRelDir name
                IO.createDirIfMissing True dirResultIteration
                BS.writeFile (fromRelDir  (dirResultIteration </> parametersFile)) (Scenario.csvParameters newParameters)
                QLearning.runQLearningExportingDiagnostics newExportConfig
                runProcess_
                  (proc
                      "touch"
                      [fromRelDir (dirResultIteration </> rewardsExtendedEndNFile)]
                    )
                runProcess_
                  (shell
                      ( "tail "
                      ++ "-"
                      ++ (show $  keepOnlyNLastIterations * QLearning.players newExportConfig)
                      ++ " "
                      ++ fromRelDir (dirResultIteration </> rewardsExtendedFile)
                      ++ " > "
                      ++ fromRelDir (dirResultIteration </> rewardsExtendedEndNFile)
                      ))
                putStrLn ("completed task: " ++ name)
                in
                   sequence_ $ fmap executeAndRematchSingleRun $ fmap show [1..numberOfRuns]))
        | i <- iters
        ]
    ]

newtype SkipNF a = SkipNF a
instance NFData (SkipNF a) where
  rnf _ = ()

{--
main :: IO ()
main = do
  sequence_ $ fmap executeAndRematchSingleRun $ fmap show [1..numberOfRuns]
 -}

