{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTs, OverloadedStrings, MonadComprehensions #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}

-- |

module Engine.QLearning.Export
  ( runQLearningExportingDiagnostics
  , ExportConfig(..)
  ) where

import           Control.Monad
import           Control.Monad.IO.Class
import qualified Control.Conditional as CC
import           Data.Aeson
import           Data.Array.Base as A
import           Data.Array.IO as A
import           Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as S8
import qualified Data.ByteString.Lazy.Builder as SB
import           Data.Double.Conversion.ByteString
import           Data.Foldable
import qualified Data.Ix as Ix
import           Data.String
import           Data.Time
import qualified Data.Vector as V
import qualified Data.Vector.Sized as SV
import qualified Engine.Memory as Memory
import           Engine.QLearning (ToIdx, toIdx, Idx, QLearningMsg(..), Env, CTable(..),ActionChoice(..))
import qualified Engine.QLearning as QLearning
import           Engine.TLL
import           FastCsv
import           GHC.TypeNats
import           Path
import           Prelude hiding (putStrLn)
import qualified RIO
import           RIO (MonadUnliftIO, RIO, GLogFunc)
import           System.IO (hSetBuffering, BufferMode(..))

--------------------------------------------------------------------------------
-- Schema

data StateActionIndex' state action = StateActionIndex'
  { state :: state
  , action :: action
  , index :: Int
  }

instance BuildHeaders (StateActionIndex' a b) where
  buildHeaders _ = "state,action,action_index"
  {-# INLINE buildHeaders #-}

instance (BuildCsvField state, BuildCsvField action) =>
         BuildCsvRow (StateActionIndex' state action) where
  {-# INLINE buildCsvRow #-}
  buildCsvRow StateActionIndex' {state, action, index = i} =
    buildCsvField state <> "," <> buildCsvField action <> "," <> SB.intDec i

data QValueRow = QValueRow
  { iteration, player, state_action_index :: !Int
  , qvalue :: !Double
  }

instance BuildCsvRow QValueRow where
  buildCsvRow QValueRow{iteration,player,state_action_index,qvalue} =
    SB.intDec iteration <> "," <>
    SB.intDec player <> "," <>
    SB.intDec state_action_index <> "," <>
    SB.byteString (toShortest qvalue)
  {-# INLINE buildCsvRow #-}

instance BuildHeaders QValueRow where
  buildHeaders _ = "iteration,player,state_action_index,qvalue"
  {-# INLINE buildHeaders #-}

data Reward = Reward
  { iteration, player, state_action_index :: !Int
  , reward :: !Double
  }

instance BuildCsvRow Reward where
  buildCsvRow Reward{iteration,player,state_action_index,reward} =
    SB.intDec iteration <> "," <>
    SB.intDec player <> "," <>
    SB.intDec state_action_index <> "," <>
    SB.byteString (toShortest reward)
  {-# INLINE buildCsvRow #-}

instance BuildHeaders Reward where
  buildHeaders _ = "iteration,player,state_action_index,reward"
  {-# INLINE buildHeaders #-}

data RewardDiagnostics = RewardDiagnostics
  { iteration, player, state_action_index :: !Int
  , action_choice :: !ActionChoice
  , explore_rate :: !QLearning.ExploreRate
  , reward :: !Double
  }

instance BuildCsvRow RewardDiagnostics where
  buildCsvRow RewardDiagnostics{iteration,player,state_action_index,action_choice,explore_rate,reward} =
    SB.intDec iteration <> "," <>
    SB.intDec player <> "," <>
    SB.intDec state_action_index <> "," <>
    SB.stringUtf8 action_choice <> "," <>
    SB.byteString (toShortest explore_rate) <> "," <>
    SB.byteString (toShortest reward)
  {-# INLINE buildCsvRow #-}

instance BuildHeaders RewardDiagnostics where
  buildHeaders _ = "iteration,player,state_action_index,action_choice,explore_rate,reward"
  {-# INLINE buildHeaders #-}





data ExportConfig n o a m = ExportConfig
  { outputEveryN :: Int
    -- ^ How often to write iteration outputs. Default=1, 5 would mean
    -- "output every 5 iterations".
  , incrementalMode :: Bool
    -- ^ Whether to only output incremental changes to a QTable of a
    -- given player for each iteration, or otherwise the whole QTable
    -- is outputted.
  , iterations :: Int
    -- ^ How many iterations to run.
  , initial :: m (List '[ ( a , Env n o a), ( a , Env n o a)])
    -- ^ Initial strategy.
  , ctable :: CTable a
    -- ^ Acion space.
  , mkObservation :: forall x. x -> x -> o x
    -- ^ How to make an observation of two player actions.
  , mapStagesM_ ::
    forall s.
       (s -> List '[ (a, Env n o a), (a, Env n o a)] -> m s)
    -> List '[ ( a , Env n o a), ( a , Env n o a)]
    -> Int -- ^ Count.
    -> s -- ^ State.
    -> m ()
    -- ^ How to map over stages step by step.
  , keepOnlyNLinesReward :: Int
    -- ^ Determines which lines, i.e. number of observations, are saved for simpler export
  , runName :: String
    -- ^ Description of file name; main purpose is to run several estimations of the same kind
  }

-----------------------
-- File paths

rewardsFile                  = [relfile|rewards.csv|]
rewardsExtendedFile          = [relfile|rewardsExtended.csv|]
rewardsExtendedEndNLinesFile = [relfile|rewardsExtendedEndNLines.csv|]
qValuesFile                  = [relfile|qvalues.csv|]
--------------------------------------------------------------------------------
-- Top-level functions

{-# INLINE runQLearningExportingDiagnostics #-}
runQLearningExportingDiagnostics ::
     ( ToJSON a
     , Show (o (Idx a))
     , Ix (Memory.Vector n (o (Idx a)))
     , Functor (Memory.Vector n)
     , Functor o
     , Memory.Memory n
     , ToJSON (o a)
     , ToIdx a
     , BuildCsvField a
     , BuildCsvField (a, a)
     , n ~ 1
     )
  => ExportConfig n o a (RIO (GLogFunc (QLearningMsg n o a)))
  -> IO ()
runQLearningExportingDiagnostics exportConfig = do
  liftIO (hSetBuffering RIO.stdout NoBuffering)
  dirResultIteration <- parseRelDir $ (runName exportConfig)
  withCsvFile
    (toFilePath (dirResultIteration </> rewardsFile))
      (\writeRewardRow ->
          withCsvFile
            (toFilePath (dirResultIteration </> rewardsExtendedFile))
              (\writeRewardExtendedRow ->
                withCsvFile
                  (toFilePath (dirResultIteration </> rewardsExtendedEndNLinesFile))
                      (\writeRewardExtendedEndNLinesRow ->
                            withCsvFile
                            (toFilePath (dirResultIteration </> qValuesFile))
                              (\writeQValueRow ->
                                  RIO.runRIO
                                    (RIO.mkGLogFunc
                                      (\_backtrace msg ->
                                          case msg of
                                            RewardMsg QLearning.Reward {..} ->  do
                                              writeRewardRow
                                                Reward
                                                  { iteration = rewardIteration
                                                  , player = rewardPlayer
                                                  , state_action_index = rewardStateActionIndex
                                                  , reward = rewardReward
                                                  }
                                            RewardDiagnosticsMsg QLearning.RewardDiagnostics {..} -> do
                                              writeRewardExtendedRow
                                                RewardDiagnostics
                                                  { iteration = rewardDiagIteration
                                                  , player = rewardDiagPlayer
                                                  , state_action_index = rewardDiagStateActionIndex
                                                  , action_choice = rewardDiagActionChoice
                                                  , explore_rate  = rewardDiagExploreRate
                                                  , reward = rewardDiagReward
                                                  }
                                              when
                                                (rewardDiagIteration > (iterations exportConfig) - (keepOnlyNLinesReward exportConfig))
                                                (writeRewardExtendedEndNLinesRow
                                                  RewardDiagnostics
                                                    { iteration = rewardDiagIteration
                                                    , player = rewardDiagPlayer
                                                    , state_action_index = rewardDiagStateActionIndex
                                                    , action_choice = rewardDiagActionChoice
                                                    , explore_rate  = rewardDiagExploreRate
                                                    , reward = rewardDiagReward
                                                    })
                                            QTableDirtied QLearning.Dirtied {..} ->
                                              when
                                                (incrementalMode exportConfig)
                                                (writeQValueRow
                                                  QValueRow
                                                    { iteration = dirtiedIteration + 1
                                                    , player = dirtiedPlayer
                                                    , state_action_index = dirtiedStateActionIndex
                                                    , qvalue = dirtiedQValue
                                                    })))
                                    (do initial' <- initial exportConfig
                                        writeStateActionIndex exportConfig initial'
                                        writeQValues exportConfig initial' writeQValueRow)))))






--------------------------------------------------------------------------------
-- Write state_action_index

-- | Dump the complete set of possible indices to the QTable.
writeStateActionIndex ::
     ( BuildCsvField action
     , BuildCsvField (action, action)
     , MonadUnliftIO m1
     , Ix (Memory.Vector n (o (Idx action)))
     , Memory.Memory n
     , KnownNat n
     , ToIdx action
     )
  => ExportConfig n o action m2
  -> List '[ ( action , Env n o action), ( action , Env n o action)]
  -> m1 ()
writeStateActionIndex ExportConfig {..} initial' = do
  putStrLn "Writing state action index ..."
  withCsvFile
    ("state_action_index_" <> runName  <> ".csv")
    (\writeRow -> do
       let (_, env) ::- _ = initial'
       bounds' <- liftIO (A.getBounds (QLearning._qTable env))
       liftIO
         (V.sequence_
            [ writeRow
              StateActionIndex' {state = (player1, player2), action, index = i}
            | player1 <- population ctable
            , player2 <- population ctable
            , action <- population ctable
            , let i =
                    Ix.index
                      bounds'
                      ( Memory.fromSV
                          (SV.replicate
                             (mkObservation (toIdx player1) (toIdx player2)))
                      , toIdx action)
            ]))

--------------------------------------------------------------------------------
-- Write QValues

-- | State for the writeQValueRow function's loop.
data State n o a = State
  { prev :: Double -- ^ Previous percentage complete.
  , iteration :: Int -- ^ Iteration number.
  , skipping :: Int -- ^ Skip every N dumps.
  }

writeQValues ::
     (MonadUnliftIO m, Ix (Memory.Vector n (o (Idx a))))
  => ExportConfig n o a m
  -> List '[ ( a , Env n o a), ( a , Env n o a)]
  -> (QValueRow -> IO ())
  -> m ()
writeQValues ExportConfig {..} initial'@(p1_0 ::- p2_0 ::- Nil) writeRow = do
  putStrLn "Writing initial tables"
  writePlayerQTable 0 1 p1_0
  writePlayerQTable 0 2 p2_0
  putStrLn "Running iterations ..."
  mapStagesM_
    (\State {prev, iteration, skipping} (p1 ::- p2 ::- Nil) -> do
       let !percent = fromIntegral iteration / fromIntegral iterations * 100
           !save =
             if percent >= prev + 10
               then percent
               else prev
       when (save /= prev) (putStrLn (toFixed 2 percent <> "% complete ..."))
       let
        in when
             -- Always include the first and last iteration.
             (iteration == iterations ||
              (not incrementalMode && (skipping == 0 || iteration == 1)))
             (do when
                   False
                   (putStrLn
                      ("Dumping QTable on iteration " <>
                       fromString (show iteration)))
                 writePlayerQTable iteration 1 p1
                 writePlayerQTable iteration 2 p2)
       pure
         State
           { prev = save
           , iteration = iteration + 1
           , skipping = Prelude.mod (skipping + 1) outputEveryN
           })
    initial'
    iterations
    State {prev = 0, iteration = 1, skipping = 1}
  where
    writePlayerQTable iteration player (_, env) = do
      liftIO
        (mapWithIndex_
           (\_i state_action_index qvalue ->
              writeRow QValueRow {state_action_index, ..})
           (QLearning._qTable env))

-- | A slightly more efficient mapper -- speculated.
mapWithIndex_ :: (MArray a e m, Ix i) => (i -> Int -> e -> m ()) -> a i e -> m ()
mapWithIndex_ f marr = do
  n <- A.getNumElements marr
  bounds' <- A.getBounds marr
  for_
    (range bounds')
    (\i -> do
       let !idx = safeIndex bounds' n i
       !e <- unsafeRead marr idx
       f i idx e)
{-# INLINE mapWithIndex_ #-}

--------------------------------------------------------------------------------
-- Threaded IO

-- | This is a thread-safe stdout printer, with timestamp.
putStrLn :: MonadIO m => ByteString -> m ()
putStrLn s =
  liftIO
    (do now' <- getCurrentTime
        S8.putStrLn (S8.pack (show now') <> ": " <> s))
