{-# LANGUAGE GeneralizedNewtypeDeriving #-}
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

module Engine.QLearningNoMemory.Export
  ( runQLearningExportingDiagnostics
  , RunNumber
  , QMatrixExportMode (..)
  , ExportConfig(..)
  , RewardDiagnostics(..)
  ) where

import           Control.Monad
import           Control.Monad.IO.Class
import           Data.Aeson
import           Data.Array.Base as A
import           Data.Array.IO as A
import           Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as S8
import qualified Data.ByteString.Lazy.Builder as SB
import           Data.Double.Conversion.ByteString
import           Data.Foldable
import           Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import           Data.Hashable
import           Data.IORef
import qualified Data.Ix as Ix
import           Data.Set (Set)
import qualified Data.Set as Set
import           Data.String
import           Data.Time
import qualified Data.Vector as V
import qualified Data.Vector.Sized as SV
import           Engine.QLearningNoMemory (ToIdx, toIdx, Idx, QLearningMsg(..), Env, CTable(..),ActionChoice)
import qualified Engine.QLearningNoMemory as QLearningNoMemory
import           Engine.TLL
import           FastCsvNoMemory
import           GHC.TypeNats
import           Path
import           Path.IO
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
  , explore_rate :: !QLearningNoMemory.ExploreRate
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


-- Which run number
type RunNumber = Int

-- Different export modes for the q matrix
data QMatrixExportMode = Incremental | Full | LastOnly
  deriving (Show,Eq)


data ExportConfig a m = ExportConfig
  { outputEveryN :: Int
    -- ^ How often to write iteration outputs. Default=1, 5 would mean
    -- "output every 5 iterations".
  , qValueExportMode :: QMatrixExportMode
    -- ^ Whether to only output incremental changes to a QTable of a
    -- given player for each iteration, or otherwise the whole QTable
    -- is outputted.
  , iterations :: Int
    -- ^ How many iterations to run.
  , threshold :: Int
    -- ^ How many iterations before we consider a player's consecutive
    -- runs of any same (state,maximal-action) pair before flagging
    -- that player as converged. After all players are converged, the
    -- iterations will terminate.
  , initial :: m (List '[ ( a , Env a), ( a , Env a)])
    -- ^ Initial strategy.
  , ctable1 :: CTable a
    -- ^ Action space player 1.
  , ctable2 :: CTable a
    -- ^ Action space player 2.
  , mapStagesM_ ::
    forall s.
       (s -> List '[ (a, Env a), (a, Env a)] -> m (QLearningNoMemory.Decision s))
    -> List '[ ( a , Env a), ( a , Env a)]
    -> Int -- ^ Count.
    -> s -- ^ State.
    -> m (List '[ (a, Env a), (a, Env a)])
    -- ^ How to map over stages step by step.
  , runName :: String
    -- ^ Description of file name; main purpose is to run several estimations of the same kind
  , players :: Int
  }

-----------------------
-- File paths

-- FIXME Different path names needed

rewardsExtendedFile :: Path b t
rewardsExtendedFile          = [relfile|rewardsExtended.csv|]
qValuesFile :: Path b t
qValuesFile                  = [relfile|qvalues.csv|]
stateActionIndexFile1 :: Path b t
stateActionIndexFile1         = [relfile|state_action_index_1.csv|]
stateActionIndexFile2 :: Path b t
stateActionIndexFile2         = [relfile|state_action_index_2.csv|]


--------------------------------------------------------------------------------
-- Top-level functions


{-# INLINE runQLearningExportingDiagnostics #-}
runQLearningExportingDiagnostics ::
     ( ToJSON a
     , ToIdx a
     , BuildCsvField a
     , Eq a
     )
  => ExportConfig a (RIO (GLogFunc (QLearningMsg a)))
  -> RunNumber
  -> IO (List '[ ( a , Env a), ( a , Env a)])
runQLearningExportingDiagnostics exportConfig runNo = do
  liftIO (hSetBuffering RIO.stdout NoBuffering)
  dirResultIteration <- parseRelDir $ (runName exportConfig)
  liftIO (ensureDir dirResultIteration)
  withCsvFileQMatrix
    (toFilePath (dirResultIteration </> rewardsExtendedFile))
        (\writeRewardExtendedRow ->
          withCsvFileQMatrix
              (toFilePath (dirResultIteration </> qValuesFile))
              (\writeQValueRow -> do
                maximalState <- newMaximalState
                RIO.runRIO
                  (RIO.mkGLogFunc
                      (\_backtrace msg ->
                        case msg of
                          GotMaximalActionForState maximal ->
                            updateMaximalTables
                              (threshold exportConfig)
                              maximalState
                              maximal
                              (players exportConfig)
                          RewardDiagnosticsMsg QLearningNoMemory.RewardDiagnostics {..} -> do
                            writeRewardExtendedRow
                              RewardDiagnostics
                                { iteration = rewardDiagIteration
                                , player = rewardDiagPlayer
                                , state_action_index =
                                    rewardDiagStateActionIndex
                                , action_choice = rewardDiagActionChoice
                                , explore_rate = rewardDiagExploreRate
                                , reward = rewardDiagReward
                                }
                          QTableDirtied QLearningNoMemory.Dirtied {..} ->
                            when
                              (qValueExportMode exportConfig == Incremental)
                              (writeQValueRow
                                  QValueRow
                                    { iteration = dirtiedIteration + 1
                                    , player = dirtiedPlayer
                                    , state_action_index =
                                        dirtiedStateActionIndex
                                    , qvalue = dirtiedQValue
                                    })))
                  (do initial' <- initial exportConfig
                      writeStateActionIndex1 exportConfig runNo initial'
                      writeStateActionIndex2 exportConfig runNo initial'
                      writeQValues
                        exportConfig
                        maximalState
                        initial'
                        writeQValueRow
                  )))



--------------------------------------------------------------------------------
-- Write state_action_index

-- | State for the writeQValueRow function's loop.
data State a = State
  { prev :: Double -- ^ Previous percentage complete.
  , iteration :: Int -- ^ Iteration number.
  , skipping :: Int -- ^ Skip every N dumps.
  }



-- | Dump the complete set of possible indices for player 1 to the QTable.
writeStateActionIndex1 ::
     ( BuildCsvField action
     , MonadUnliftIO m1
     , RIO.MonadThrow m1
     , ToIdx action
     )
  => ExportConfig action m2
  -> RunNumber
  -> List '[ ( action , Env action), ( action , Env action)]
  -> m1 ()
writeStateActionIndex1 ExportConfig {..} runNo initial' = do
  when
    (runNo == 1)
    (do dirResultIteration <- parseRelDir runName
        withCsvFile
          (toFilePath (dirResultIteration </> stateActionIndexFile1))
          (\writeRow -> do
            let (_, env) ::- _ = initial'
            bounds' <- liftIO (A.getBounds (QLearningNoMemory._qTable env))
            liftIO
              (V.sequence_
                  [ writeRow
                    StateActionIndex' {state = player1, action, index = i}
                  | player1 <- population ctable1
                  , action <- population ctable1
                  , let i =
                          Ix.index
                            bounds'
                            (toIdx action)
                  ])))

-- | Dump the complete set of possible indices for player 2 to the QTable.
writeStateActionIndex2 ::
     ( BuildCsvField action
     , MonadUnliftIO m1
     , RIO.MonadThrow m1
     , ToIdx action
     )
  => ExportConfig action m2
  -> RunNumber
  -> List '[ ( action , Env  action), ( action , Env action)]
  -> m1 ()
writeStateActionIndex2 ExportConfig {..} runNo  initial' = do
  when
    (runNo == 1)
    (do dirResultIteration <- parseRelDir runName
        withCsvFile
          (toFilePath (dirResultIteration </> stateActionIndexFile2))
          (\writeRow -> do
            let (_, env) ::- _ = initial'
            bounds' <- liftIO (A.getBounds (QLearningNoMemory._qTable env))
            liftIO
              (V.sequence_
                  [ writeRow
                    StateActionIndex' {state =  player2, action, index = i}
                  | player2 <- population ctable2
                  , action <- population ctable2
                  , let i =
                          Ix.index
                            bounds'
                            (toIdx action)
                  ])))

--------------------------------------------------------------------------------
-- Write QValues

writeQValues ::
     (MonadUnliftIO m)
  => ExportConfig a m
  -> MaximalState a
  -> List '[ ( a , Env a), ( a , Env a)]
  -> (QValueRow -> IO ())
  -> m (List '[ ( a , Env a), ( a , Env a)])
writeQValues ExportConfig {..} maximalState initial'@(p1_0 ::- p2_0 ::- Nil) writeRow = do
  writePlayerQTable 0 1 p1_0
  writePlayerQTable 0 2 p2_0
  mapStagesM_
    (\State {prev, iteration, skipping} (p1 ::- p2 ::- Nil) -> do
       let !percent = fromIntegral iteration / fromIntegral iterations * 100
           !save =
             if percent >= prev + 10
               then percent
               else prev
       let
        in when
             (iteration == 1
             -- ^ first iteration
             || iteration == iterations
             -- ^ last iteration
             || ((qValueExportMode == Full)
                && (skipping == 0)))
             -- ^ _Full_ export and a skipping period
             (do when
                   False
                   (putStrLn
                      ("Dumping QTable on iteration " <>
                       fromString (show iteration)))
                 writePlayerQTable iteration 1 p1
                 writePlayerQTable iteration 2 p2)
       terminate <- allPlayersTerminated maximalState
       pure
         (if terminate
            then QLearningNoMemory.Stop
            else QLearningNoMemory.Continue
                   (State
                      { prev = save
                      , iteration = iteration + 1
                      , skipping = Prelude.mod (skipping + 1) outputEveryN
                      })))
    initial'
    iterations
    State {prev = 0, iteration = 1, skipping = 1}
  where
    writePlayerQTable iteration player (_, env) = do
      liftIO
        (mapWithIndex_
           (\_i state_action_index qvalue ->
              writeRow QValueRow {state_action_index, ..})
           (QLearningNoMemory._qTable env))



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

--------------------------------------------------------------------------------
-- Maximal action tracking

-- | A counter for how many times we've seen this action before.
newtype Count = Count Int deriving (Num, Eq, Ord)
newtype Player = Player Int deriving (Num, Eq, Ord, Hashable)

data MaximalState a = MaximalState
  { table :: IORef (HashMap Player (a, Count))
  , flaggedPlayers :: IORef (Set Player)
  , terminate :: IORef Bool
  }

newMaximalState ::
     ( MonadIO m
     , Eq a
     )
  => m (MaximalState a)
newMaximalState = do
  table <- liftIO $ newIORef mempty
  flaggedPlayers <- liftIO $ newIORef mempty
  terminate <- liftIO $ newIORef False
  pure MaximalState {table, flaggedPlayers, terminate}

updateMaximalTables ::
     ( Eq a)
  => Int
  -> MaximalState  a
  -> QLearningNoMemory.MaximalAction a
  -> Int
  -> IO ()
updateMaximalTables threshold MaximalState {..} QLearningNoMemory.MaximalAction {..} playerCount = do
  !tbl <-
    fmap
      (HM.insertWith
         (\(actionNew, countNew) (actionOld, countOld) ->
            ( actionNew
            , if actionNew == actionOld
                then countOld + 1
                else countNew))
         key
         (maximalAction, 1))
      (readIORef table)
  writeIORef table tbl
  case HM.lookup key tbl of
    Just (_action, Count count)
      | count > threshold -> do
        !set' <- fmap (Set.insert (Player maximalPlayer)) (readIORef flaggedPlayers)
        writeIORef flaggedPlayers set'
        when (Set.size set' == playerCount) (writeIORef terminate True)
    _ -> pure ()
  where
    key = (Player maximalPlayer)

allPlayersTerminated :: MonadIO m => MaximalState a -> m Bool
allPlayersTerminated MaximalState {..} = liftIO (readIORef terminate)