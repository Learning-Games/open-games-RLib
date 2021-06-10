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

-- |

module Engine.QLearning.Export
  ( exportingRewardsCsv
  , exportQValuesCsv
  , ExportConfig(..)
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
import qualified Data.Ix as Ix
import           Data.Time
import qualified Data.Vector as V
import qualified Data.Vector.Sized as SV
import qualified Engine.Memory as Memory
import           Engine.QLearning (ToIdx, toIdx, Idx, QLearningMsg(..), Env, CTable(..))
import qualified Engine.QLearning as QLearning
import           Engine.TLL
import           FastCsv
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
  buildHeaders _ = "state,action,action,index"
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
  , initial :: List '[ ( a , Env n o a), ( a , Env n o a)]
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
  }

--------------------------------------------------------------------------------
-- Top-level functions

{-# INLINE exportingRewardsCsv #-}
exportingRewardsCsv :: RIO (GLogFunc (QLearningMsg n o a)) () -> IO ()
exportingRewardsCsv m =
  withCsvFile
    "rewards.csv"
    (\writeRow -> do
       RIO.runRIO
         (RIO.mkGLogFunc
            (\_backtrace msg ->
               case msg of
                 RewardMsg QLearning.Reward {..} -> do
                   writeRow
                     Reward
                       { iteration = rewardIteration
                       , player = rewardPlayer
                       , state_action_index = rewardStateActionIndex
                       , reward = rewardReward
                       }))
         m)

-- | State for the exportQValuesCsv function's loop.
data State = State
  { prev :: Double -- ^ Previous percentage complete.
  , iteration :: Int -- ^ Iteration number.
  , skips :: Int -- ^ How many iterations to skip before writing an output.
  }

{-# INLINE exportQValuesCsv #-}
exportQValuesCsv ::
     ( ToJSON a
     , Show (o (Idx a))
     , Ix (Memory.Vector n (o (Idx a)))
     , Functor (Memory.Vector n)
     , Functor o
     , Memory.Memory n
     , ToJSON (o a)
     , MonadUnliftIO m
     , ToIdx a
     , BuildCsvField a
     , BuildCsvField (a, a)
     , n ~ 1
     )
  => ExportConfig n o a m
  -> m ()
exportQValuesCsv ExportConfig {..} = do
  liftIO (hSetBuffering RIO.stdout NoBuffering)
  putStrLn "Writing state action index ..."
  withCsvFile
    "state_action_index.csv"
    (\writeRow -> do
       let (_, env) ::- _ = initial
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
  putStrLn "Running iterations ..."
  withCsvFile
    "qvalues.csv"
    (\writeRow ->
       mapStagesM_
         (\State {prev, iteration, skips} (p1 ::- p2 ::- Nil) -> do
            let percent :: Double =
                  fromIntegral iteration / fromIntegral iterations * 100
                save =
                  if percent >= prev + 10
                    then percent
                    else prev
            when
              (save /= prev)
              (putStrLn (toFixed 2 percent <> "% complete ..."))
            let writePlayer player (_, env) = do
                  liftIO
                    (mapWithIndex_
                       (\state_action_index qvalue ->
                          writeRow QValueRow {state_action_index, ..})
                       (QLearning._qTable env))
            when
              (outputEveryN < 2 || skips == 0)
              (do writePlayer 1 p1
                  writePlayer 2 p2)
            pure
              State
                { prev = save
                , iteration = iteration + 1
                , skips = mod (skips + 1) outputEveryN
                })
         initial
         iterations
         State {prev = 0, iteration = 1, skips = 1})

-- | A slightly more efficient mapper -- speculated.
mapWithIndex_ :: (MArray a e m, Ix i) => (Int -> e -> m ()) -> a i e -> m ()
mapWithIndex_ f marr = do
  n <- A.getNumElements marr
  bounds' <- A.getBounds marr
  for_
    (range bounds')
    (\i -> do
       let !idx = safeIndex bounds' n i
       !e <- unsafeRead marr idx
       f idx e)
{-# INLINE mapWithIndex_ #-}

--------------------------------------------------------------------------------
-- Threaded IO

-- | This is a thread-safe stdout printer, with timestamp.
putStrLn :: MonadIO m => ByteString -> m ()
putStrLn s =
  liftIO
    (do now' <- getCurrentTime
        S8.putStrLn (S8.pack (show now') <> ": " <> s))
