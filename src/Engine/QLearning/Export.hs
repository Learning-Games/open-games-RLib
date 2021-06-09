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
  ) where

import           Control.Monad.IO.Class
import           Data.Aeson
import           Data.Array.Base as A
import           Data.Array.IO as A
import qualified Data.ByteString.Char8 as S8
import qualified Data.ByteString.Lazy.Builder as SB
import           Data.Double.Conversion.ByteString
import           Data.Foldable
import qualified Data.Ix as Ix
import qualified Data.Vector as V
import qualified Data.Vector.Sized as SV
import qualified Engine.Memory as Memory
import           Engine.QLearning (ToIdx, toIdx, Idx, QLearningMsg(..), Env, CTable(..))
import qualified Engine.QLearning as QLearning
import           Engine.TLL
import           FastCsv
import qualified RIO
import           RIO (MonadUnliftIO, RIO, GLogFunc)

--------------------------------------------------------------------------------
-- Schema

data StateActionIndex' state action = StateActionIndex'
  { state :: state
  , action :: action
  , index :: Int
  }

instance BuildHeaders (StateActionIndex' a b) where
  buildHeaders _ = "state_action_index,action,qvalue"
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
  => Int
  -> (   (Int -> List '[ (a, Env n o a), (a, Env n o a)] -> m ())
      -> List '[ ( a , Env n o a), ( a , Env n o a)]
      -> Int
      -> m ())
  -> List '[ ( a , Env n o a), ( a , Env n o a)]
  -> CTable a
  -> (forall x. x -> x -> o x)
  -> m ()
exportQValuesCsv steps mapStagesM_ initial (CTable {population}) mkObservation = do
  liftIO (S8.putStrLn "Writing state action index ...")
  withCsvFile
    "state_action_index.csv"
    (\writeRow -> do
       let (_, env) ::- _ = initial
       bounds' <- liftIO (A.getBounds (QLearning._qTable env))
       liftIO
         (V.sequence_
            [ writeRow
              StateActionIndex' {state = (player1, player2), action, index = i}
            | player1 <- population
            , player2 <- population
            , action <- population
            , let i =
                    Ix.index
                      bounds'
                      ( Memory.fromSV
                          (SV.replicate
                             (mkObservation (toIdx player1) (toIdx player2)))
                      , toIdx action)
            ]))
  liftIO (S8.putStrLn "Running iterations ...")
  withCsvFile
    "qvalues.csv"
    (\writeRow ->
       mapStagesM_
         (\iteration (p1 ::- p2 ::- Nil) -> do
            let writePlayer player (_, env) = do
                  liftIO
                    (mapWithIndex_
                       (\state_action_index qvalue ->
                          writeRow QValueRow {state_action_index, ..})
                       (QLearning._qTable env))
             in do writePlayer 1 p1
                   writePlayer 2 p2)
         initial
         steps)

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
