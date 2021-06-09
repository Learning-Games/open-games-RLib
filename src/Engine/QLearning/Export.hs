{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications, DuplicateRecordFields #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DerivingStrategies, MonadComprehensions #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DeriveGeneric, OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveGeneric #-}

-- |

module Engine.QLearning.Export
  ( exportingRewardsSqlite
  , exportQValuesCsv
  ) where

import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Logger
import           Control.Monad.Trans.Reader
import           Data.Aeson
import           Data.Array.Base as A
import           Data.Array.IO as A
import qualified Data.ByteString.Char8 as S8
import qualified Data.ByteString.Lazy.Builder as SB
import           Data.Double.Conversion.ByteString
import           Data.Foldable
import           Data.IORef
import qualified Data.Ix as Ix
import           Data.Pool
import qualified Data.Sequence as Seq
import qualified Data.Vector as V
import qualified Data.Vector.Sized as SV
import           Database.Persist
import           Database.Persist.Sqlite
import           Database.Persist.TH
import qualified Engine.Memory as Memory
import           Engine.QLearning (ToIdx, toIdx, Idx, QLearningMsg(..), Env, CTable(..))
import qualified Engine.QLearning as QLearning
import           Engine.TLL
import           FastCsv
import qualified RIO
import           RIO (MonadUnliftIO, RIO, GLogFunc)

--------------------------------------------------------------------------------
-- Schema

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Reward
  iteration Int
  player Int
  stateActionIndex Int
  reward Double
  deriving Show
|]

--------------------------------------------------------------------------------
-- Top-level functions

stackSize :: Int
stackSize = 1000

exportingRewardsSqlite ::
     RIO (GLogFunc (QLearningMsg n o a)) x
  -> IO x
exportingRewardsSqlite m =
  runNoLoggingT
    (withSqlitePool
       "rewards.sqlite3"
       1
       (\pool -> do
          withResource pool (runReaderT (runMigration migrateAll))
          stackRef <- liftIO (newIORef mempty)
          result <-
            RIO.runRIO
              (RIO.mkGLogFunc
                 (\_backtrace msg ->
                    case msg of
                      RewardMsg QLearning.Reward {..} -> do
                        let reward =
                              Reward
                                { rewardIteration
                                , rewardPlayer
                                , rewardStateActionIndex
                                , rewardReward
                                }
                        modifyIORef stackRef (Seq.|> reward)
                        stack <- readIORef stackRef
                        when
                          (length stack > stackSize)
                          (do writeIORef stackRef mempty
                              withResource
                                pool
                                (runReaderT (insertMany_ (toList stack))))))
              m
          stack <- liftIO (readIORef stackRef)
          when
            (not (null stack))
            (withResource pool (runReaderT (insertMany_ (toList stack))))
          pure result))

--------------------------------------------------------------------------------
-- Fast CSV export

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
