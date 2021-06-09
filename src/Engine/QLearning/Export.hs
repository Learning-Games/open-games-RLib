{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DerivingStrategies #-}
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
  ( exportQValuesSqlite
  , exportingRewardsSqlite
  , exportQValuesCsv
  ) where

import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Logger
import           Control.Monad.Trans.Reader
import           Control.Monad.Trans.State
import           Data.Aeson
import qualified Data.Aeson as Aeson
import           Data.Array.Base as A
import           Data.Array.IO as A
import           Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.Builder as SB
import           Data.Double.Conversion.ByteString
import           Data.Foldable
import           Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import           Data.IORef
import qualified Data.Ix as Ix
import           Data.Pool
import           Data.Sequence (Seq(..))
import qualified Data.Sequence as Seq
import           Data.Text (Text)
import qualified Data.Text.Encoding as T
import           Database.Persist
import           Database.Persist.Sqlite
import           Database.Persist.TH
import qualified Engine.Memory as Memory
import           Engine.QLearning (Idx, QLearningMsg(..), Env, CTable)
import qualified Engine.QLearning as QLearning
import           Engine.TLL
import           FastCsv
import qualified RIO
import           RIO (MonadUnliftIO, RIO, GLogFunc)

--------------------------------------------------------------------------------
-- Schema

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
StateActionIndex
  state Text
  action Text
QValue
  iteration Int
  player Int
  stateActionIndex Int
  qvalue Double
  deriving Show
Reward
  iteration Int
  player Int
  stateActionIndex Int
  reward Double
  deriving Show
|]

--------------------------------------------------------------------------------
-- Types

data ExportQValues n o a = ExportQValues
   { expQValues  :: ![((Memory.Vector n (o (Idx a)),Idx a),Double)]
   , expQBounds :: !(Bounds n o a)
   , expKeys :: ![(Memory.Vector n (o (Idx a)), Idx a)]
   }

type Bounds n o a
   = ( (Memory.Vector n (o (Idx a)), Idx a)
     , (Memory.Vector n (o (Idx a)), Idx a))

data Out = Out
  { qvalues :: !(Seq QValue)
  , keys :: !(HashMap (ByteString, ByteString) Int)
  }

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

exportQValuesSqlite ::
     ( ToJSON a
     , Show (o (Idx a))
     , Ix (Memory.Vector n (o (Idx a)))
     , Functor (Memory.Vector n)
     , Functor o
     , Memory.Memory n
     , ToJSON (o a)
     )
  => [List '[(a, Env n o a), (a, Env n o a)]]
  -> CTable a
  -> IO ()
exportQValuesSqlite results actionSpace =
  runNoLoggingT
    (withSqlitePool
       "qvalues.sqlite3"
       1
       (flip
          withResource
          (runReaderT
             (do runMigration migrateAll
                 Out {..} <-
                   flip
                     execStateT
                     Out {qvalues = mempty, keys = mempty}
                     (mapM_
                        (\(iterationIdx, tll) -> do
                           players <- liftIO (fromTLLToExport tll)
                           mapM_
                             (\(player, exportQValues) -> do
                                let stateActionValueTriples =
                                      expQValues exportQValues
                                mapM_
                                  (\key@(state0, action) ->
                                     let idx =
                                           Ix.index
                                             (expQBounds exportQValues)
                                             key
                                         state' =
                                           fmap
                                             (fmap (QLearning.readTable actionSpace))
                                             state0
                                         action' = QLearning.readTable actionSpace action
                                      in modify'
                                           (\Out {..} ->
                                              Out
                                                { keys =
                                                    HM.insert
                                                      ( L.toStrict
                                                          (Aeson.encode state')
                                                      , L.toStrict
                                                          (Aeson.encode action'))
                                                      idx
                                                      keys
                                                , ..
                                                }))
                                  (expKeys exportQValues)
                                mapM_
                                  (\(stateAndAction, value) -> do
                                     let newqvalue =
                                           QValue
                                             { qValueIteration = iterationIdx
                                             , qValuePlayer = player
                                             , qValueStateActionIndex =
                                                 Ix.index
                                                   (expQBounds
                                                      exportQValues)
                                                   stateAndAction
                                             , qValueQvalue = value
                                             }
                                     modify'
                                       (\Out {..} ->
                                          Out
                                            { qvalues = qvalues Seq.|> newqvalue
                                            , ..
                                            }))
                                  stateActionValueTriples)
                             (zip [1 ..] players))
                        (zip [1 ..] results))
                 insertEntityMany
                   (map
                      (\((state', action), idx) ->
                         Entity
                           { entityVal =
                               StateActionIndex
                                 { stateActionIndexState = T.decodeUtf8 state'
                                 , stateActionIndexAction = T.decodeUtf8 action
                                 }
                           , entityKey = toSqlKey (fromIntegral idx)
                           })
                      (HM.toList keys))
                 insertMany_ (toList qvalues)))))

-- | Extract relevant information into a record to be exported
fromTLLToExport ::
     (Ix (Memory.Vector n (o (Idx a))))
  => List '[ (a, Env n o a), (a, Env n o a)]
  -> IO [ExportQValues n o a]
fromTLLToExport (p1 ::- p2 ::- Nil) = do
  let ((_, env1)) = p1
      ((_, env2)) = p2

  expQValues1 <- A.getAssocs $ QLearning._qTable env1
  expQValues2 <- A.getAssocs $ QLearning._qTable env2
  bounds1 <- A.getBounds (QLearning._qTable env1)
  bounds2 <- A.getBounds (QLearning._qTable env2)
  keys1 <- fmap (fmap fst) (A.getAssocs (QLearning._qTable env1))
  keys2 <- fmap (fmap fst) (A.getAssocs (QLearning._qTable env1))
  let
      expPlayer1 = ExportQValues expQValues1 bounds1 keys1
      expPlayer2 = ExportQValues expQValues2 bounds2 keys2
  pure $ [expPlayer1, expPlayer2]

--------------------------------------------------------------------------------
-- Fast CSV export

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

instance BuildHeaders QValueRow where
  buildHeaders _ = "iteration,player,state_action_index,qvalue"

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
     )
  => Int
  -> (   (Int -> List '[ (a, Env n o a), (a, Env n o a)] -> m ())
      -> List '[ ( a , Env n o a), ( a , Env n o a)]
      -> Int
      -> m ())
  -> List '[ ( a , Env n o a), ( a , Env n o a)]
  -> CTable a
  -> m ()
exportQValuesCsv steps mapStagesM_ initial _ctable =
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
