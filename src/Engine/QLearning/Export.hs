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
  ) where

import           Control.Monad.IO.Class
import           Control.Monad.Logger
import           Control.Monad.Trans.Reader
import           Control.Monad.Trans.State
import           Data.Aeson
import qualified Data.Aeson as Aeson
import           Data.Array.IO as A
import           Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as L
import           Data.Foldable
import           Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
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


import           Engine.QLearning
import           Engine.TLL

--------------------------------------------------------------------------------
-- Schema

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
StateActionIndex
  state Text
  action Text
QValue
  iteration Int
  player Int
  stateActionIndex StateActionIndexId
  qvalue Double
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

exportQValuesSqlite ::
     ( ToJSON a
     , Show (o (Idx a))
     , Ix (Memory.Vector n (o (Idx a)))
     , Functor (Memory.Vector n)
     , Functor o
     , Memory.Memory n
     , ToJSON (o a)
     )
  => [List '[ (a, Env n o a), (a, Env n o a)]]
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
                                             (fmap (readTable actionSpace))
                                             state0
                                         action' = readTable actionSpace action
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
                                                 toSqlKey
                                                   (fromIntegral
                                                      (Ix.index
                                                         (expQBounds
                                                            exportQValues)
                                                         stateAndAction))
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

  expQValues1 <- A.getAssocs $ _qTable env1
  expQValues2 <- A.getAssocs $ _qTable env2
  bounds1 <- A.getBounds (_qTable env1)
  bounds2 <- A.getBounds (_qTable env2)
  keys1 <- fmap (fmap fst) (A.getAssocs (_qTable env1))
  keys2 <- fmap (fmap fst) (A.getAssocs (_qTable env1))
  let
      expPlayer1 = ExportQValues expQValues1 bounds1 keys1
      expPlayer2 = ExportQValues expQValues2 bounds2 keys2
  pure $ [expPlayer1, expPlayer2]
