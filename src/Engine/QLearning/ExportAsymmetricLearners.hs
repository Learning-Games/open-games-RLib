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

module Engine.QLearning.ExportAsymmetricLearners
  ( runQLearningExportingDiagnostics
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
import qualified Engine.Memory as Memory
import           Engine.QLearning (ToIdx, toIdx, Idx, QLearningMsg(..), Env, CTable(..),ActionChoice)
import qualified Engine.QLearning as QLearning
import           Engine.TLL
import           FastCsv
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
  , threshold :: Int
    -- ^ How many iterations before we consider a player's consecutive
    -- runs of any same (state,maximal-action) pair before flagging
    -- that player as converged. After all players are converged, the
    -- iterations will terminate.
  , initial :: m (List '[ ( a , Env n o a), ( a , Env n o a)])
    -- ^ Initial strategy.
  , ctable1 :: CTable a
    -- ^ Action space player 1.
  , ctable2 :: CTable a
    -- ^ Action space player 2.
  , mkObservation :: forall x. x -> x -> o x
    -- ^ How to make an observation of two player actions.
  , mapStagesM_ ::
    forall s.
       (s -> List '[ (a, Env n o a), (a, Env n o a)] -> m (QLearning.Decision s))
    -> List '[ ( a , Env n o a), ( a , Env n o a)]
    -> Int -- ^ Count.
    -> s -- ^ State.
    -> m (List '[ (a, Env n o a), (a, Env n o a)])
    -- ^ How to map over stages step by step.
  , runName :: String
    -- ^ Description of file name; main purpose is to run several estimations of the same kind
  , players :: Int
  }

-----------------------
-- File paths

rewardsFile :: Path b t
rewardsFile                  = [relfile|rewards.csv|]
rewardsExtendedFile :: Path b t
rewardsExtendedFile          = [relfile|rewardsExtended.csv|]
qValuesFile :: Path b t
qValuesFile                  = [relfile|qvalues.csv|]
stateActionIndexFile         = [relfile|state_action_index.csv|]

--------------------------------------------------------------------------------
-- Top-level functions

{-# INLINE runQLearningExportingDiagnostics #-}
runQLearningExportingDiagnostics ::
     ( ToJSON a
     , Show (o (Idx a))
     , Ix (Memory.Vector n (o (Idx a)))
     , Hashable (Memory.Vector n (o (Idx a)))
     , Hashable (o (Idx a))
     , Functor (Memory.Vector n)
     , Functor o
     , Memory.Memory n
     , ToJSON (o a)
     , ToIdx a
     , BuildCsvField a
     , BuildCsvField (a, a)
     , Eq a
     , n ~ 1
     )
  => ExportConfig n o a (RIO (GLogFunc (QLearningMsg n o a)))
  -> IO (List '[ ( a , Env n o a), ( a , Env n o a)])
runQLearningExportingDiagnostics exportConfig = do
  liftIO (hSetBuffering RIO.stdout NoBuffering)
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
              _ -> mempty))
      (do initial' <- initial exportConfig
          keepLastQValue
            exportConfig
            maximalState
            initial'
      )


--------------------------------------------------------------------------------
-- Write QValues

-- | State for the writeQValueRow function's loop.
data State n o a = State
  { prev :: Double -- ^ Previous percentage complete.
  , iteration :: Int -- ^ Iteration number.
  , skipping :: Int -- ^ Skip every N dumps.
  }

 
keepLastQValue ::
     (MonadUnliftIO m, Ix (Memory.Vector n (o (Idx a))))
  => ExportConfig n o a m
  -> MaximalState n o a
  -> List '[ ( a , Env n o a), ( a , Env n o a)]
  -> m (List '[ ( a , Env n o a), ( a , Env n o a)])
keepLastQValue ExportConfig {..} maximalState initial'@(p1_0 ::- p2_0 ::- Nil) = do
  putStrLn "Running iterations ..."
  mapStagesM_
    (\State {prev, iteration, skipping} (p1 ::- p2 ::- Nil) -> do
       let !percent = fromIntegral iteration / fromIntegral iterations * 100
           !save =
             if percent >= prev + 10
               then percent
               else prev
       when (save /= prev) (putStrLn (toFixed 2 percent <> "% complete ..."))
       terminate <- allPlayersTerminated maximalState
       when terminate (putStrLn "Terminating early because all players converged.")
       pure
         (if terminate
            then QLearning.Stop
            else (QLearning.Continue
                    State
                      { prev = save
                      , iteration = iteration + 1
                      , skipping = Prelude.mod (skipping + 1) outputEveryN
                      })))
    initial'
    iterations
    State {prev = 0, iteration = 1, skipping = 1}


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

data MaximalState n o a = MaximalState
  { table :: IORef (HashMap (Player, Memory.Vector n (o (Idx a))) (a, Count))
  , flaggedPlayers :: IORef (Set Player)
  , terminate :: IORef Bool
  }

newMaximalState ::
     ( Hashable (o (Idx a))
     , MonadIO m
     , Memory.Memory n
     , Eq a
     , Eq (Memory.Vector n (o (Idx a)))
     )
  => m (MaximalState n o a)
newMaximalState = do
  table <- liftIO $ newIORef mempty
  flaggedPlayers <- liftIO $ newIORef mempty
  terminate <- liftIO $ newIORef False
  pure MaximalState {table, flaggedPlayers, terminate}

updateMaximalTables ::
     ( Hashable (o (Idx a))
     , Memory.Memory n
     , Eq a
     , Eq (Memory.Vector n (o (Idx a)))
     )
  => Int
  -> MaximalState n o a
  -> QLearning.MaximalAction n o a
  -> Int
  -> IO ()
updateMaximalTables threshold MaximalState {..} QLearning.MaximalAction {..} playerCount = do
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
    key = (Player maximalPlayer, maximalState)

allPlayersTerminated :: MonadIO m => MaximalState n o a -> m Bool
allPlayersTerminated MaximalState {..} = liftIO (readIORef terminate)