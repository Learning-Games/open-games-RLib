{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE NamedFieldPuns, PolyKinds #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveGeneric #-}
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

module Engine.QLearningNoMemory where

import           Control.DeepSeq
import           Control.Monad.Reader
import qualified Control.Monad.Trans.State as ST
import           Data.Aeson
import qualified Data.Array.IO as A
import           Data.Csv
import           Data.Hashable
import           Data.Ix
import qualified Data.Ix as Ix
import qualified Data.Vector as V
import           Engine.OpenGames hiding (lift)
import           Engine.OpticClass
import           Engine.TLL
import           GHC.Generics
import           Optics.Operators
import           Optics.Optic ((%))
import           Optics.TH (makeLenses)
import qualified RIO
import           RIO (glog, HasGLogFunc(..))
import           System.Random
import qualified System.Random as Rand
import           System.Random.MWC.CondensedTable
import           System.Random.Stateful

--------------------------------------------------------------------------------
-- Logging

-- | When evaluating stages with evalStageM*, we can decide to
-- stop. This is the type used to model that.
data Decision a = Continue a | Stop

data QLearningMsg a
  = RewardMsg !(Reward a)
  | QTableDirtied !(Dirtied a)
  | RewardDiagnosticsMsg !(RewardDiagnostics a)
  | GotMaximalActionForState !(MaximalAction a)

data MaximalAction a = MaximalAction
  { maximalAction :: !a
  , maximalPlayer :: !Int
  }

data RewardExportType =
    RewardExport
  | RewardExtendedExport
     deriving (Eq)

data Dirtied a = Dirtied
  { dirtiedIteration :: !Int
  , dirtiedPlayer :: !Int
  , dirtiedStateActionIndex :: !Int
  , dirtiedQValue :: !Double
  }

data Reward a = Reward
  { rewardPlayer :: !Int
  , rewardIteration :: !Int
  , rewardStateAction :: a
  , rewardStateActionIndex :: !Int
  , rewardReward :: !Double
  }

data RewardDiagnostics a = RewardDiagnostics
  { rewardDiagPlayer :: !Int
  , rewardDiagIteration :: !Int
  , rewardDiagStateAction :: a
  , rewardDiagStateActionIndex :: !Int
  , rewardDiagActionChoice :: !ActionChoice
  , rewardDiagExploreRate :: !ExploreRate
  , rewardDiagReward :: !Double
  }

type ActionChoice = String
--------------------------------------------------------------------------------
-- A simple condensed table type

data CTable a = CTable
  { ctable :: !(CondensedTableV a)
  , population :: !(V.Vector a)
  }

-- | Create a uniform distribution condensed table.
uniformCTable :: V.Vector a -> CTable a
uniformCTable population =
  CTable
    { population
    , ctable = tableFromProbabilities (fmap (, probability) population)
    }
  where
    probability = 1 / fromIntegral (V.length population)

-- | Read the table at an index.
readTable :: CTable a -> Idx a -> a
readTable (CTable {population}) (Idx i) =
  population V.! i

--------------------------------------------------------------------------------
-- Random sampling

{-# INLINE samplePopulation #-}
samplePopulation :: CTable a -> StdGen -> (a, StdGen)
samplePopulation population gen = runStateGen gen $ genFromTable (ctable population)

{-# INLINE samplePopulation_ #-}
samplePopulation_ :: CTable a -> StdGen -> a
samplePopulation_ population gen = fst $ samplePopulation population gen

--------------------------------------------------------------------------------
-- | Indexable values
-- We use this type for constructing indices out of specific action spaces (for each application)
newtype Idx a = Idx Int deriving (A.Ix, Eq, Ord, Show, NFData, ToJSON, Hashable, ToField)
class ToIdx a where
  toIdx :: a -> Idx a

--------------------------------------------------------------------------------
-- Comments

-- TODO check the random update
-- TODO take care of the range of values being used by array on one side and by the random module on the other side
-- TODO Replace the array access functionality with a lens
-- NOTE The _x_ input needs to be manipulated in the outside game definition

------------------
-- 0 Preliminaries

type Agent = String

type Temperature = Double

type ExploreRate = Double

type LearningRate = Double

type DiscountFactor = Double

type QTable a = A.IOUArray (Idx a) Double

-- Complete state comprises the internal state of the agent -- mainly the qmatrix
-- and the external state, the observation made in the last round
-- TODO needs to be generalized

data State a = State
  { _env :: Env a
  }

data Env a = Env
  { _name   :: String
  , _player :: Int
  , _qTable :: QTable a
  , _iteration  :: Int
  , _exploreRate :: ExploreRate
  , _randomGen :: Rand.StdGen
  , _temperature :: Temperature
  , _actionChoice :: ActionChoice
  , _stageNewValue :: Double
  }  deriving (Generic)
deriving instance (Show a) => Show (Env a)
-- ^ Added here the agent observation the idea is that global and local information might diverge
instance (NFData a) => NFData (Env a)
instance Show (QTable a) where show _ = "QTable"
instance NFData (QTable a) where rnf _ = ()



type QLearningStageGame m a b x s y r = OpenGame (MonadOptic m) (MonadContext m) a b x s y r

makeLenses ''Env
makeLenses ''State


------------------------
-- 1 Auxiliary functions
-- and updating functions
-- given a q-table, derive maximal score and the action that guarantees it
{-# INLINE maxScore #-}
maxScore ::
     ( ToIdx a
     , Ord a
     , MonadIO m
     , MonadReader r m
     , HasGLogFunc r
     , GMsg r ~ QLearningMsg a
     )
  => QTable a
  -> CTable a
  -> Int
  -> m (Double, a)
maxScore table0 support player = do
  valuesAndActions <-
    liftIO
      (V.mapM
         (\action -> do
            let index = toIdx action
            value <- A.readArray table0 index
            pure (value, action))
         (population support))
  let !maximum' = V.maximum valuesAndActions
  glog
    (GotMaximalActionForState
       MaximalAction
         {maximalAction = (snd maximum'), maximalPlayer = player})
  pure maximum'

-- better prepare output to be used
extractFst :: Maybe (a,b) -> Maybe a
extractFst Nothing      = Nothing
extractFst (Just (a,b)) = Just a

extractSnd :: Maybe (a,b) -> Maybe b
extractSnd Nothing      = Nothing
extractSnd (Just (a,b)) = Just b


-----------------------
-- Updating information
-----------------------

-----------------
-- Single updates

-- Update randomG
updateRandomG :: State a -> Rand.StdGen -> State a
updateRandomG s r = env % randomGen .~ r  $  s

-- Update QTable
updateQTable :: State a -> QTable a  -> State a
updateQTable s q = env % qTable .~ q  $  s

--- Update iterator
updateIteration :: State a -> State a
updateIteration = env % iteration %~ (+ 1)

-- Update temperature
updateTemperature :: Temperature -> State a ->  State a
updateTemperature decreaseFactor = env % temperature %~ (* decreaseFactor)

-- Update explorRate
updateExploreRate :: ExploreRate -> State a -> State a
updateExploreRate decreaseFactor = env % exploreRate %~ (* ((exp 1) ** (-decreaseFactor)))

-- Update reward
updateNewValue :: Double -> State a -> State a
updateNewValue value s = env % stageNewValue .~ value $ s

-- Update action choice info
updateActionChoice ::  ActionChoice -> State a -> State a
updateActionChoice info s = env % actionChoice .~ info $ s


------------------
-- Combine updates

-- Update gen, agentObs, iteration
updateAll :: Double -> ExploreRate ->   Rand.StdGen -> State a -> State a
updateAll  value decreaseFactor r s =
  updateExploreRate decreaseFactor $
     updateNoLearning value r s

-- Update gen, agentObs, iteration, stage reward, previous memory
updateNoLearning :: Double -> Rand.StdGen -> State a -> State a
updateNoLearning value r s =
    updateNewValue value $
      updateIteration $
          updateRandomG s r


-----------------------------------
-- 2 Implementation based on StateT
-- TODO simplify this analysis; redundancy between choosing and not

-- 2.1. e-greedy experimentation
-- | Choose optimally given qmatrix; do not explore. This is for the play part
-- Does _not_ update the matrix
{-# INLINE chooseNoExploreAction  #-}
chooseNoExploreAction :: (MonadIO m, MonadReader r m, HasGLogFunc r, GMsg r ~ QLearningMsg a, Ord a, ToIdx a) =>
  CTable a -> State a -> m (a, ActionChoice)
chooseNoExploreAction support s = do
  maxed <- maxScore (_qTable $ _env s) support (_player (_env s))
  let (exploreR, gen') = Rand.randomR (0.0 :: Double, 1.0 :: Double) (_randomGen $ _env s)
      optimalAction = snd $  maxed
  return (optimalAction, "Exploitation w/o updates")

-- | Choose the optimal action given the current state or explore; indicate whether exploration tool place (False) or randomization tool place (True)
{-# INLINE chooseExploreAction #-}
chooseExploreAction :: (MonadIO m, MonadReader r m, HasGLogFunc r, GMsg r ~ QLearningMsg a, Ord a, ToIdx a) =>
  CTable a -> State a -> m (a,ActionChoice)
chooseExploreAction support s = do
  -- NOTE: gen'' is not updated anywhere...!!!
  let (exploreR, gen') = Rand.randomR (0.0, 1.0) (_randomGen $ _env s)
  if exploreR < _exploreRate (_env s)
    then do
      let !action' = samplePopulation_ support gen'
      return (action',"Randomization")
    else do
      maxed <- maxScore (_qTable $ _env s) support (_player (_env s))
      let optimalAction = snd $  maxed
      return (optimalAction,"Exploitation")




-- 2.2. Different updates of the qmatrix depending on learning form
-- | Given an action, state, obs and a reward, update the qmatrix with decreasing exploration rate
{-# INLINE chooseLearnDecrExploreQTable #-}
chooseLearnDecrExploreQTable ::  (MonadIO m, MonadReader r m, HasGLogFunc r, GMsg r ~ QLearningMsg a, Ord a, ToIdx a) =>
                     LearningRate ->  DiscountFactor ->  ExploreRate -> CTable a -> State a -> (a,ActionChoice) -> Double ->  ST.StateT (State a) m a
chooseLearnDecrExploreQTable learningRate gamma decreaseFactorExplore support s (action,info) reward  = do
       let table0             = _qTable $ _env s
       prediction    <- liftIO $ A.readArray table0 (toIdx action)
       maxed <- maxScore table0 support (_player (_env s))
       let  (_,gen')     = Rand.randomR (0.0 :: Double, 1.0 :: Double) (_randomGen $ _env s)
            updatedValue = reward + gamma * (fst $ maxed)
            newValue     = (1 - learningRate) * prediction + learningRate * updatedValue
       recordingArray (_iteration (_env s)) (_player (_env s)) table0 (toIdx action) newValue
       ST.put $  updateAll newValue decreaseFactorExplore gen' s
       return action

-- | Given an action, state, obs and a reward, update the qmatrix with decreasing exploration rate
{-# INLINE chooseNoLearnDecrExploreQTable #-}
chooseNoLearnDecrExploreQTable ::  (MonadIO m, MonadReader r m, HasGLogFunc r, GMsg r ~ QLearningMsg a, Ord a, ToIdx a) =>
                     LearningRate ->  DiscountFactor ->  ExploreRate -> CTable a -> State a -> (a,ActionChoice) -> Double ->  ST.StateT (State a) m a
chooseNoLearnDecrExploreQTable learningRate gamma decreaseFactorExplore support s (action,info) reward  = do
       let (_,gen')     = Rand.randomR (0.0 :: Double, 1.0 :: Double) (_randomGen $ _env s)
           table0       = _qTable $ _env s
       prediction    <- liftIO $ A.readArray table0 (toIdx action)
       recordingArray (_iteration (_env s)) (_player (_env s)) table0 (toIdx action) prediction
       ST.put $  updateNoLearning prediction gen' s
       return action

----------------------------------------------------
-- Helper function for logging qvalues incrementally
{-# INLINE recordingArray #-}
recordingArray :: (MonadIO m, A.MArray a1 Double IO, Ix i, HasGLogFunc env, MonadReader env m, GMsg env ~ QLearningMsg a2) => Int -> Int -> a1 i Double -> i -> Double -> m ()
recordingArray dirtiedIteration dirtiedPlayer table0 index' value = do
  bounds <- liftIO (A.getBounds table0)
  RIO.glog
    (QTableDirtied
       Dirtied
         { dirtiedIteration
         , dirtiedPlayer
         , dirtiedStateActionIndex = Ix.index bounds index'
         , dirtiedQValue = value
         })

--------------------------------
-- Helper function for exporting
-- Determines kind of information being exported
exportRewards ::
    RewardExportType
  -> Int
  -> Int
  -> a
  -> Int
  -> ActionChoice
  -> ExploreRate
  -> Double
  -> QLearningMsg a
exportRewards exportType player iteration stateAction stateActionIndex actionChoice exploreRate reward
  | exportType ==  RewardExtendedExport =
         (RewardDiagnosticsMsg
             RewardDiagnostics
                { rewardDiagPlayer           = player
                , rewardDiagIteration        = iteration
                , rewardDiagStateAction      = stateAction
                , rewardDiagStateActionIndex = stateActionIndex
                , rewardDiagActionChoice     = actionChoice
                , rewardDiagExploreRate      = exploreRate
                , rewardDiagReward           = reward
                })
  | otherwise =
         (RewardMsg
             Reward
                { rewardPlayer           = player
                , rewardIteration        = iteration
                , rewardStateAction      = stateAction
                , rewardStateActionIndex = stateActionIndex
                , rewardReward           = reward
                })
----------------------------
-- Define a Qlearning Config
data ConfigQLearning a m = ConfigQLearning
  { chooseActionFunction :: (CTable a -> State a ->  m (a,ActionChoice))
  , updateFunction :: (CTable a -> State a -> (a,ActionChoice) -> Double -> ST.StateT (State a) m a)
  , exportType :: RewardExportType}

-----------------
-- Open game definition for Qlearning

{-# INLINE pureDecisionQStage #-}
pureDecisionQStage ::
     ( MonadIO m
     , MonadReader r m
     , HasGLogFunc r
     , GMsg r ~ QLearningMsg a
     , ToIdx a
     )
  => ConfigQLearning a m
  -> CTable a
  -> Agent
  -> QLearningStageGame m '[ m (a, Env a)] '[ m (a, Env a)] () () a Double
pureDecisionQStage ConfigQLearning {..} actionSpace name = OpenGame {
  play =  \(strat ::- Nil) -> let  v _ = do
                                           (_,env') <- strat
                                           let s  = State env'
                                           (action,_)   <- chooseActionFunction actionSpace s
                                           pure ((),action)
                                        in MonadOptic v (\_ -> (\_ -> pure ())),
  -- ^ This finds the optimal action or chooses randomly
  evaluate = \(strat ::- Nil) (MonadContext h k) ->
              let
                output = do
                   (_,pdenv') <- strat
                   (z,_) <- h
                   -- ^ Take the (old observation) from the context
                   (action,actionChoiceType) <- chooseActionFunction actionSpace (State pdenv')
                   reward <- k z action
                   bounds <- liftIO (A.getBounds (_qTable pdenv'))
                   RIO.glog (exportRewards
                                exportType
                                (_player pdenv')
                                (_iteration pdenv')
                                (action)
                                (Ix.index bounds (toIdx action))
                                actionChoiceType
                                (_exploreRate pdenv')
                                reward)
                   (State env') <- ST.execStateT (updateFunction actionSpace (State pdenv') (action,actionChoiceType) reward)
                                                   (State pdenv')
                   return (action,env')
                in (output ::- Nil)}


--------------------------
-- 4 Further functionality


{-# INLINE fromLens #-}
fromLens :: Monad m => (x -> y) -> (x -> r -> s) -> QLearningStageGame m '[] '[] x s y r
fromLens v u = OpenGame {
  play = \Nil -> MonadOptic (\x -> pure (x, v x)) (\x -> (\r -> pure $ u x r)),
  evaluate = \Nil _ -> Nil}



{-# INLINE fromFunctions #-}
fromFunctions :: Monad m => (x -> y) -> (r -> s) -> QLearningStageGame m '[] '[] x s y r
fromFunctions f g = fromLens f (const g)
