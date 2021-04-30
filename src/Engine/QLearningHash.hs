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

module Engine.QLearningHash where

import           Control.DeepSeq
import           Control.Monad.Trans
import           Control.Monad.Trans.Reader
import           Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import           Data.Hashable
import           Data.Maybe
import           Data.STRef
import           Engine.OpenGames hiding (lift)
import           Engine.OpticClass
import           Engine.TLL
import           GHC.Generics

import           Control.Comonad
import           Control.Monad.State.Class
import qualified Control.Monad.Trans.State as ST
import           Data.List (maximumBy)
import           Data.Ord (comparing)
import qualified System.Random as Rand
import qualified GHC.Arr as A



import           Optics.TH (makeLenses)
import           Optics.Optic ((%))
import           Optics.Operators

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

type QTable a = HashMap (Observation a, a) Double


type Observation a = (a,a)

-- Complete state comprises the internal state of the agent -- mainly the qmatrix
-- and the external state, the observation made in the last round
-- TODO needs to be generalized

data State a = State
  { _env :: Env a
  , _obs :: Observation a
  }  deriving (Show)


data Env a = Env
  { _name   :: String
  , _qTable :: QTable a
  , _iteration  :: Int
  , _exploreRate :: ExploreRate
  , _randomGen :: Rand.StdGen
  , _obsAgent :: Observation a
  , _temperature :: Temperature
  }  deriving (Show, Generic)
-- ^ Added here the agent observation the idea is that global and local information might diverge
instance NFData a => NFData (Env a)


type QLearningStageGame m a b x s y r = OpenGame (MonadOptic m) (MonadContext m) a b x s y r

makeLenses ''Env
makeLenses ''State


------------------------
-- 1 Auxiliary functions
-- and updating functions
-- given a q-table, derive maximal score and the action that guarantees it
maxScore ::
  (Ord a, Enum a, Hashable a) =>
  Observation a ->
  HashMap ((Observation a), a) Double ->
  [a] ->
  (Double, a)
maxScore obs table support = maximum [(value,action)| (value,(_,action)) <-valuesAndIndices]
  where
    indices = (obs, ) <$> support
    valuesAndIndices =  (\i -> (fromJust (HM.lookup i table ), i)) <$> indices

-- better prepare output to be used
extractFst :: Maybe (a,b) -> Maybe a
extractFst Nothing      = Nothing
extractFst (Just (a,b)) = Just a

extractSnd :: Maybe (a,b) -> Maybe b
extractSnd Nothing      = Nothing
extractSnd (Just (a,b)) = Just b

-- Update randomG
updateRandomG :: State a -> Rand.StdGen -> State a
updateRandomG s r = env % randomGen .~ r  $  s

-- Update QTable
updateQTable :: State a -> QTable a  -> State a
updateQTable s q = env % qTable .~ q  $  s

-- Update Observation for each agent
updateObservationAgent ::  Observation a -> State a -> State a
updateObservationAgent obs s = env % obsAgent .~ obs $  s

-- Update iterator
updateIteration :: State a -> State a
updateIteration = env % iteration %~ (+ 1)

-- Update temperature
updateTemperature :: Temperature -> State a ->  State a
updateTemperature decreaseFactor = env % temperature %~ (* decreaseFactor)

-- Update explorRate
updateExploreRate :: ExploreRate -> State a -> State a
updateExploreRate decreaseFactor = env % exploreRate %~ (* decreaseFactor)

-- Update gen, qtable
updateRandomGAndQTable :: State a -> Rand.StdGen -> QTable a  -> State a
updateRandomGAndQTable s r q = updateRandomG  (updateQTable s q) r

-- Update gen, qtable, temp
updateRandomGQTableTemp :: Temperature -> State a -> Rand.StdGen -> QTable a -> State a
updateRandomGQTableTemp decreaseFactor s r q = (updateTemperature decreaseFactor) $ updateRandomGAndQTable s r q

-- Update gen, qtable,exploreRate
updateRandomGQTableExplore :: ExploreRate -> State a -> Rand.StdGen -> QTable a -> State a
updateRandomGQTableExplore decreaseFactor s r q = (updateExploreRate decreaseFactor) $ updateRandomGAndQTable s r q

-- Update gen, qtable,exploreRate,agentObs
updateRandomGQTableExploreObs :: ExploreRate -> Observation a -> State a -> Rand.StdGen -> QTable a -> State a
updateRandomGQTableExploreObs decreaseFactor obs s r q = (updateObservationAgent obs) $ updateRandomGQTableExplore decreaseFactor s r q

-- Update gen, qtable,exploreRate,agentObs, iteration
updateRandomGQTableExploreObsIteration :: ExploreRate -> Observation a -> State a -> Rand.StdGen -> QTable a -> State a
updateRandomGQTableExploreObsIteration decreaseFactor obs s r q = updateIteration $ updateRandomGQTableExploreObs decreaseFactor obs s r q

-----------------------------------
-- 2 Implementation based on StateT
-- TODO simplify this analysis; redundancy between choosing and not

-- 2.1. e-greedy experimentation
-- | Choose optimally given qmatrix; do not explore. This is for the play part
chooseActionNoExplore :: (Monad m, Ord a, Enum a, Rand.Random a, Hashable a) =>
  [a] -> State a -> ST.StateT (State a) m a
chooseActionNoExplore support s = do
  let (exploreR, gen') = Rand.randomR (0.0 :: Double, 1.0 :: Double) (_randomGen $ _env s)
      optimalAction = snd $  maxScore (_obs s) (_qTable $ _env s) support
  return optimalAction


-- | Choose the optimal action given the current state or explore greedily
chooseExploreAction :: (Monad m, Enum a, Rand.Random a, Hashable a, Ord a) =>
  [a] -> State a -> ST.StateT (State a) m a
chooseExploreAction support s = do
  let (exploreR, gen') = Rand.randomR (0.0, 1.0) (_randomGen $ _env s)
  if exploreR < _exploreRate (_env s)
    then do
      let  (index,gen'')       = Rand.randomR (0, (length support - 1)) gen'
           action'            = support !! index
      return action'
    else do
      let optimalAction = snd $  maxScore (_obs s) (_qTable $ _env s) support
      return optimalAction

-- | Explore until temperature is below exgogenous threshold; with each round the threshold gets reduced
chooseExploreActionDecrTemp :: (Monad m, Enum a, Rand.Random a, Hashable a, Ord a) =>
  Temperature -> [a] -> State a -> ST.StateT (State a) m a
chooseExploreActionDecrTemp tempThreshold support  s = do
    let temp           = _temperature $ _env s
        (_, gen')      = Rand.randomR (0.0 :: Double, 1.0 :: Double) (_randomGen $ _env s)
        q              = _qTable $ _env s
        chooseExplore  =
          do
            let (index,gen'') = Rand.randomR (0, (length support - 1)) gen'
                action'       = support !! index
            return action'
        chooseNoExplore =
            do
              let optimalAction = snd $  maxScore (_obs s) (_qTable $ _env s) support
              return optimalAction
        in if temp < tempThreshold
           then chooseNoExplore
           else chooseExplore



-- 2.2. Different updates of the qmatrix depending on learning form
-- | Given an action, state, obs and a reward, update the qmatrix
-- | TODO Constant exploration rate
updateQTableST ::  (Monad m, Enum a, Rand.Random a, Hashable a, Ord a) =>
                     LearningRate ->  DiscountFactor ->   [a] -> State a -> Observation a -> a -> Double ->  ST.StateT (State a) m a
updateQTableST learningRate gamma support s obs2 action reward  = do
        let (_exp, gen')  = Rand.randomR (0.0 :: Double, 1.0 :: Double) (_randomGen $ _env s)
            q             = _qTable $ _env s
            prediction    = fromJust (HM.lookup (_obs s, action) q)
            updatedValue  = reward + gamma * (fst $ maxScore obs2 q support)
            newValue      = (1 - learningRate) * prediction + learningRate * updatedValue
            newQ          = HM.insert (_obs s, action) newValue q
        ST.put $ updateRandomGAndQTable s gen' newQ
        return action


-- | Update the qmatrix with evolving exploration rate
chooseLearnDecrExploreQTable ::  (Monad m, Enum a, Rand.Random a, Hashable a, Ord a) =>
                     LearningRate ->  DiscountFactor ->  ExploreRate -> [a] -> State a -> Observation a -> a -> Double ->  ST.StateT (State a) m a
chooseLearnDecrExploreQTable learningRate gamma decreaseFactorExplore support s obs2 action reward  = do
       let  (_,gen')     = Rand.randomR (0.0 :: Double, 1.0 :: Double) (_randomGen $ _env s)
            q            = _qTable $ _env s
            prediction   = fromJust (HM.lookup (_obs s, action) q)
            updatedValue = reward + gamma * (fst $ maxScore obs2 q support)
            newValue     = (1 - learningRate) * prediction + learningRate * updatedValue
            newQ         = HM.insert (_obs s, action) newValue q
       ST.put $  updateRandomGQTableExploreObsIteration decreaseFactorExplore obs2 s gen' newQ
       return action


-----------------
-- TODO the assumption on Comonad, Monad structure; works for Identity; should we further simplify this?
pureDecisionQStage :: Monad m =>
                      [a]
                      -> Agent
                      -> ([a] -> State a -> ST.StateT (State a) m a)
                      -> ([a] -> State a -> Observation a -> a -> Double -> ST.StateT (State a) m a)
                      -> QLearningStageGame m '[m (a,Env a)] '[m (a,Env a)] (Observation a) () a (Double,(Observation a))
pureDecisionQStage actionSpace name chooseAction updateQTable = OpenGame {
  play =  \(strat ::- Nil) -> let  v obs = do
                                           (_,env') <- strat
                                           let s obs = State env' obs
                                           action   <- ST.evalStateT  (chooseAction actionSpace (s obs)) (s obs)
                                           pure ((),action)
                                        in MonadOptic v (\_ -> (\_ -> pure ())),
  -- ^ This evaluates the statemonad with the monadic input of the external state and delivers a monadic action
  evaluate = \(strat ::- Nil) (MonadContext h k) ->
              let
                output = do
                   (_,pdenv') <- strat
                   (z,obs) <- h
                   -- ^ Take the (old observation) from the context
                   action <- ST.evalStateT  (chooseAction actionSpace (State pdenv' obs)) (State pdenv' obs)
                   (reward,obsNew) <- k z action
                   (State env' _) <- ST.execStateT (updateQTable actionSpace (State pdenv' obs) obsNew  action reward)
                                                   (State pdenv' obs)
                   return (action,env')
                in (output ::- Nil)}



deterministicStratStage ::  Monad m =>  Agent -> (Observation a -> a) -> QLearningStageGame m '[m a] '[m a] (Observation a) () a  (Double,Observation a)
deterministicStratStage name policy = OpenGame {
  play =  \(_ ::- Nil) -> let v obs = pure $ ((),policy obs)
                              in MonadOptic v (\_ -> (\_ -> pure ())),
  evaluate = \(a ::- Nil) (MonadContext h k) ->
              let
                output = do
                   (_,obs) <- h
                   -- ^ Take the (old observation) from the context
                   pure $ policy obs
                in (output ::- Nil)}

--------------------------
-- 4 Further functionality

fromLens :: Monad m => (x -> y) -> (x -> r -> s) -> QLearningStageGame m '[] '[] x s y r
fromLens v u = OpenGame {
  play = \Nil -> MonadOptic (\x -> pure (x, v x)) (\x -> (\r -> pure $ u x r)),
  evaluate = \Nil _ -> Nil}


fromFunctions :: Monad m => (x -> y) -> (r -> s) -> QLearningStageGame m '[] '[] x s y r
fromFunctions f g = fromLens f (const g)
