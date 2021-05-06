{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE KindSignatures #-}
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

module Engine.QLearning where

import           Control.DeepSeq
import           Control.Monad.Trans
import           Control.Monad.Trans.Reader
import           Data.STRef
import qualified Data.Vector.Sized as V
import           Engine.OpenGames hiding (lift)
import           Engine.OpticClass
import           Engine.TLL
import           GHC.Generics
import           GHC.TypeNats

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

type QTable n o a = A.Array (V.Vector n (o a), a) Double

-- Complete state comprises the internal state of the agent -- mainly the qmatrix
-- and the external state, the observation made in the last round
-- TODO needs to be generalized

data State o a = State
  { _env :: Env o a
  , _obs :: o a
  }


data Env o a = forall n. KnownNat n => Env
  { _name   :: String
  , _qTable :: QTable n o a
  , _iteration  :: Int
  , _exploreRate :: ExploreRate
  , _randomGen :: Rand.StdGen
  , _obsAgent :: o a
  , _temperature :: Temperature
  }
-- ^ Added here the agent observation the idea is that global and local information might diverge
-- instance (NFData a, NFData (o a)) => NFData (Env o a)


type QLearningStageGame m a b x s y r = OpenGame (MonadOptic m) (MonadContext m) a b x s y r

makeLenses ''Env
makeLenses ''State


------------------------
-- 1 Auxiliary functions
-- and updating functions
-- given a q-table, derive maximal score and the action that guarantees it
maxScore ::
  (Ord a, Enum a, A.Ix (o a), A.Ix a, KnownNat n) =>
  o a ->
  QTable n o a ->
  [a] ->
  (Double, a)
maxScore obs table support = maximum [(value,action)| (value,(_,action)) <-valuesAndIndices]
  where
    indices = (V.replicate obs, ) <$> support
    valuesAndIndices =  map (\i -> ((table A.! i), i)) indices

-- better prepare output to be used
extractFst :: Maybe (a,b) -> Maybe a
extractFst Nothing      = Nothing
extractFst (Just (a,b)) = Just a

extractSnd :: Maybe (a,b) -> Maybe b
extractSnd Nothing      = Nothing
extractSnd (Just (a,b)) = Just b

-- Update randomG
updateRandomG :: State o a -> Rand.StdGen -> State o a
updateRandomG s r = env % randomGen .~ r  $  s

-- Update QTable
updateQTable :: State o a -> QTable n o a  -> State o a
updateQTable s q = undefined {-env % qTable .~ q  $  s-}

-- Update Observation for each agent
updateObservationAgent ::  o a -> State o a -> State o a
updateObservationAgent obs s = env % obsAgent .~ obs $  s

-- Update iterator
updateIteration :: State o a -> State o a
updateIteration = env % iteration %~ (+ 1)

-- Update temperature
updateTemperature :: Temperature -> State o a ->  State o a
updateTemperature decreaseFactor = env % temperature %~ (* decreaseFactor)

-- Update explorRate
updateExploreRate :: ExploreRate -> State o a -> State o a
updateExploreRate decreaseFactor = env % exploreRate %~ (* decreaseFactor)

-- Update gen, qtable
updateRandomGAndQTable :: State o a -> Rand.StdGen -> QTable n o a  -> State o a
updateRandomGAndQTable s r q = updateRandomG  (updateQTable s q) r

-- Update gen, qtable, temp
updateRandomGQTableTemp :: Temperature -> State o a -> Rand.StdGen -> QTable n o a -> State o a
updateRandomGQTableTemp decreaseFactor s r q = (updateTemperature decreaseFactor) $ updateRandomGAndQTable s r q

-- Update gen, qtable,exploreRate
updateRandomGQTableExplore :: ExploreRate -> State o a -> Rand.StdGen -> QTable n o a -> State o a
updateRandomGQTableExplore decreaseFactor s r q = (updateExploreRate decreaseFactor) $ updateRandomGAndQTable s r q

-- Update gen, qtable,exploreRate,agentObs
updateRandomGQTableExploreObs :: ExploreRate -> o a -> State o a -> Rand.StdGen -> QTable n o a -> State o a
updateRandomGQTableExploreObs decreaseFactor obs s r q = (updateObservationAgent obs) $ updateRandomGQTableExplore decreaseFactor s r q

-- Update gen, qtable,exploreRate,agentObs, iteration
updateRandomGQTableExploreObsIteration :: ExploreRate -> o a -> State o a -> Rand.StdGen -> QTable n o a -> State o a
updateRandomGQTableExploreObsIteration decreaseFactor obs s r q = updateIteration $ updateRandomGQTableExploreObs decreaseFactor obs s r q

-----------------------------------
-- 2 Implementation based on StateT
-- TODO simplify this analysis; redundancy between choosing and not

-- 2.1. e-greedy experimentation
-- | Choose optimally given qmatrix; do not explore. This is for the play part
chooseActionNoExplore :: (Monad m, Enum a, Rand.Random a, A.Ix (o a), A.Ix a) =>
  [a] -> State o a -> ST.StateT (State o a) m a
chooseActionNoExplore support s = undefined -- do
  -- let (exploreR, gen') = Rand.randomR (0.0 :: Double, 1.0 :: Double) (_randomGen $ _env s)
  --     optimalAction = snd $  maxScore (_obs s) (_qTable $ _env s) support
  -- return optimalAction


-- | Choose the optimal action given the current state or explore greedily
chooseExploreAction :: (Monad m, Enum a, Rand.Random a, A.Ix (o a), A.Ix a) =>
  [a] -> State o a -> ST.StateT (State o a) m a
chooseExploreAction support s = do
  let (exploreR, gen') = Rand.randomR (0.0, 1.0) (_randomGen $ _env s)
  if exploreR < _exploreRate (_env s)
    then do
      let  (index,gen'')       = Rand.randomR (0, (length support - 1)) gen'
           action'            = support !! index
      return action'
    else do
      case _env s of
        Env{_qTable} -> do
         let optimalAction = snd $  maxScore (_obs s) _qTable support
         return optimalAction

-- | Explore until temperature is below exgogenous threshold; with each round the threshold gets reduced
chooseExploreActionDecrTemp :: (Monad m, Enum a, Rand.Random a, A.Ix (o a), A.Ix a) =>
  Temperature -> [a] -> State o a -> ST.StateT (State o a) m a
chooseExploreActionDecrTemp tempThreshold support  s = undefined {-do
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
           else chooseExplore-}



-- 2.2. Different updates of the qmatrix depending on learning form
-- | Given an action, state, obs and a reward, update the qmatrix
-- | TODO Constant exploration rate
updateQTableST ::  (Monad m, Enum a, Rand.Random a, A.Ix (o a), A.Ix a) =>
                     LearningRate ->  DiscountFactor ->   [a] -> State o a -> o a -> a -> Double ->  ST.StateT (State o a) m a
updateQTableST learningRate gamma support s obs2 action reward  = undefined {-do
        let (_exp, gen')  = Rand.randomR (0.0 :: Double, 1.0 :: Double) (_randomGen $ _env s)
            q             = _qTable $ _env s
            prediction    = q A.! (_obs s, action)
            updatedValue  = reward + gamma * (fst $ maxScore obs2 q support)
            newValue      = (1 - learningRate) * prediction + learningRate * updatedValue
            newQ          = q A.// [((_obs s, action), newValue)]
        ST.put $ updateRandomGAndQTable s gen' newQ
        return action-}


-- | Update the qmatrix with evolving exploration rate
chooseLearnDecrExploreQTable ::  (Monad m, Enum a, Rand.Random a, A.Ix (o a), A.Ix a) =>
                     LearningRate ->  DiscountFactor ->  ExploreRate -> [a] -> State o a -> o a -> a -> Double ->  ST.StateT (State o a) m a
chooseLearnDecrExploreQTable learningRate gamma decreaseFactorExplore support s obs2 action reward  = do
    case _env s of
      Env {_qTable=q} -> do
       let  (_,gen')     = Rand.randomR (0.0 :: Double, 1.0 :: Double) (_randomGen $ _env s)
            prediction   = q A.! (V.replicate (_obs s), action)
            updatedValue = reward + gamma * (fst $ maxScore obs2 q support)
            newValue     = (1 - learningRate) * prediction + learningRate * updatedValue
            newQ         = q A.// [((V.replicate (_obs s), action), newValue)]
       ST.put $  updateRandomGQTableExploreObsIteration decreaseFactorExplore obs2 s gen' newQ
       return action


-----------------
-- TODO the assumption on Comonad, Monad structure; works for Identity; should we further simplify this?
pureDecisionQStage :: Monad m =>
                      [a]
                      -> Agent
                      -> ([a] -> State o a -> ST.StateT (State o a) m a)
                      -> ([a] -> State o a -> o a -> a -> Double -> ST.StateT (State o a) m a)
                      -> QLearningStageGame m '[m (a,Env o a)] '[m (a,Env o a)] (o a) () a (Double,(o a))
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



deterministicStratStage ::  Monad m =>  Agent -> (o a -> a) -> QLearningStageGame m '[m a] '[m a] (o a) () a  (Double,o a)
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
