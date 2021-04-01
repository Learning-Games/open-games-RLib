{-# LANGUAGE TypeOperators, ScopedTypeVariables, TupleSections, DataKinds, GADTs, FlexibleInstances, FlexibleContexts, TemplateHaskell, MultiParamTypeClasses, UndecidableInstances, TypeApplications#-}


module Engine.QLearning where

import Engine.OpenGames hiding (lift)
import Engine.OpticClass
import Engine.TLL

import Control.Comonad 
import Control.Monad.State.Class
import qualified Control.Monad.Trans.State as ST
import qualified System.Random as Rand
import qualified GHC.Arr as A


import Optics.TH  (makeLenses)
import qualified Optics.Lens as L
import qualified Optics.Setter as S
import           Optics.Operators

-- TODO check the random update
-- TODO Replace the array access functionality with a lens
-- NOTE The _x_ input needs to be manipulated in the outside game definition

------------------
-- 0 Preliminaries

type Agent = String

data Action = Cooperate | Defect
  deriving (Eq,Ord,Enum,Show,A.Ix)

type Observation = (Action,Action)
type QTable = A.Array (Observation, Action) Double

-- Complete state comprises the internal state of the agent -- mainly the qmatrix
-- and the external state, the observation made in the last round
-- TODO needs to be generalized
data State = State
  { _env :: PDEnv
  , _obs :: Observation
  }  deriving (Show)

data PDEnv = PDEnv
  { _qTable :: QTable
  , _exploreRate :: Double
  , _randomGen :: Rand.StdGen
  }  deriving (Show)


type QLearningStageGame m a b x s y r = OpenGame (MonadicLearnLens m) (MonadicLearnLensContext m) a b x s y r

makeLenses ''PDEnv
makeLenses ''State

-- fixing outside parameters
gamma = 0.96
learningRate = 0.81


------------------------
-- 1 Auxiliary functions
-- given a q-table, derive maximal score and the action that guarantees it
maxScore ::
  Observation ->
  A.Array (Observation, Action) Double ->
  (Double, Action)
maxScore obs table = maximum [(value,action)| (value,(_,action)) <-valuesAndIndices]
  where
    indices = (obs, ) <$> [Cooperate,Defect]
    valuesAndIndices =  (\i -> (table A.! i, i)) <$> indices

-- Update randomG
updateRandomG :: State -> Rand.StdGen -> State
updateRandomG s r = env .~ newRandom $  s
     where newRandom = randomGen .~ r $ _env s 


-- Update QTable
updateQTable :: State -> QTable -> State
updateQTable s q = env .~ newQTable $  s
     where newQTable = qTable .~ q $ _env s 

-- Update Observation
updateObservation :: State -> Observation -> State
updateObservation s o = obs .~ o $  s

-- Update State
updateRandomGAndQTable :: State -> Rand.StdGen -> QTable -> State
updateRandomGAndQTable s r q = updateRandomG  (updateQTable s q) r 

-- Policy for deterministic player
titForTat :: Observation -> Action
titForTat (_,Cooperate) = Cooperate
titForTat (_,Defect)    = Defect


-----------------------------------
-- 2 Implementation based on StateT
-- Choose the optimal action given the current state
chooseActionQTable :: Monad m =>
  State -> ST.StateT State m Action
chooseActionQTable s = do
  let (exploreR, gen') = Rand.randomR (0.0, 1.0) (_randomGen $ _env s)
  if exploreR < _exploreRate (_env s)
    then do
      let (actionP, gen'') = Rand.randomR (0.0 :: Double, 1.0 :: Double) gen'
      let  action' = if actionP < 0.5 then Cooperate else Defect
      ST.put $ updateRandomG s gen'' 
      return action'
    else do
      let optimalAction = snd $  maxScore (_obs s) (_qTable $ _env s)
      ST.put $  updateRandomG s gen'
      return optimalAction

-- choose optimally and update the q-matrix and output the new action
chooseLearnQTable ::  Monad m => State ->  Observation -> Observation -> Double -> Action -> ST.StateT State m Action
chooseLearnQTable s obs1 obs2 reward action = do
  optimalAction <- chooseActionQTable s
  let q = _qTable $ _env s
      prediction = q A.! (obs1, action)
      target = reward + gamma * (fst $ maxScore obs2 q)
      newValue = prediction + learningRate * (target - prediction)
      newQ = q A.// [((obs1, action), newValue)]
  ST.put $ updateQTable s newQ
  return optimalAction

chooseLearnQTable2 ::  Monad m => State -> Observation -> Double -> ST.StateT State m Action
chooseLearnQTable2 s obs2 reward  = do
    let (exploreR, gen') = Rand.randomR (0.0, 1.0) (_randomGen $ _env s)
    if exploreR < _exploreRate (_env s)
      then do
        let (actionP, gen'') = Rand.randomR (0.0 :: Double, 1.0 :: Double) gen'
        let  action' = if actionP < 0.5 then Cooperate else Defect
        let q = _qTable $ _env s
            prediction = q A.! (_obs s, action')
            target = reward + gamma * (fst $ maxScore obs2 q)
            newValue = prediction + learningRate * (target - prediction)
            newQ = q A.// [((_obs s, action'), newValue)]
        ST.put $ updateRandomGAndQTable s gen'' newQ 
        return action'
     else do
        let optimalAction = snd $  maxScore (_obs s) (_qTable $ _env s)
        let q = _qTable $ _env s
            prediction = q A.! (_obs s, optimalAction)
            target = reward + gamma * (fst $ maxScore obs2 q)
            newValue = prediction + learningRate * (target - prediction)
            newQ = q A.// [((_obs s, optimalAction), newValue)]
        ST.put $ updateRandomGAndQTable s gen' newQ
        return optimalAction





-----------------
-- The stage game -- only with external observation fed through
pureDecisionQStage :: (Monad m) =>  Agent -> QLearningStageGame m '[m (Action,PDEnv)] '[m (Action,PDEnv)] State () Action  Double
pureDecisionQStage name = OpenGame {
  play =  \(_ ::- Nil) -> let v s = ST.evalStateT  (chooseActionQTable s) s
                                    in MonadicLearnLens v (\_ ->pure  (\_ -> pure ())),
  -- ^ This evaluates the statemonad with the monadic input of the external state and delivers a monadic action
  evaluate = \(pdenv ::- Nil) (MonadicLearnLensContext h k) ->
             let
                output = do
                   (State _ obs) <- h
                   k' <- k
                   (_,pdenv') <- pdenv
                   action <- ST.evalStateT  (chooseActionQTable (State pdenv' obs)) (State pdenv' obs)
                   reward <- k' action 
                   (State env' _) <- ST.execStateT (chooseLearnQTable (State pdenv' obs) (obs) (obs) reward action)
                                                   (State pdenv' obs)
                   return (action,env')
                in (output ::- Nil)}


-- TODO the assumption on Comonad, Monad structure; works for Identity; should we further simplify this?
pureDecisionQStage' :: (Comonad m, Monad m) =>  Agent -> QLearningStageGame m '[m (Action,PDEnv)] '[m (Action,PDEnv)] Observation () Action  (Double,Observation)
pureDecisionQStage' name = OpenGame {
  play =  \(strat ::- Nil) -> let (_,pdenv') = extract strat
                                  v obs =
                                    let s obs = State pdenv' obs
                                        in ST.evalStateT  (chooseActionQTable (s obs)) (s obs)
                                        in MonadicLearnLens v (\_ ->pure  (\_ -> pure ())),
  -- ^ This evaluates the statemonad with the monadic input of the external state and delivers a monadic action
  evaluate = \(strat ::- Nil) (MonadicLearnLensContext h k) ->
              let
                output = do
                   obs <- h
                   -- ^ Take the (old observation) from the context
                   k' <- k
                   -- ^ continuation from the outside; :: Action -> (Double,Observation)
                   (_,pdenv') <- strat
                   action <- ST.evalStateT  (chooseActionQTable (State pdenv' obs)) (State pdenv' obs)
                   (reward,obsNew) <- k' action 
                   (State env' _) <- ST.execStateT (chooseLearnQTable2 (State pdenv' obs) obsNew reward)
                                                   (State pdenv' obs)
                   return (action,env')
                in (output ::- Nil)}

-- Follow a deterministic strategy
-- TODO change strategy? Here just to see the trace easily
-- TODO change return type? Here just to simplify symmetric treatment
-- TODO here just assumption of fixed action? 
deterministicStratStage :: (Comonad m, Monad m) =>  Agent -> (Observation -> Action) -> QLearningStageGame m '[m Action] '[m Action] Observation () Action  (Double,Observation)
deterministicStratStage name policy = OpenGame {
  play =  \(_ ::- Nil) -> let v obs = pure $ policy obs
                              in MonadicLearnLens v (\_ ->pure  (\_ -> pure ())),
  -- ^ This evaluates the statemonad with the monadic input of the external state and delivers a monadic action
  evaluate = \(a ::- Nil) (MonadicLearnLensContext h k) ->
              let
                output = do
                   obs <- h
                   -- ^ Take the (old observation) from the context
                   pure $ policy obs
                in (output ::- Nil)}





--------------------------
-- 4 Further functionality

fromLens :: Monad m => (x -> y) -> (x -> r -> s) -> QLearningStageGame m '[] '[] x s y r
fromLens v u = OpenGame {
  play = \Nil -> MonadicLearnLens (\x -> pure $ v x) (\x -> pure $ (\r -> pure $ u x r)),
  evaluate = \Nil _ -> Nil}


fromFunctions :: Monad m => (x -> y) -> (r -> s) -> QLearningStageGame m '[] '[] x s y r
fromFunctions f g = fromLens f (const g)





---------------------
-- 5. Not needed now

-- TODO another idea: feed information back inside as coutility? 
pureDecisionQStage2 :: (Monad m) =>  Agent -> QLearningStageGame m '[m (Action,PDEnv)] '[m (Action,PDEnv)] State Double Action Double
pureDecisionQStage2 name = OpenGame {
  play =  \(_ ::- Nil) -> let v s = ST.evalStateT  (chooseActionQTable s) s
                                    in MonadicLearnLens v (\_ ->pure  (\b -> pure $ gamma * b)),
  -- ^ This evaluates the statemonad with the monadic input of the external state and delivers a monadic action
  evaluate = \(pdenv ::- Nil) (MonadicLearnLensContext h k) ->
             let
                output = do
                   (State _ obs) <- h
                   k' <- k
                   (_,pdenv') <- pdenv
                   action <- ST.evalStateT  (chooseActionQTable (State pdenv' obs)) (State pdenv' obs)
                   reward <- k' action 
                   (State env' _) <- ST.execStateT (chooseLearnQTable (State pdenv' obs) (obs) (obs) reward action)
                                                   (State pdenv' obs)
                   return (action,env')
                in (output ::- Nil)}



