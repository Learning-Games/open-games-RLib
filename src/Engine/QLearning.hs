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


import           Optics.TH  (makeLenses)
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

data Action = Cooperate | Defect
  deriving (Eq,Ord,Enum,Show,A.Ix)

type Observation = (Action,Action)
type QTable = A.Array (Observation, Action) Double

type QTable2 a = A.Array (Observation2 a, a) Double
type Observation2 a = (a,a)

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
  , _temperature :: Temperature
  }  deriving (Show)



data State2 a = State2
  { _env2 :: Env2 a
  , _obs2 :: Observation2 a
  }  deriving (Show)




data Env2 a = Env2
  { _qTable2 :: QTable2 a
  , _exploreRate2 :: Double
  , _randomGen2 :: Rand.StdGen
  , _temperature2 :: Temperature
  }  deriving (Show)



type QLearningStageGame m a b x s y r = OpenGame (MonadicLearnLens m) (MonadicLearnLensContext m) a b x s y r

makeLenses ''PDEnv
makeLenses ''State

makeLenses ''Env2
makeLenses ''State2

-- fixing outside parameters
gamma = 0.7
learningRate = 0.40

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

maxScore2 ::
  (Ord a, Enum a, A.Ix a) =>
  Observation2 a ->
  A.Array ((Observation2 a), a) Double ->
  (a,a,a) ->
  (Double, a)
maxScore2 obs table (low,diff,high) = maximum [(value,action)| (value,(_,action)) <-valuesAndIndices]
  where
    indices = (obs, ) <$> [low,diff..high]
    valuesAndIndices =  (\i -> (table A.! i, i)) <$> indices


-- Update randomG
updateRandomG :: State -> Rand.StdGen -> State
updateRandomG s r = env % randomGen .~ r  $  s

updateRandomG2 :: State2 a -> Rand.StdGen -> State2 a
updateRandomG2 s r = env2 % randomGen2 .~ r  $  s


-- Update QTable
updateQTable :: State -> QTable -> State
updateQTable s q = env % qTable .~ q  $  s

updateQTable2 :: State2 a -> QTable2 a  -> State2 a
updateQTable2 s q = env2 % qTable2 .~ q  $  s



-- Update Observation
updateObservation :: State -> Observation -> State
updateObservation s o = obs .~ o $  s

updateObservation2 :: State2 a  -> Observation2 a -> State2 a 
updateObservation2 s o = obs2 .~ o $  s



-- Update temperature
updateTemperature :: State ->  State
updateTemperature = env % temperature %~ (* 0.999)

updateTemperature2 :: State2 a ->  State2 a
updateTemperature2 = env2 % temperature2 %~ (* 0.999)



-- Update State
updateRandomGAndQTable :: State -> Rand.StdGen -> QTable -> State
updateRandomGAndQTable s r q = updateRandomG  (updateQTable s q) r 

updateRandomGAndQTable2 :: State2 a -> Rand.StdGen -> QTable2 a  -> State2 a
updateRandomGAndQTable2 s r q = updateRandomG2  (updateQTable2 s q) r 


-- Update state including temperature
updateRandomGQTableTemp :: State -> Rand.StdGen -> QTable -> State
updateRandomGQTableTemp s r q = updateTemperature $ updateRandomGAndQTable s r q

updateRandomGQTableTemp2 :: State2 a -> Rand.StdGen -> QTable2 a -> State2 a
updateRandomGQTableTemp2 s r q = updateTemperature2 $ updateRandomGAndQTable2 s r q



-- better prepare output to be used
extractFst :: Maybe (a,b) -> Maybe a
extractFst Nothing      = Nothing
extractFst (Just (a,b)) = Just a

extractSnd :: Maybe (a,b) -> Maybe b
extractSnd Nothing      = Nothing
extractSnd (Just (a,b)) = Just b



-----------------------------------
-- 2 Implementation based on StateT
-- TODO simplify this analysis; redundancy between choosing and not

-- 2.1. e-greedy experimentation
-- Choose the optimal action given the current state or explore greedily
chooseActionQTable :: Monad m =>
  State -> ST.StateT State m Action
chooseActionQTable s = do
  let (exploreR, gen') = Rand.randomR (0.0, 1.0) (_randomGen $ _env s)
  if exploreR < _exploreRate (_env s)
    then do
      let (actionP, gen'') = Rand.randomR (0.0 :: Double, 1.0 :: Double) gen'
          action'          = if actionP < 0.5 then Cooperate else Defect
      ST.put $ updateRandomG s gen'' 
      return action'
    else do
      let optimalAction = snd $  maxScore (_obs s) (_qTable $ _env s)
      ST.put $  updateRandomG s gen'
      return optimalAction

chooseActionQTable2 :: (Monad m, Enum a, Rand.Random a, A.Ix a) =>
  (a,a,a) -> State2 a -> ST.StateT (State2 a) m a
chooseActionQTable2 (low,diff,high) s = do
  let (exploreR, gen') = Rand.randomR (0.0, 1.0) (_randomGen2 $ _env2 s)
  if exploreR < _exploreRate2 (_env2 s)
    then do
      let (action', gen'') = Rand.randomR (low,high) gen'
      ST.put $ updateRandomG2 s gen'' 
      return action'
    else do
      let optimalAction = snd $  maxScore2 (_obs2 s) (_qTable2 $ _env2 s) (low,diff,high) 
      ST.put $  updateRandomG2 s gen'
      return optimalAction



-- choose optimally or explore greedily
chooseLearnQTable ::  Monad m => State -> Observation -> Double -> ST.StateT State m Action
chooseLearnQTable s obs2 reward  = do
    let (exploreR, gen') = Rand.randomR (0.0, 1.0) (_randomGen $ _env s)
    if exploreR < _exploreRate (_env s)
      then do
        let (actionP, gen'') = Rand.randomR (0.0 :: Double, 1.0 :: Double) gen'
            action'          = if actionP < 0.5 then Cooperate else Defect
            q                = _qTable $ _env s
            prediction       = q A.! (_obs s, action')
            updatedValue     = reward + gamma * (fst $ maxScore obs2 q)
            newValue         = (1 - learningRate) * prediction + learningRate * updatedValue
            newQ             = q A.// [((_obs s, action'), newValue)]
        ST.put $ updateRandomGAndQTable s gen'' newQ
        return action'
     else do
        let optimalAction = snd $  maxScore (_obs s) (_qTable $ _env s)
            q             = _qTable $ _env s
            prediction    = q A.! (_obs s, optimalAction)
            updatedValue  = reward + gamma * (fst $ maxScore obs2 q)
            newValue      = (1 - learningRate) * prediction + learningRate * updatedValue
            newQ          = q A.// [((_obs s, optimalAction), newValue)]
        ST.put $ updateRandomGAndQTable s gen' newQ
        return optimalAction

-- choose optimally or explore greedily
chooseLearnQTable2 ::  (Monad m, Enum a, Rand.Random a, A.Ix a) =>
                       (a,a,a) -> State2 a -> Observation2 a -> Double ->  ST.StateT (State2 a) m a
chooseLearnQTable2 (low,diff,high) s obs2 reward  = do
    let (exploreR, gen') = Rand.randomR (0.0, 1.0) (_randomGen2 $ _env2 s)
    if exploreR < _exploreRate2 (_env2 s)
      then do
        let (action', gen'') = Rand.randomR (low,high) gen'
            q                = _qTable2 $ _env2 s
            prediction       = q A.! (_obs2 s, action')
            updatedValue     = reward + gamma * (fst $ maxScore2 obs2 q (low,diff,high))
            newValue         = (1 - learningRate) * prediction + learningRate * updatedValue
            newQ             = q A.// [((_obs2 s, action'), newValue)]
        ST.put $ updateRandomGAndQTable2 s gen'' newQ
        return action'
     else do
        let optimalAction = snd $  maxScore2 (_obs2 s) (_qTable2 $ _env2 s) (low,diff,high)
            q             = _qTable2 $ _env2 s 
            prediction    = q A.! (_obs2 s, optimalAction)
            updatedValue  = reward + gamma * (fst $ maxScore2 obs2 q (low,diff,high))
            newValue      = (1 - learningRate) * prediction + learningRate * updatedValue
            newQ          = q A.// [((_obs2 s, optimalAction), newValue)]
        ST.put $ updateRandomGAndQTable2 s gen' newQ
        return optimalAction



-- 2.2. Boltzmann prob updating
-- Choose the optimal action given the current state 
chooseBoltzQTable :: Monad m =>
  State -> ST.StateT State m Action
chooseBoltzQTable s = do
      let (_, gen')     = Rand.randomR (0.0 :: Double, 1.0 :: Double) (_randomGen $ _env s)
          optimalAction = snd $  maxScore (_obs s) (_qTable $ _env s)
      ST.put $  updateRandomG s gen'
      return optimalAction

-- choose optimally or explore according to the Boltzmann rule
chooseUpdateBoltzQTable ::  Monad m => State -> Observation -> Double -> ST.StateT State m Action
chooseUpdateBoltzQTable s obs2 reward  =
    let temp      = _temperature $ _env s
        (_, gen') = Rand.randomR (0.0 :: Double, 1.0 :: Double) (_randomGen $ _env s)
        q         = _qTable $ _env s
        chooseNoExplore =
            do
              let (_, gen'')   = Rand.randomR (0.0 :: Double, 1.0 :: Double) gen'
                  action'      = snd $  maxScore (_obs s) (_qTable $ _env s)
                  prediction   = q A.! (_obs s, action')
                  updatedValue = reward + gamma * (fst $ maxScore obs2 q)
                  newValue     = (1 - learningRate) * prediction + learningRate * updatedValue
                  newQ         = q A.// [((_obs s, action'), newValue)]
              ST.put $ updateRandomGQTableTemp s gen'' newQ
              return action'
        chooseExplore  =
          do
            let qCooperate       = q A.! (_obs s, Cooperate)
                qDefect          = q A.! (_obs s, Defect)
                eCooperate       = (exp 1.0)** qCooperate / temp
                eDefect          = (exp 1.0)** qDefect / temp
                probCooperate    = eCooperate / (eCooperate + eDefect)
                (actionP, gen'') = Rand.randomR (0.0 :: Double, 1.0 :: Double) gen'
                action'          = if actionP < probCooperate then Cooperate else Defect
                prediction       = q A.! (_obs s, action')
                updatedValue     = reward + gamma * (fst $ maxScore obs2 q)
                newValue         = (1 - learningRate) * prediction + learningRate * updatedValue
                newQ             = q A.// [((_obs s, action'), newValue)]
            ST.put $ updateRandomGQTableTemp s gen'' newQ
            return action'
        in if temp < 0.01
           then chooseNoExplore
           else chooseExplore


-- Policy for deterministic player
titForTat :: Observation -> Action
titForTat (_,Cooperate) = Cooperate
titForTat (_,Defect)    = Defect




-----------------
-- TODO the assumption on Comonad, Monad structure; works for Identity; should we further simplify this?
pureDecisionQStage :: (Comonad m, Monad m) =>
                      Agent
                      -> (State -> ST.StateT State m Action)
                      -> (State -> Observation -> Double -> ST.StateT State m Action)
                      -> QLearningStageGame m '[m (Action,PDEnv)] '[m (Action,PDEnv)] Observation () Action  (Double,Observation)
pureDecisionQStage name chooseAction updateQTable = OpenGame {
  play =  \(strat ::- Nil) -> let (_,pdenv') = extract strat
                                  v obs =
                                    let s obs = State pdenv' obs
                                        in ST.evalStateT  (chooseAction (s obs)) (s obs)
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
                   action <- ST.evalStateT  (chooseAction (State pdenv' obs)) (State pdenv' obs)
                   (reward,obsNew) <- k' action 
                   (State env' _) <- ST.execStateT (updateQTable (State pdenv' obs) obsNew reward)
                                                   (State pdenv' obs)
                   return (action,env')
                in (output ::- Nil)}

-- TODO the assumption on Comonad, Monad structure; works for Identity; should we further simplify this?
pureDecisionQStage2 :: (Comonad m, Monad m) =>
                      Agent
                      -> (State2 a -> ST.StateT (State2 a) m a)
                      -> (State2 a -> Observation2 a -> Double -> ST.StateT (State2 a) m a)
                      -> QLearningStageGame m '[m (a,Env2 a)] '[m (a,Env2 a)] (Observation2 a) () a (Double,(Observation2 a))
pureDecisionQStage2 name chooseAction updateQTable = OpenGame {
  play =  \(strat ::- Nil) -> let (_,env') = extract strat
                                  v obs =
                                    let s obs = State2 env' obs
                                        in ST.evalStateT  (chooseAction (s obs)) (s obs)
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
                   action <- ST.evalStateT  (chooseAction (State2 pdenv' obs)) (State2 pdenv' obs)
                   (reward,obsNew) <- k' action 
                   (State2 env' _) <- ST.execStateT (updateQTable (State2 pdenv' obs) obsNew reward)
                                                   (State2 pdenv' obs)
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


deterministicStratStage2 :: (Comonad m, Monad m) =>  Agent -> (Observation2 a -> a) -> QLearningStageGame m '[m a] '[m a] (Observation2 a) () a  (Double,Observation2 a)
deterministicStratStage2 name policy = OpenGame {
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





