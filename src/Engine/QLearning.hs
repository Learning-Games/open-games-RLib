{-# LANGUAGE TypeOperators, ScopedTypeVariables, TupleSections, DataKinds, GADTs, FlexibleInstances, FlexibleContexts, TemplateHaskell, MultiParamTypeClasses, UndecidableInstances, TypeApplications#-}


module Engine.QLearning where

import Engine.OpenGames hiding (lift)
import Engine.OpticClass
import Engine.TLL

import           Control.Comonad
import           Control.Monad.State.Class
import qualified Control.Monad.Trans.State as ST
import qualified Data.Random   as R
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

type ExploreRate = Double

type LearningRate = Double

type DiscountFactor = Double

type QTable a = A.Array (Observation a, a) Double


type Observation a = (a,a)

-- Complete state comprises the internal state of the agent -- mainly the qmatrix
-- and the external state, the observation made in the last round
-- TODO needs to be generalized

data State a = State
  { _env :: Env a
  , _obs :: Observation a
  }  deriving (Show)




data Env a = Env
  { _qTable :: QTable a
  , _exploreRate :: ExploreRate
  , _randomGen :: Rand.StdGen
  , _temperature :: Temperature
  }  deriving (Show)



type QLearningStageGame m a b x s y r = OpenGame (MonadicLearnLens m) (MonadicLearnLensContext m) a b x s y r

makeLenses ''Env
makeLenses ''State


------------------------
-- 1 Auxiliary functions
-- and updating functions
-- given a q-table, derive maximal score and the action that guarantees it
maxScore ::
  (Ord a, Enum a, A.Ix a) =>
  Observation a ->
  A.Array ((Observation a), a) Double ->
  [a] ->
  (Double, a)
maxScore obs table support = maximum [(value,action)| (value,(_,action)) <-valuesAndIndices]
  where
    indices = (obs, ) <$> support
    valuesAndIndices =  (\i -> (table A.! i, i)) <$> indices

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

-- Update Observation
updateObservation :: State a  -> Observation a -> State a
updateObservation s o = obs .~ o $  s

-- Update temperature
updateTemperature :: Temperature -> State a ->  State a
updateTemperature decreaseFactor = env % temperature %~ (* decreaseFactor)

-- Update explorRate
updateExploreRate :: ExploreRate -> State a -> State a
updateExploreRate decreaseFactor = env % exploreRate %~ (* decreaseFactor) 

-- Update State
updateRandomGAndQTable :: State a -> Rand.StdGen -> QTable a  -> State a
updateRandomGAndQTable s r q = updateRandomG  (updateQTable s q) r

-- Update state including temperature
updateRandomGQTableTemp :: Temperature -> State a -> Rand.StdGen -> QTable a -> State a
updateRandomGQTableTemp decreaseFactor s r q = (updateTemperature decreaseFactor) $ updateRandomGAndQTable s r q

-- Update state including exploreRate
updateRandomGQTableExplore :: ExploreRate -> State a -> Rand.StdGen -> QTable a -> State a
updateRandomGQTableExplore decreaseFactor s r q = (updateExploreRate decreaseFactor) $ updateRandomGAndQTable s r q



-----------------------------------
-- 2 Implementation based on StateT
-- TODO simplify this analysis; redundancy between choosing and not

-- 2.1. e-greedy experimentation
-- Choose the optimal action given the current state or explore greedily
chooseActionQTable :: (Monad m, Enum a, Rand.Random a, A.Ix a) =>
  [a] -> State a -> ST.StateT (State a) m a
chooseActionQTable support s = do
  let (exploreR, gen') = Rand.randomR (0.0, 1.0) (_randomGen $ _env s)
  if exploreR < _exploreRate (_env s)
    then do
      let  (index,gen'')       = Rand.randomR (0, (length support - 1)) gen'
           action'            = support !! index
      ST.put $ updateRandomG s gen''
      return action'
    else do
      let optimalAction = snd $  maxScore (_obs s) (_qTable $ _env s) support
      ST.put $  updateRandomG s gen'
      return optimalAction

-- choose optimally or explore greedily with fixed exploreRate
chooseLearnQTable ::  (Monad m, Enum a, Rand.Random a, A.Ix a) =>
                     LearningRate ->  DiscountFactor -> [a] -> State a -> Observation a -> Double ->  ST.StateT (State a) m a
chooseLearnQTable learningRate gamma support s obs2 reward  = do
    let (exploreR, gen') = Rand.randomR (0.0, 1.0) (_randomGen $ _env s)
    if exploreR < _exploreRate (_env s)
      -- ^ if True, then explore, else just choose optimally given qmatrix
      then do
        let (index,gen'')    = Rand.randomR (0, (length support - 1)) gen'
            action'          = support !! index
            q                = _qTable $ _env s
            prediction       = q A.! (_obs s, action')
            updatedValue     = reward + gamma * (fst $ maxScore obs2 q support)
            newValue         = (1 - learningRate) * prediction + learningRate * updatedValue
            newQ             = q A.// [((_obs s, action'), newValue)]
        ST.put $ updateRandomGAndQTable s gen'' newQ
        return action'
     else do
        let optimalAction = snd $  maxScore (_obs s) (_qTable $ _env s) support
            q             = _qTable $ _env s 
            prediction    = q A.! (_obs s, optimalAction)
            updatedValue  = reward + gamma * (fst $ maxScore obs2 q support)
            newValue      = (1 - learningRate) * prediction + learningRate * updatedValue
            newQ          = q A.// [((_obs s, optimalAction), newValue)]
        ST.put $ updateRandomGAndQTable s gen' newQ
        return optimalAction

-- choose optimally given qmatrix; do not explore
chooseActionNoExploreQTable :: (Monad m, Enum a, Rand.Random a, A.Ix a) =>
  [a] -> State a -> ST.StateT (State a) m a
chooseActionNoExploreQTable support s = do
  let (exploreR, gen') = Rand.randomR (0.0 :: Double, 1.0 :: Double) (_randomGen $ _env s)
      optimalAction = snd $  maxScore (_obs s) (_qTable $ _env s) support
  ST.put $  updateRandomG s gen'
  return optimalAction


-- explore until temperature is below exgogenous threshold; with each round the threshold gets reduced
chooseLearnDecrTempEQTable ::  (Monad m, Enum a, Rand.Random a, A.Ix a) =>
                      LearningRate ->  DiscountFactor -> Temperature -> Temperature -> [a] -> State a -> Observation a -> Double ->  ST.StateT (State a) m a
chooseLearnDecrTempEQTable  learningRate gamma tempThreshold decreaseFactor support s obs2 reward  =
    let temp      = _temperature $ _env s
        (_, gen') = Rand.randomR (0.0 :: Double, 1.0 :: Double) (_randomGen $ _env s)
        q         = _qTable $ _env s
        chooseExplore  =
          do
            let (index,gen'')    = Rand.randomR (0, (length support - 1)) gen'
                action'          = support !! index
                q                = _qTable $ _env s
                prediction       = q A.! (_obs s, action')
                updatedValue     = reward + gamma * (fst $ maxScore obs2 q support)
                newValue         = (1 - learningRate) * prediction + learningRate * updatedValue
                newQ             = q A.// [((_obs s, action'), newValue)]
            ST.put $ updateRandomGQTableTemp decreaseFactor s gen'' newQ
            return action'
        chooseNoExplore =
            do
              let optimalAction = snd $  maxScore (_obs s) (_qTable $ _env s) support
                  q             = _qTable $ _env s 
                  prediction    = q A.! (_obs s, optimalAction)
                  updatedValue  = reward + gamma * (fst $ maxScore obs2 q support)
                  newValue      = (1 - learningRate) * prediction + learningRate * updatedValue
                  newQ          = q A.// [((_obs s, optimalAction), newValue)]
              ST.put $ updateRandomGQTableTemp decreaseFactor s gen' newQ
              return optimalAction
                in if temp < tempThreshold
           then chooseNoExplore
           else chooseExplore


chooseLearnDecrExploreQTable ::  (Monad m, Enum a, Rand.Random a, A.Ix a) =>
                     LearningRate ->  DiscountFactor ->  ExploreRate -> [a] -> State a -> Observation a -> Double ->  ST.StateT (State a) m a
chooseLearnDecrExploreQTable learningRate gamma decreaseFactor support s obs2 reward  = do
    let (exploreR, gen') = Rand.randomR (0.0, 1.0) (_randomGen $ _env s)
    if exploreR < _exploreRate (_env s)
      -- ^ if True, then explore, else just choose optimally given qmatrix
      then do
        let (index,gen'')    = Rand.randomR (0, (length support - 1)) gen'
            action'          = support !! index
            q                = _qTable $ _env s
            prediction       = q A.! (_obs s, action')
            updatedValue     = reward + gamma * (fst $ maxScore obs2 q support)
            newValue         = (1 - learningRate) * prediction + learningRate * updatedValue
            newQ             = q A.// [((_obs s, action'), newValue)]
        ST.put $  updateRandomGQTableExplore decreaseFactor s gen'' newQ
        return action'
     else do
        let optimalAction = snd $  maxScore (_obs s) (_qTable $ _env s) support
            q             = _qTable $ _env s 
            prediction    = q A.! (_obs s, optimalAction)
            updatedValue  = reward + gamma * (fst $ maxScore obs2 q support)
            newValue      = (1 - learningRate) * prediction + learningRate * updatedValue
            newQ          = q A.// [((_obs s, optimalAction), newValue)]
        ST.put $ updateRandomGQTableExplore decreaseFactor s gen' newQ
        return optimalAction



-----------------
-- TODO the assumption on Comonad, Monad structure; works for Identity; should we further simplify this?
pureDecisionQStage :: (Comonad m, Monad m) =>
                      [a]
                      -> Agent
                      -> ([a] -> State a -> ST.StateT (State a) m a)
                      -> ([a]  -> State a -> Observation a -> Double -> ST.StateT (State a) m a)
                      -> QLearningStageGame m '[m (a,Env a)] '[m (a,Env a)] (Observation a) () a (Double,(Observation a))
pureDecisionQStage actionSpace name chooseAction updateQTable = OpenGame {
  play =  \(strat ::- Nil) -> let (_,env') = extract strat
                                  v obs =
                                    let s obs = State env' obs
                                        in ST.evalStateT  (chooseAction actionSpace (s obs)) (s obs)
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
                   action <- ST.evalStateT  (chooseAction actionSpace (State pdenv' obs)) (State pdenv' obs)
                   (reward,obsNew) <- k' action 
                   (State env' _) <- ST.execStateT (updateQTable actionSpace (State pdenv' obs) obsNew reward)
                                                   (State pdenv' obs)
                   return (action,env')
                in (output ::- Nil)}



deterministicStratStage :: (Comonad m, Monad m) =>  Agent -> (Observation a -> a) -> QLearningStageGame m '[m a] '[m a] (Observation a) () a  (Double,Observation a)
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





