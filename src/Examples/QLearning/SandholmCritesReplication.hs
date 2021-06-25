{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
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

module Examples.QLearning.SandholmCritesReplication
                     ( evalStageLS
                     , initiateStrat
                     , sequenceL
                     , evalStageM
                     )
                     where

import           Control.Comonad
import           Control.Monad.IO.Class
import           Control.Monad.Reader as Lift
import qualified Control.Monad.Trans.State as ST
import qualified Data.Array.IO as A
import           Data.Coerce
import           Data.Function
import           Data.Functor.Identity
import qualified Data.Vector as V
import qualified Data.Vector.Sized as SV
import qualified Engine.Memory as Memory
import           GHC.Generics
import           Language.Haskell.TH
import qualified RIO
import           RIO (runRIO, RIO, glog, GLogFunc, HasGLogFunc(..))
import qualified System.Random as Rand

import           Engine.QLearning
import           Engine.OpenGames
import           Engine.TLL
import           Engine.OpticClass
import           Preprocessor.AbstractSyntax
import           Preprocessor.Compile
import           Preprocessor.THSyntax



------------------------------------
-- Configure observations and memory

type M = RIO (GLogFunc (QLearningMsg N Observation Action))

type N = 1

newtype Observation a = Obs
  { unObs :: (a, a)
  } deriving (Show,Generic,A.Ix,Ord, Eq, Functor)

----------------------
-- Configure QLearning
configQL = ConfigQLearning
  chooseBoltzQTable
  chooseUpdateBoltzQTable
  RewardExtendedExport



------------------
-- Introducing specialized learning rule:

-- Choose the optimal action given the current state
chooseBoltzQTable ::
     ( MonadIO m
      , MonadReader r m
      , HasGLogFunc r
      , GMsg r ~ QLearningMsg N Observation Action)
  => CTable Action
  -> State N Observation Action
  -> ST.StateT (State N Observation Action) m (Action,ActionChoice)
chooseBoltzQTable ls s = do
    theMaxScore <- maxScore obsVec (_qTable $ _env s) ls (_player (_env s))
    let temp      = _temperature $ _env s
        (_, gen') = Rand.randomR (0.0 :: Double, 1.0 :: Double) (_randomGen $ _env s)
        q         = _qTable $ _env s
        chooseNoExplore =
            do
              let (_, gen'')   = Rand.randomR (0.0 :: Double, 1.0 :: Double) gen'
                  action'      = snd $  theMaxScore
              return (action',"Exploitation")
    qCooperate    <- liftIO $ A.readArray q (obsVec, toIdx (Action True))
    qDefect       <- liftIO $ A.readArray q (obsVec, toIdx (Action False))
    let chooseExplore  =
          do
            let
                eCooperate       = (exp 1.0)** qCooperate / temp
                eDefect          = (exp 1.0)** qDefect / temp
                probCooperate    = eCooperate / (eCooperate + eDefect)
                (actionP, gen'') = Rand.randomR (0.0 :: Double, 1.0 :: Double) gen'
                action'          = Action (if actionP < probCooperate then True else False)
            return (action',"Exploration")
        in if temp < 0.01
           then chooseNoExplore
           else chooseExplore
  where obsVec = _obsAgent (_env s)

-- choose optimally or explore according to the Boltzmann rule
chooseUpdateBoltzQTable ::
     ( MonadIO m
     , MonadReader r m
     , HasGLogFunc r
     , GMsg r ~ QLearningMsg N Observation Action
     )
  => CTable Action
  -> State N Observation Action
  -> Observation Action
  -> (Action, ActionChoice)
  -> Double
  -> ST.StateT (State N Observation Action) m Action
chooseUpdateBoltzQTable ls s obs2 (action,_) reward  = do
    let q         = _qTable $ _env s
    prediction    <- liftIO $ A.readArray q (obsVec, toIdx action)
    let temp      = _temperature $ _env s
        (_, gen') = Rand.randomR (0.0 :: Double, 1.0 :: Double) (_randomGen $ _env s)
    maxed :: (Double, Action) <- Lift.lift $ maxScore (Memory.pushEnd obsVec (fmap toIdx (_obs s))) q ls (_player (_env s))
    let updatedValue = reward + gamma * (fst $ maxed)
        newValue     = (1 - learningRate) * prediction + learningRate * updatedValue
        -- newQ         = q A.// [((_obs s, action), newValue)]
    liftIO $ A.writeArray q (Memory.pushEnd obsVec (fmap toIdx (_obs s)), toIdx action) newValue
    ST.put $ updateRandomGQTableTemp (0.999) s gen'
    return action
  where obsVec = _obsAgent (_env s)




------------------
-- Types

newtype Action = Action Bool  deriving (Show, Eq, Ord)
instance ToIdx Action where
  toIdx =
    \case
      Action False -> Idx 0
      Action True -> Idx 1

actionSpace :: CTable Action
actionSpace = uniformCTable (fmap coerce (V.fromList [False,True]))

-------------
-- Parameters

gamma :: Double
gamma = 0.8

learningRate :: Double
learningRate = 0.40



pdMatrix :: Action -> Action -> Double
pdMatrix = on pdMatrix' coerce

pdMatrix' :: Bool -> Bool -> Double
pdMatrix' True True = 0.3
pdMatrix' True False = 0
pdMatrix' False True = 0.5
pdMatrix' False False = 0.1

-- Policy for deterministic player
titForTat :: Observation Action -> Action
titForTat (Obs (_,Action True))   = Action True
titForTat (Obs (_,Action False))  = Action False

-----------------------------------
-- Constructing initial state
-- TODO change this; make it random
lstIndexValues = [ (((Action x, Action y),Action z),v) | (((x,y),z),v) <-
  [(((True,True), True),0),(((True,False),True),0),(((False,True),True), 0),(((False,False),True),0),
    (((True,True), False),0),(((True,False),False),0),(((False,True),False), 0),(((False,False),False),0)]]

lstIndexValues2 = [
  (((True,True), True),4),(((True,False),True),0),(((False,True),True), 2),(((False,False),True),1),
   (((True,True), False),5),(((True,False),False),2),(((False,True),False), 4),(((False,False),False),1)]

-- TODO check whether this makes sense
-- TODO need a constructor for this more generally
initialArray :: MonadIO m => m (QTable N Observation Action)
initialArray = liftIO (do
   arr <- A.newArray_ (asIdx l, asIdx u)
   traverse (\(k, v) -> A.writeArray arr (asIdx k) v) lstIndexValues
   pure arr)
  where
    l = minimum $ fmap fst lstIndexValues
    u = maximum $ fmap fst lstIndexValues
    asIdx ((x, y), z) = (Memory.fromSV (SV.replicate (Obs (toIdx x, toIdx y))), toIdx z)


-- initialEnv and parameters
initialEnv1 :: M (Env N Observation Action)
initialEnv1 = initialArray >>= \arr -> pure $ Env "Player1" 1 arr 0  0.2  (Rand.mkStdGen 3) (fmap (fmap toIdx) (Memory.fromSV(SV.replicate initialObservation))) (5 * 0.999) "NothingHappendYet"
initialEnv2 :: M (Env N Observation Action)
initialEnv2 = initialArray >>= \arr ->  pure $ Env "PLayer2" 2 arr 0  0.2  (Rand.mkStdGen 100) (fmap (fmap toIdx) (Memory.fromSV(SV.replicate initialObservation))) (5 * 0.999) "NothingHappendYet"
-- ^ Value is taking from the benchmark paper Sandholm and Crites


---------------------------------------
-- Preparing the context for evaluation
-- Initial observation
-- TODO randomize it
initialObservation :: Observation Action
initialObservation = fmap coerce $ Obs (True,True)

-- | the initial observation is used twice as the context for two players requires it in that form
initialObservationContext :: (Observation Action, Observation Action)
initialObservationContext = (initialObservation,initialObservation)


initialContext :: Monad m => MonadicLearnLensContext m (Observation Action,Observation Action) () (Action,Action) ()
initialContext = MonadicLearnLensContext (pure initialObservationContext) (pure (\(_,_) -> pure ()))

-- initialstrategy
initiateStrat :: M (List '[M (Action, Env N Observation Action ), M Action])
initiateStrat = do e <- initialEnv1
                   pure $ pure (Action True,e) ::- pure (Action True) ::- Nil


------------------------------
-- Updating state


toObs :: Monad m => m (Action,Env N Observation Action) -> m Action -> m ((), (Observation Action,Observation Action))
toObs a1 a2 = do
             (act1,env1) <- a1
             act2 <- a2
             let obs1 = Obs (act1,act2)
                 obs2 = Obs (act2,act1)
                 in return ((),(obs1,obs2))


toObsFromLS :: Monad m => List '[m (Action,Env N Observation Action), m Action] -> m ((), (Observation Action,Observation Action))
toObsFromLS (x ::- (y ::- Nil))= toObs x y


-- From the outputted list of strategies, derive the context
fromEvalToContext :: Monad m =>  List '[m (Action,Env N Observation Action), m Action] ->
                     MonadContext m (Observation Action, Observation Action) () (Action,Action) ()
fromEvalToContext ls = MonadContext (toObsFromLS ls) (\_ -> (\_ -> pure ()))


------------------------------
-- Game stage 1
stageDeterministic = [opengame|
   inputs    : (state1,state2) ;
   feedback  :      ;

   :-----------------:
   inputs    :  state1    ;
   feedback  :      ;
   operation : pureDecisionQStage configQL actionSpace "Player1" ;
   outputs   :  act1 ;
   returns   :  (pdMatrix act1 act2, Obs (act1,act2)) ;

   inputs    : state2     ;
   feedback  :      ;
   operation : deterministicStratStage "Player2" titForTat ;
   outputs   :  act2 ;
   returns   :  (pdMatrix act2 act1, Obs (act1,act2))    ;
   :-----------------:

   outputs   :  (act1, act2)    ;
   returns   :      ;

|]

----------------------------------
-- Defining the iterator structure
evalStage ::
     List '[ M (Action, Env N Observation Action), M Action]
  -> MonadContext M (Observation Action, Observation Action) () (Action, Action) ()
  -> List '[ M (Action, Env N Observation Action), M Action]
evalStage  strat context  = evaluate stageDeterministic strat context



-- Explicit list constructor much better
evalStageLS startValue n =
          let context  = fromEvalToContext startValue
              newStrat = evalStage startValue context
              in if n > 0 then newStrat : evalStageLS newStrat (n-1)
                          else [newStrat]


hoist ::
     Applicative f
  => List '[ (Action, Env N Observation Action), Action]
  -> List '[ f (Action, Env N Observation Action), f Action]
hoist (x ::- y ::- Nil) = pure x ::- pure y ::- Nil

sequenceL ::
     Monad f
  => List '[ f (Action, Env N Observation Action), f Action]
  -> f (List '[ (Action, Env N Observation Action), Action])
sequenceL (x ::- y ::- Nil) = do
  v <- x
  v' <- y
  pure (v ::- v' ::- Nil)

evalStageM ::
     List '[ (Action, Env N Observation Action), Action]
  -> Int
  -> M [List '[ (Action, Env N Observation Action), Action]]
evalStageM startValue 0 = pure []
evalStageM startValue n = do
  newStrat <-
    sequenceL
      (evalStage (hoist startValue) (fromEvalToContext (hoist startValue)))
  rest <- evalStageM newStrat (pred n)
  pure (newStrat : rest)

{-# INLINE mapStagesM_ #-}
mapStagesM_ ::
  (s ->  List '[ (Action, Env N Observation Action), Action] -> M s)
  -> List '[ (Action, Env N Observation Action), Action]
  -> Int
  -> s
  -> M ()
mapStagesM_ f startValue n0 s0 = go s0 startValue n0
  where
    go _ _ 0 = pure ()
    go s value !n = do
      newStrat <-
        sequenceL (evalStage  (hoist value) (fromEvalToContext (hoist value)))
      s' <- f s newStrat
      go s' newStrat (pred n)
