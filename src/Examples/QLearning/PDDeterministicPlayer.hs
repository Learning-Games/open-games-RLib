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


module Examples.QLearning.PDDeterministicPlayer
--                     ( evalStageLS
--                     , initiateStrat
--                     )
                     where

import Control.Comonad
import Data.Functor.Identity
import Language.Haskell.TH
import qualified Control.Monad.Trans.State as ST
import qualified GHC.Arr as A
import GHC.Generics
import qualified System.Random as Rand

import Engine.QLearning
import Engine.OpenGames
import Engine.TLL
import Engine.OpticClass
import Preprocessor.AbstractSyntax
import Preprocessor.Compile
import Preprocessor.THSyntax



------------------
-- Types

type Action = Bool


-------------
-- Parameters

gamma = 0.7

learningRate = 0.40





pdMatrix :: Action -> Action -> Double
pdMatrix True True = 0.3 
pdMatrix True False = 0
pdMatrix False True = 0.5
pdMatrix False False = 0.1

-- Policy for deterministic player
titForTat :: Observation Action -> Action
titForTat (_,True) = True
titForTat (_,False)    = False



-----------------------------
-- Constructing initial state
-- TODO change this
lstIndexValues = [
  (((True,True), True),0),(((True,False),True),0),(((False,True),True), 0),(((False,False),True),0),
   (((True,True), False),0),(((True,False),False),0),(((False,True),False), 0),(((False,False),False),0)]



lstIndexValues2 = [
  (((True,True), True),4),(((True,False),True),0),(((False,True),True), 2),(((False,False),True),1),
   (((True,True), False),5),(((True,False),False),2),(((False,True),False), 4),(((False,False),False),1)]

-- TODO check whether this makes sense
-- TODO need a constructor for this more generally
initialArray :: QTable Action
initialArray =  A.array (((False,False),False),((True,True),True)) lstIndexValues


-- initialEnv and parameters
initialEnv1 = Env "Player1" initialArray  0.2  (Rand.mkStdGen 3) initialObservation (5 * 0.999)
initialEnv2 = Env "Player2" initialArray  0.2  (Rand.mkStdGen 100) initialObservation (5 * 0.999)
-- ^ Value is taking from the benchmark paper Sandholm and Crites

---------------------------------------
-- Preparing the context for evaluation
-- Initial observation
-- TODO randomize it
initialObservation :: Observation Action
initialObservation = (True,True)

-- | the initial observation is used twice as the context for two players requires it in that form
initialObservationContext :: (Observation Action, Observation Action)
initialObservationContext = (initialObservation,initialObservation)


initialContext :: Monad m => MonadContext m (Observation Action,Observation Action) () (Action,Action) ()
initialContext = MonadContext (pure (() ,initialObservationContext)) (\_ -> (\_ -> pure ()))

-- initialstrategy
initiateStrat :: List '[Identity (Action, Env Action ), Identity Action]
initiateStrat =  pure (True,initialEnv1) ::- pure True ::- Nil


------------------------------
-- Updating state

toObs :: Monad m => m (Action,Env Action) -> m Action -> m ((), (Observation Action,Observation Action))
toObs a1 a2 = do
             (act1,env1) <- a1
             act2 <- a2
             let obs1 = (act1,act2)
                 obs2 = (act2,act1)
                 in return ((),(obs1,obs2))


toObsFromLS :: Monad m => List '[m (Action,Env Action), m Action] -> m ((), (Observation Action,Observation Action))
toObsFromLS (x ::- (y ::- Nil))= toObs x y


-- From the outputted list of strategies, derive the context
fromEvalToContext :: Monad m =>  List '[m (Action,Env Action), m Action] ->
                     MonadContext m (Observation Action, Observation Action) () (Action,Action) ()
fromEvalToContext ls = MonadContext (toObsFromLS ls) (\_ -> (\_ -> pure ()))



------------------------------
-- Game stage 1

generateGame "stageDeterministic" ["helper"]
                (Block ["state1", "state2"] []
                [ Line [[|state1|]] [] [|pureDecisionQStage [False,True] "Player1" chooseExploreAction (updateQTableST learningRate gamma)|] ["act1"]  [[|(pdMatrix act1 act2, (act1,act2))|]]
                , Line [[|state2|]] [] [|deterministicStratStage "Player2" titForTat|] ["act2"]  [[|(pdMatrix act2 act1, (act1,act2))|]]]
                [[|(act1, act2)|]] [] :: Block String (Q Exp))


----------------------------------
-- Defining the iterator structure

evalStage :: List '[Identity (Action, Env Action), Identity Action]
            -> MonadContext
                  Identity
                  (Observation Action, Observation Action)
                  ()
                  (Action, Action)
                  ()
            -> List '[Identity (Action, Env Action), Identity Action]
evalStage = evaluate (stageDeterministic "helper") 


-- Explicit list constructor much better
evalStageLS startValue n =
          let context  = fromEvalToContext startValue
              newStrat = evalStage startValue context
              in if n > 0 then newStrat : evalStageLS newStrat (n-1)
                          else [newStrat]

