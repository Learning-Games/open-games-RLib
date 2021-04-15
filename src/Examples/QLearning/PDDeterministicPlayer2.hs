{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators, ScopedTypeVariables, TupleSections, DataKinds, GADTs, FlexibleContexts, TemplateHaskell, QuasiQuotes, DeriveAnyClass #-}


module Examples.QLearning.PDDeterministicPlayer2
                     ( evalStageLS
                     , initiateStrat
                     )
                     where

import Control.Comonad
import Data.Functor.Identity
import Language.Haskell.TH
import qualified Control.Monad.Trans.State as ST
import qualified GHC.Arr as A
import GHC.Generics
import qualified System.Random as Rand

import Engine.QLearning2
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
initialEnv1 = Env initialArray  0.2  (Rand.mkStdGen 3) (5 * 0.999)
initialEnv2 = Env initialArray  0.2  (Rand.mkStdGen 100) (5 * 0.999)
-- ^ Value is taking from the benchmark paper Sandholm and Crites


initialObservation :: (Observation Action, Observation Action)
initialObservation = ((True,True),(True,True))


initialContext :: Monad m => MonadContext m (Observation Action,Observation Action) () (Action,Action) ()
initialContext = MonadContext (pure (() ,initialObservation)) (\_ -> (\_ -> pure ()))

-- initialstrategy
initiateStrat :: List '[(Action, Env Action ), Action]
initiateStrat = (True,initialEnv1) ::- True ::- Nil


------------------------------
-- Updating state

toObs :: (Action,Env Action) -> Action -> (Observation Action,Observation Action)
toObs (act1,env1) act2 = 
             let obs1 = (act1,act2)
                 obs2 = (act2,act1)
                 in (obs1,obs2)

toObsFromLS :: List '[(Action,Env Action),Action] ->  (Observation Action,Observation Action)
toObsFromLS (x ::- (y ::- Nil))= toObs x y


-- From the outputted list of strategies, derive the context
fromEvalToContext :: List '[(Action,Env Action),Action] ->
                     MonadContext Identity (Observation Action, Observation Action) () (Action,Action) ()
fromEvalToContext ls = MonadContext (pure ((),toObsFromLS ls)) (\_ -> (\_ -> pure ()))



------------------------------
-- Game stage 1

generateGame "stageDeterministic" ["helper"]
                (Block ["state1", "state2"] []
                [ Line [[|state1|]] [] [|pureDecisionQStage [False,True] "Player1" chooseExploreAction (updateQTableST learningRate gamma)|] ["act1"]  [[|(pdMatrix act1 act2, (act1,act2))|]]
                , Line [[|state2|]] [] [|deterministicStratStage "Player2" titForTat|] ["act2"]  [[|(pdMatrix act2 act1, (act1,act2))|]]]
                [[|(act1, act2)|]] [] :: Block String (Q Exp))




----------------------------------
-- Defining the iterator structure
evalStage  strat context  = evaluate (stageDeterministic "helper") strat context


-- Explicit list constructor much better
evalStageLS startValue n = do
              context <- fromEvalToContext startValue
              let newStrat = evalStage startValue context
                  in if n > 0 then newStrat : evalStageLS newStrat (n-1)
                              else [newStrat]

