{-# LANGUAGE TypeOperators, ScopedTypeVariables, TupleSections, DataKinds, GADTs, FlexibleContexts, TemplateHaskell, QuasiQuotes #-}


module Examples.QLearning.PDDeterministicPlayer
   --                  ( iterateEval
   --                  , evalStageLS
   --                  , evalStageLS2
   --                  , initiateStrat)
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


pdMatrix :: Action -> Action -> Double
pdMatrix Cooperate Cooperate = 5
pdMatrix Cooperate Defect = 0
pdMatrix Defect Cooperate = 2
pdMatrix Defect Defect = 1



-----------------------------
-- Constructing initial state
-- TODO change this
lstIndexValues = [
  (((Cooperate,Cooperate), Cooperate),0),(((Cooperate,Defect),Cooperate),0),(((Defect,Cooperate),Cooperate), 0),(((Defect,Defect),Cooperate),0),
   (((Cooperate,Cooperate), Defect),0),(((Cooperate,Defect),Defect),0),(((Defect,Cooperate),Defect), 0),(((Defect,Defect),Defect),0)]



lstIndexValues2 = [
  (((Cooperate,Cooperate), Cooperate),4),(((Cooperate,Defect),Cooperate),0),(((Defect,Cooperate),Cooperate), 2),(((Defect,Defect),Cooperate),1),
   (((Cooperate,Cooperate), Defect),5),(((Cooperate,Defect),Defect),2),(((Defect,Cooperate),Defect), 4),(((Defect,Defect),Defect),1)]

-- TODO check whether this makes sense
-- TODO need a constructor for this more generally
initialArray :: QTable
initialArray =  A.array (((Cooperate,Cooperate),Cooperate),((Defect,Defect),Defect)) lstIndexValues


-- initialEnv and parameters
initialEnv1 = PDEnv initialArray  0.2  (Rand.mkStdGen 3)
initialEnv2 = PDEnv initialArray  0.2  (Rand.mkStdGen 100)



initialObservation :: (Observation, Observation)
initialObservation = ((Cooperate,Cooperate),(Cooperate,Cooperate))


initialContext :: Monad m => MonadicLearnLensContext m (Observation,Observation) () (Action,Action) ()
initialContext = MonadicLearnLensContext (pure initialObservation) (pure (\(_,_) -> pure ()))

-- initialstrategy
initiateStrat :: List '[Identity (Action, PDEnv), Identity Action]
initiateStrat = pure (Cooperate,initialEnv1) ::- pure Cooperate ::- Nil


------------------------------
-- Updating state

toObs :: (Comonad m, Monad m) => m (Action,PDEnv) -> m Action -> m (Observation,Observation)
toObs a1 a2 = do
             (act1,env1) <- a1
             act2 <- a2
             let obs1 = (act1,act2)
                 obs2 = (act2,act1)
                 in pure (obs1,obs2)

toObsFromLS :: (Comonad m, Monad m) => List '[m (Action,PDEnv),m Action] -> m (Observation,Observation)
toObsFromLS (x ::- (y ::- Nil))= toObs x y


-- From the outputted list of strategies, derive the context
fromEvalToContext :: (Comonad m, Monad m) =>
                     List '[m (Action,PDEnv),m Action] ->
                     MonadicLearnLensContext m (Observation, Observation) () (Action,Action) ()
fromEvalToContext ls = MonadicLearnLensContext (toObsFromLS ls) (pure (\_ -> pure ()))



------------------------------
-- Game stage 1
-- TODO still unclear whether there is actually a link to continuation payoffs
-- TODO how is the contravariant output used? Is it? But I could reuse it at the decision-level! 

generateGame "stageDeterministic" ["helper"]
                (Block ["state1", "state2"] []
                [ Line [[|state1|]] [] [|pureDecisionQStage' "Player1"|] ["act1"]  [[|(pdMatrix act1 act2, (act1,act2))|]]
                , Line [[|state2|]] [] [|deterministicStratStage "Player2" titForTat|] ["act2"]  [[|(pdMatrix act2 act1, (act1,act2))|]]]
                [[|(act1, act2)|]] [])




----------------------------------
-- Defining the iterator structure
evalStage  strat context  = evaluate (stageDeterministic "helper") strat context


-- One solution; nest the iterator; very bad memory-wise 
iterateEval 0  = evalStage initiateStrat initialContext
iterateEval n  = evalStage (iterateEval (n-1)) (fromEvalToContext (iterateEval (n-1)))

-- Explicit list constructor much better
evalStageLS startValue n =
          let context  = fromEvalToContext startValue
              newStrat = evalStage startValue context
              in if n > 0 then newStrat : evalStageLS newStrat (n-1)
                          else [newStrat]


