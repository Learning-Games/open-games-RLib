{-# LANGUAGE TypeOperators, ScopedTypeVariables, TupleSections, DataKinds, GADTs, FlexibleContexts, TemplateHaskell, QuasiQuotes #-}


module Examples.QLearning.PD
   --                  ( iterateEval
   --                  , evalStageLS
   --                  , evalStageLS2
   --                  , initiateStrat)
                     where

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
  (((Cooperate,Cooperate), Cooperate),0),(((Cooperate,Defect),Cooperate),2),(((Defect,Cooperate),Cooperate), 0),(((Defect,Defect),Cooperate),4),
   (((Cooperate,Cooperate), Defect),1),(((Cooperate,Defect),Defect),0),(((Defect,Cooperate),Defect), 3),(((Defect,Defect),Defect),0)]



lstIndexValues2 = [
  (((Cooperate,Cooperate), Cooperate),4),(((Cooperate,Defect),Cooperate),0),(((Defect,Cooperate),Cooperate), 2),(((Defect,Defect),Cooperate),1),
   (((Cooperate,Cooperate), Defect),5),(((Cooperate,Defect),Defect),2),(((Defect,Cooperate),Defect), 4),(((Defect,Defect),Defect),1)]

-- TODO check whether this makes sense
-- TODO need a constructor for this more generally
initialArray :: QTable
initialArray =  A.array (((Cooperate,Cooperate),Cooperate),((Defect,Defect),Defect)) lstIndexValues


-- initialEnv and parameters
initialEnv = PDEnv initialArray  0.2  (Rand.mkStdGen 3)
initialState = State initialEnv (Cooperate,Cooperate)


initialContext :: Monad m => MonadicLearnLensContext m (State,State) (Double,Double) (Action,Action) (Double,Double)
initialContext = MonadicLearnLensContext (pure (initialState,initialState)) (pure (\(_,_) -> pure (0,0)))

-- initialstrategy
initiateStrat :: List '[Identity (Action, PDEnv), Identity (Action, PDEnv)]
initiateStrat = pure (Cooperate,initialEnv) ::- pure (Cooperate,initialEnv) ::- Nil


------------------------------
-- Updating state

toState :: Monad m => m (Action,PDEnv) -> m (Action,PDEnv) -> m (State,State)
toState a1 a2 = do
             (act1,env1) <- a1
             (act2,env2) <- a2
             let obs = (act1,act2)
                in pure $ ((State env1 obs),(State env2 obs))

toStateFromLS :: Monad m => List '[m (Action,PDEnv),m (Action,PDEnv)] -> m (State,State)
toStateFromLS (x ::- (y ::- Nil))= toState x y


-- From the outputted list of strategies, derive the context
fromEvalToContext :: Monad m =>
                     List '[m (Action,PDEnv),m (Action,PDEnv)] ->
                     MonadicLearnLensContext m (State,State) (Double,Double) (Action,Action) (Double,Double)
fromEvalToContext ls = MonadicLearnLensContext (toStateFromLS ls) (pure (\(a1,a2) -> pure (pdMatrix a1 a2, pdMatrix a2 a1)))



------------------------------
-- Game stage 1
-- TODO still unclear whether there is actually a link to continuation payoffs
-- TODO how is the contravariant output used? Is it? But I could reuse it at the decision-level! 

generateGame "pdQDecStage" ["helper"]
                (Block ["state1", "state2"] [[|(pdMatrix act1 act2 + gamma * cont1, pdMatrix act2 act1 + gamma * cont2)|]]
                [ Line [[|state1|]] [] [|pureDecisionQStage "Player1"|] ["act1"]  [[|pdMatrix act1 act2|]]
                , Line [[|state2|]] [] [|pureDecisionQStage "Player2"|] ["act2"]  [[|pdMatrix act2 act1|]]]
                [[|(act1, act2)|]] ["cont1","cont2"])


--------------------------------------------------------
-- Game stage 1 with payoffs being discounted internally
generateGame "pdQDecStage'" ["helper"]
                (Block ["state1", "state2"] [[|(cont1', cont2')|]]
                [ Line [[|state1|]] ["cont1'"] [|pureDecisionQStage' "Player1"|] ["act1"]  [[|cont1|]]
                , Line [[|state2|]] ["cont2'"] [|pureDecisionQStage' "Player2"|] ["act2"]  [[|cont2|]]]
                [[|(act1, act2)|]] ["cont1","cont2"])




--------------------------------------------------------
-- Game stage 1 with payoffs being discounted internally
generateGame "pdQDecStage2" ["helper"]
                (Block ["state1", "state2"] [[|(cont1', cont2')|]]
                [ Line [[|state1|]] ["cont1'"] [|pureDecisionQStage2 "Player1"|] ["act1"]  [[|cont1|]]
                , Line [[|state2|]] ["cont2'"] [|pureDecisionQStage2 "Player2"|] ["act2"]  [[|cont2|]]]
                [[|(act1, act2)|]] ["cont1","cont2"])



----------------------------------
-- Defining the iterator structure
evalStage  strat context  = evaluate (pdQDecStage "helper") strat context

evalStage' strat context = evaluate (pdQDecStage' "helper") strat context


evalStage2 strat context = evaluate (pdQDecStage2 "helper") strat context

-- One solution; nest the iterator; very bad memory-wise 
iterateEval 0  = evalStage initiateStrat initialContext
iterateEval n  = evalStage (iterateEval (n-1)) (fromEvalToContext (iterateEval (n-1)))

-- Explicit list constructor much better
evalStageLS startValue n =
          let context  = fromEvalToContext startValue
              newStrat = evalStage startValue context
              in if n > 0 then newStrat : evalStageLS newStrat (n-1)
                          else [newStrat]

