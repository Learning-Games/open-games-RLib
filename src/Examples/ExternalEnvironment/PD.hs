{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}

module Examples.ExternalEnvironment.PD where


import Engine.Engine hiding (fromLens, fromFunctions)
import Preprocessor.Preprocessor

import Engine.ExternalEnvironment
import Examples.SimultaneousMoves (prisonersDilemmaMatrix,ActionPD(..))


prisonersDilemmaExternal = [opengame|

   inputs    :      ;
   feedback  : (payoff1,payoff2)     ;

   :----------------------------:
   inputs    :      ;
   feedback  : payoff1    ;
   operation : interactWithEnv ;
   outputs   : decisionPlayer1 ;
   returns   : prisonersDilemmaMatrix decisionPlayer1 decisionPlayer2 ;

   inputs    :      ;
   feedback  : payoff2    ;
   operation : interactWithEnv ;
   outputs   : decisionPlayer2 ;
   returns   : prisonersDilemmaMatrix decisionPlayer2 decisionPlayer1 ;

   :----------------------------:

   outputs   : (decisionPlayer1, decisionPlayer2)   ;
   returns   :     ;
  |]


-- extract continuation
extractPayoff :: MonadOptic IO () (Double,Double) (ActionPD,ActionPD) () -> IO (Double,Double)
extractPayoff (MonadOptic v u) = do
  (z,_) <- v ()
  u z ()


-- extract next state (action)
extractNextState :: MonadOptic IO () (Double,Double) (ActionPD,ActionPD) () -> IO (ActionPD,ActionPD)
extractNextState (MonadOptic v _) = do
  (_,a) <- v ()
  pure a


strat = Cooperate ::- Cooperate ::- Nil

test = play prisonersDilemmaExternal strat



{-- Usage: 
extractPayoff test

extractNextState test
-}





