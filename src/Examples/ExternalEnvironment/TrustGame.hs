{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}


module Examples.ExternalEnvironment.TrustGame where

import Engine.Engine hiding (fromLens, fromFunctions, state, nature, liftStochastic)
import Preprocessor.Preprocessor

import Engine.ExternalEnvironment
import Examples.SequentialMoves (trustGamePayoffProposer, trustGamePayoffResponder, Sent,SentBack)

import           Data.Tuple.Extra (uncurry3)

-- 1.1. Trust Game
trustGame pie factor = [opengame|

   inputs    :      ;
   feedback  :  (payoff1,payoff2)  ;

   :----------------------------:
   inputs    :   ;
   feedback  : payoff1     ;
   operation : interactWithEnv ;
   outputs   : sent ;
   returns   : trustGamePayoffProposer factor sent sentBack;

   inputs    : sent;
   feedback  : payoff2     ;
   operation : interactWithEnv;
   outputs   : sentBack ;
   returns   : trustGamePayoffResponder factor sent sentBack ;

   :----------------------------:

   outputs   :  (sent,sentBack)  ;
   returns   :      ;
   |]

-- 2. splitting things into components

proposerDecision ::  OpenGame
                      (MonadOptic IO)
                      (MonadContext IO)
                      ('[Double])
                      ('[])
                      Double
                      ()
                      Double
                      Double
proposerDecision  = [opengame|
   inputs    : pie     ;
   feedback  :   ;

   :----------------------------:
   inputs    : pie  ;
   feedback  : ignorePayoff    ;
   operation : interactWithEnv ;
   outputs   : sent ;
   returns   : payoffProposer    ;

   :----------------------------:

   outputs   :  sent        ;
   returns   :  payoffProposer    ;
  |]


responderDecision ::  OpenGame
                      (MonadOptic IO)
                      (MonadContext IO)
                      ('[Double])
                      ('[])
                      (Double,Double)
                      ()
                      Double
                      Double
responderDecision = [opengame|
   inputs    : pie,sent     ;
   feedback  :   ;

   :----------------------------:
   inputs    : pie,sent  ;
   feedback  : ignorePayoff     ;
   operation : interactWithEnv ;
   outputs   : sentBack ;
   returns   : payoffResponder ;

   :----------------------------:

   outputs   :  sentBack         ;
   returns   :  payoffResponder  ;
   |]


proposerPayoff  = [opengame|
   inputs    : pie, sent, sentBack    ;
   feedback  :   ;

   :----------------------------:
   inputs    : pie, sent, sentBack  ;
   feedback  :      ;
   operation : fromFunctions (uncurry3 trustGamePayoffProposer) id;
   outputs   : payoffSender;
   returns   :  ;

   :----------------------------:

   outputs   :  payoffSender    ;
   returns   :      ;
  |]

responderPayoff factor = [opengame|
   inputs    : sent, sentBack    ;
   feedback  :   ;

   :----------------------------:
   inputs    : sent, sentBack  ;
   feedback  :      ;
   operation : fromFunctions (uncurry $ trustGamePayoffProposer factor) id;
   outputs   : payoffResponder;
   returns   :  ;

   :----------------------------:

   outputs   :  payoffResponder    ;
   returns   :      ;
  |]


-- extract next state (action)
extractNextState :: MonadOptic m s t a b -> s -> m a
extractNextState (MonadOptic v _) x = do
  (_,a) <- v x
  pure a
 
  
{-
-- extract continuation
extractPayoff :: MonadOptic m s t a b -> s -> b -> m t
extractPayoff (MonadOptic v u) x r= do
  (z,_) <- v x
  u z r

 



--- Control flow now:

-- 0 extractNextState from proposerDecision -> _sent_
-- 1 extractNextState from responderDecision ->  _sentBack_
-- 2 responderDecision produces _payoff2_ -> 
-- 3 senderDecision produces _payoff1_ 


testSender = senderDecision (3 :: Double)

testResponder = responderDecision (3 :: Double)
--}


proposerEval = extractNextState (play proposerDecision (10 ::- Nil)) 10
                                                                     -- ^ this is the s input in the extractState, the _pie_ in the p_roposerGame_

responderEval = extractNextState (play responderDecision (5 ::- Nil)) (10,5)

proposerPayoffEval = extractNextState (play proposerPayoff Nil) (10,10,5)

responderPayoffEval factor = extractNextState (play (responderPayoff factor) Nil) (10,5)
