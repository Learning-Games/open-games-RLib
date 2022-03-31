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


module Examples.ExternalEnvironment.RockPaperScissors where


import Engine.Engine hiding (fromLens, fromFunctions, state)
import Preprocessor.Preprocessor

import Engine.ExternalEnvironment


-- Types

data ActionRPS = Rock | Paper | Scissors
  deriving (Ord,Eq,Show)

-- Payoff function

rockPaperScissorsPayoff :: ActionRPS -> ActionRPS -> Double
rockPaperScissorsPayoff a1 a2
  | a1 == Rock && a2 == Rock  = 0
  | a1 == Rock && a2 == Paper = -1
  | a1 == Rock && a2 == Scissors = 1
  | a1 == Paper && a2 == Rock  = 1
  | a1 == Paper && a2 == Paper = 0
  | a1 == Paper && a2 == Scissors = -1
  | a1 == Scissors && a2 == Rock  = -1
  | a1 == Scissors && a2 == Paper = 1
  | a1 == Scissors && a2 == Scissors = 0



-- Game

rockPaperScissorsExternal :: OpenGame
                                    (MonadOptic IO)
                                    (MonadContext IO)
                                    '[ActionRPS, ActionRPS]
                                    '[]
                                    ()
                                    (Double, Double)
                                    (ActionRPS, ActionRPS)
                                    ()
rockPaperScissorsExternal = [opengame|

   inputs    :      ;
   feedback  : (payoff1,payoff2)     ;

   :----------------------------:
   inputs    :      ;
   feedback  : payoff1    ;
   operation : interactWithEnv ;
   outputs   : decisionPlayer1 ;
   returns   : rockPaperScissorsPayoff decisionPlayer1 decisionPlayer2 ;

   inputs    :      ;
   feedback  : payoff2    ;
   operation : interactWithEnv ;
   outputs   : decisionPlayer2 ;
   returns   : rockPaperScissorsPayoff decisionPlayer2 decisionPlayer1 ;

   :----------------------------:

   outputs   : (decisionPlayer1, decisionPlayer2)   ;
   returns   :     ;
  |]


-- extract continuation
extractPayoff :: MonadOptic IO () (Double,Double) (ActionRPS,ActionRPS) () -> IO (Double,Double)
extractPayoff (MonadOptic v u) = do
  (z,_) <- v ()
  u z ()


-- extract next state (action)
extractNextState :: MonadOptic IO () (Double,Double) (ActionRPS,ActionRPS) () -> IO (ActionRPS,ActionRPS)
extractNextState (MonadOptic v _) = do
  (_,a) <- v ()
  pure a




