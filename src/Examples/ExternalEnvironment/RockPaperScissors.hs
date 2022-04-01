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
rockPaperScissorsPayoff Rock Rock = 0
rockPaperScissorsPayoff Rock Paper = -1
rockPaperScissorsPayoff Rock Scissors = 1
rockPaperScissorsPayoff Paper Rock = 1
rockPaperScissorsPayoff Paper Paper = 0
rockPaperScissorsPayoff Paper Scissors = -1
rockPaperScissorsPayoff Scissors Rock = -1
rockPaperScissorsPayoff Scissors Paper = 1
rockPaperScissorsPayoff Scissors Scissors = 0



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


-- extractPayoff :: MonadOptic IO () (Double,Double) (ActionRPS,ActionRPS) () -> IO (Double,Double)
-- extractNextState :: MonadOptic IO () (Double,Double) (ActionRPS,ActionRPS) () -> IO (ActionRPS,ActionRPS)

