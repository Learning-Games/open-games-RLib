{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}

module Examples.InteractiveIO.InteractiveIO where

import Engine.InteractiveIO
import Engine.OpenGames
import Engine.OpticClass
import Preprocessor.Preprocessor



import Control.Monad.Reader

------------------------------------------------------------------------
-- Implement a simple interactive game which does evaluation accordingly

-----------------------
-- 1. Types and payoffs

-- 1.0. Prisoner's dilemma
data ActionPD = Cooperate | Defect
  deriving (Eq, Ord, Show)

-- | Payoff matrix for player i given i's action and j's action
prisonersDilemmaMatrix :: ActionPD -> ActionPD -> Double
prisonersDilemmaMatrix Cooperate Cooperate   = 3
prisonersDilemmaMatrix Cooperate Defect  = 0
prisonersDilemmaMatrix Defect Cooperate  = 5
prisonersDilemmaMatrix Defect Defect = 1

--------------------
-- 1. Representation
-- 1.0 Prisoner's dilemma

prisonersDilemmaIO ::
                       InteractiveStageGame
                       '[ActionPD, ActionPD]
                       '[[DiagnosticInfoInteractive ActionPD], [DiagnosticInfoInteractive ActionPD]]
                       ()
                       ()
                       ()
                       ()
prisonersDilemmaIO = [opengame|

   inputs    :      ;
   feedback  :      ;

   :----------------------------:
   inputs    :      ;
   feedback  :      ;
   operation : interactiveInput "player1" [Cooperate,Defect];
   outputs   : decisionPlayer1 ;
   returns   : prisonersDilemmaMatrix decisionPlayer1 decisionPlayer2 ;

   inputs    :      ;
   feedback  :      ;
   operation : interactiveInput "player2" [Cooperate,Defect];
   outputs   : decisionPlayer2 ;
   returns   : prisonersDilemmaMatrix decisionPlayer2 decisionPlayer1 ;

   :----------------------------:

   outputs   :      ;
   returns   :      ;
  |]


-- interfacing outside world
inputStrat x = if x == "cooperate" then Cooperate else Defect
