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


module Examples.ExternalEnvironment.MontyHall where

import Engine.Engine hiding (fromLens, fromFunctions, state, nature, liftStochastic)
import Preprocessor.Preprocessor

import Engine.ExternalEnvironment
import Examples.ExternalEnvironment.Common (extractPayoff)

import qualified Data.Set as S
import           System.Random
import qualified System.Random as Rand
import           System.Random.MWC.CondensedTable



-- TODO Use the dependency operator for that!

-------------
-- Data types
type DoorCar  = Int
type Door     = Int
type DoorGoat = Int

type ChangeChoice = Bool

-------------
-- Parameters
initialSetDoors = S.fromList [1,2,3]
prize = 10

----------------------
-- Auxiliary functions

-- Fix door behind which is the car
chooseDoor :: IO DoorCar
chooseDoor = do
  g <- newStdGen
  return $ fst $ randomR (1,3) g

-- Given a set of doors choose one randomly
revealDoorGoat :: S.Set Door -> IO DoorGoat
revealDoorGoat doors = do
  g <- newStdGen
  let s = S.size doors
      index = fst $ randomR (0,s) g
  return $ S.elemAt index doors

-- Given door with car behind and chosen door, reveal door which contains a goat
revealGoatDoor :: DoorCar -> Door -> IO DoorGoat
revealGoatDoor winner choice = do
  case winner == choice of
    True -> revealDoorGoat $ S.delete winner initialSetDoors
    -- ^ If winning door has been chosen, randomly choose one of the goat doors
    _    -> revealDoorGoat $ S.delete choice $ S.delete winner initialSetDoors
    -- ^ If a goat door has been chosen, reveal the other goat door

----------
-- Payoffs
payoffDecision :: DoorCar ->  Door -> ChangeChoice -> Double
payoffDecision winner doorChosen choiceChanged
  | winner == doorChosen && not choiceChanged  = prize
  -- ^ if you chose the right door initially and did not change your mind, you win
  | winner /= doorChosen && choiceChanged      = prize
  -- ^ if you chose the wrong door initially and change your mind, you win
  | otherwise = 0

-------
-- Game
montyHall :: OpenGame
                (MonadOptic IO)
                (MonadContext IO)
                '[Door,Bool]
                '[]
                ()
                (Double)
                (Door,Bool)
                ()
montyHall = [opengame|

   inputs    :      ;
   feedback  :  payoff    ;

   :----------------------------:
   inputs    :     ;
   feedback  :     ;
   operation : nature chooseDoor ;
   outputs   : winningDoor ;
   returns   :     ;
   // Chooses behind which door the car is

   inputs    :     ;
   feedback  : ignoreZeroPayoff ;
   operation : interactWithEnv ;
   outputs   : decision1 ;
   returns   : 0 ;
   // first decision; fix payoff at zero

   inputs    : winningDoor,decision1 ;
   feedback  :  ;
   operation : liftStochastic (uncurry revealGoatDoor) ;
   outputs   : revealedDoor ;
   returns   :  ;
   // if first decision did not win the prize, reveal one door which contains a goat

   inputs    : revealedDoor  ;
   feedback  : payoff ;
   operation : interactWithEnv ;
   outputs   : decision2 ;
   returns   : payoffDecision winningDoor decision1 decision2 ;
   // reverse or keep first choice; if winning prize else zero

   :----------------------------:

   outputs   : decision1,decision2 ;
   returns   :    ;
  |]



