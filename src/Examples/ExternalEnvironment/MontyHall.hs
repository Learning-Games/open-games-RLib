{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

module Examples.ExternalEnvironment.MontyHall where

import Engine.Engine hiding (fromLens, fromFunctions, state, nature)
import Preprocessor.Preprocessor

import Control.Monad.IO.Class   (liftIO)
import Network.Wai.Handler.Warp (setPort, setBeforeMainLoop, runSettings, defaultSettings)
import Servant                  ((:>), (:<|>)((:<|>)), Server, Handler, Application
                                , Proxy(..), JSON, Get, Post, ReqBody, serve)

import Engine.ExternalEnvironment
import Examples.ExternalEnvironment.Common (extractPayoff, extractPayoffAndNextState)

import qualified Data.Set as S
import           System.Random

import Data.Aeson               (ToJSON, FromJSON)
import GHC.Generics             (Generic)

import Control.Monad (replicateM)

-- TODO Use the dependency operator for that!

-------------
-- Data types
type DoorCar  = Int
type Door     = Int
type DoorGoat = Int

type ChangeChoice = Bool

data PlayParameters = PlayParameters
  { theDoor :: Door
  , playerAction :: ChangeChoice
  } deriving (Show, Generic, ToJSON, FromJSON)

data PlayResult = PlayResult
  { playerPayoff  :: Double
  } deriving (Show, Generic, ToJSON, FromJSON)

-------------
-- Parameters

initialSetDoors :: S.Set DoorCar
initialSetDoors = S.fromList [1,2,3]

prize :: Double
prize = 10

----------------------
-- Auxiliary functions

-- Fix door behind which is the car
chooseWinningDoor :: IO DoorCar
chooseWinningDoor = do
  g <- newStdGen
  let winning_door = fst $ randomR (1,3) g
  -- putStrLn $ "winning door : " ++ show winning_door -- debugging print
  return winning_door

-- Given a set of doors choose one randomly
-- chooseRandomDoorFromSet bug: index out of bounds (0,s)
chooseRandomDoorFromSet :: S.Set Door -> IO DoorGoat
chooseRandomDoorFromSet doors = do
  g <- newStdGen
  let s = S.size doors
      index = fst $ randomR (0, s - 1) g
  let result = S.elemAt index doors
  return result

-- Given door with car behind and chosen door, reveal door which contains a goat
-- revealGoatDoor bug: the resulting index does not refer to the original list
revealGoatDoor :: DoorCar -> Door -> IO DoorGoat
revealGoatDoor winner choice = do
  -- putStrLn $ "chosen door  : " ++ show choice -- debugging print
  opened_door <- chooseRandomDoorFromSet $ S.delete choice $ S.delete winner initialSetDoors
  -- ^ ignore the winner door and the chosen door (might overlap, but that's alright)
  -- putStrLn $ "opened door  : " ++ show opened_door -- debugging print
  return opened_door

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
montyHallExternal :: OpenGame
                       (MonadOptic IO)
                       (MonadContext IO)
                       '[Door,Bool]
                       '[]
                       ()
                       (Double)
                       (Door, Bool)
                       ()
montyHallExternal = [opengame|

   inputs    :      ;
   feedback  :  payoff    ;

   :----------------------------:
   inputs    :     ;
   feedback  :     ;
   operation : nature chooseWinningDoor ;
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

   inputs    : revealedDoor ;
   feedback  : payoff ;
   operation : interactWithEnv ;
   outputs   : decision2 ;
   returns   : payoffDecision winningDoor decision1 decision2 ;
   // reverse or keep first choice; if winning prize else zero
   // GEORGE: why do we need revealedDoor as an input??

   :----------------------------:

   outputs   : decision1,decision2 ;
   returns   :    ;
  |]


test :: PlayParameters -> IO PlayResult
test PlayParameters { theDoor, playerAction } = do
  -- putStrLn $ "switch door  : " ++ show playerAction -- debugging print

  let strategy :: List '[Door, ChangeChoice]
      strategy = theDoor ::- playerAction ::- Nil

      gamenext :: MonadOptic IO () Double (Door, Bool) ()
      gamenext = play montyHallExternal strategy

  (p, _ns) <- extractPayoffAndNextState gamenext () ()

  return $ PlayResult { playerPayoff = p }

switch :: Int -> IO Double
switch n = fmap (/ fromIntegral n) (sum <$> replicateM n (playerPayoff <$> test (PlayParameters { theDoor = 2, playerAction = True })))

dontSwitch :: Int -> IO Double
dontSwitch n = fmap (/ fromIntegral n) (sum <$> replicateM n (playerPayoff <$> test (PlayParameters { theDoor = 2, playerAction = False })))

--------------
-- Servant API

-- Using: https://httpie.io/docs/cli/json
--
--  http localhost:3000/healthcheck
--  http POST localhost:3000/play < json-data/mh-game.json
--
type Api = "play" :> ReqBody '[JSON] PlayParameters :> Post '[JSON] PlayResult
           :<|> "healthcheck" :> Get '[JSON] String

api :: Proxy Api
api = Proxy

server :: Server Api
server =
  runPlay
  :<|> return "Ok!"

run :: IO ()
run = do
  let port = 3000
      settings =
        setPort port $
          setBeforeMainLoop (putStrLn ("Listening on port " ++ show port)) $
            defaultSettings
  runSettings settings =<< mkApp

mkApp :: IO Application
mkApp = return $ serve api server

runPlay :: PlayParameters -> Handler PlayResult
runPlay PlayParameters { theDoor, playerAction } = do

  let strategy = theDoor ::- playerAction ::- Nil
      gamenext = play montyHallExternal strategy

  -- TODO: Does this need to be in IO?
  p <- liftIO $ extractPayoff gamenext () ()

  return $ PlayResult { playerPayoff = p }

