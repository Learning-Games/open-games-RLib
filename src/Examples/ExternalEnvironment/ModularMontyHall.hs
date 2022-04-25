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

module Examples.ExternalEnvironment.ModularMontyHall where

import           Control.Exception                   (handle, SomeException(..))
import           Control.Monad                       (forever)
import           Control.Monad                       (replicateM)
import           Control.Monad.IO.Class              (liftIO)
import           Data.Aeson                          (ToJSON, FromJSON)
import           Data.Aeson                          (encode, decode)
import           Data.ByteString.Lazy.Internal       (ByteString)
import           Data.Tuple.Extra                    (uncurry3)
import           Engine.Engine hiding (fromLens, fromFunctions, state, nature)
import           Engine.ExternalEnvironment
import           Examples.ExternalEnvironment.Common (extractNextState)
import           GHC.Generics                        (Generic)
import           Network.WebSockets.Connection       (PendingConnection)
import           Preprocessor.Preprocessor
import           Servant                             (Handler)
import           System.Random
import qualified Data.Set as S
import qualified Network.WebSockets                  as WS

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

-- setup: choose a winning door for the car
-- player chooses a door
-- presenter opens another door
-- player chooses to change or not
-- compute payoff

chooseInitialDoor :: OpenGame
                       (MonadOptic IO)
                       (MonadContext IO)
                       '[]
                       '[]
                       ()
                       ()
                       DoorCar
                       ()
chooseInitialDoor = [opengame|
   inputs    :      ;
   feedback  :      ;
   :----------------------------:
   inputs    :     ;
   feedback  :     ;
   operation : nature chooseWinningDoor ;
   outputs   : winningDoor ;
   returns   :     ;
   // Chooses behind which door the car is
   :----------------------------:
   outputs   : winningDoor ;
   returns   :     ;
  |]

playerChooseDoor :: OpenGame
                       (MonadOptic IO)
                       (MonadContext IO)
                       '[Door]      -- a: inputs from interactWithEnv (1 call
                       '[]          -- b?
                       DoorCar      -- x? (winningDoor)
                       ()           -- s
                       Door         -- y: outputs of the overall thing
                       ()           -- r: returns
playerChooseDoor = [opengame|
   inputs    : winningDoor ;
   feedback  :     ;
   :----------------------------:
   inputs    : winningDoor ;
   feedback  : ignoreZeroPayoff ;
   operation : interactWithEnv ;
   outputs   : decision1 ;
   returns   : 0   ;
   // first decision; fix payoff at zero
   :----------------------------:
   outputs   : decision1 ;
   returns   :     ;
  |]


chooseGoatDoor :: OpenGame
                    (MonadOptic IO)
                    (MonadContext IO)
                    '[]
                    '[]
                    (DoorCar, Door)
                    ()
                    DoorGoat
                    ()
chooseGoatDoor = [opengame|
   inputs    : winningDoor,decision1 ;
   feedback  :  ;
   :----------------------------:
   inputs    : winningDoor,decision1 ;
   feedback  :  ;
   operation : liftStochastic (uncurry revealGoatDoor) ;
   outputs   : revealedDoor ;
   returns   :  ;
   // if first decision did not win the prize, reveal one door which contains a goat
   :----------------------------:
   outputs   : revealedDoor ;
   returns   :     ;
  |]


playerChangeChoice :: OpenGame
                        (MonadOptic IO)
                        (MonadContext IO)
                        '[ChangeChoice]
                        '[]
                        (DoorCar, Door, DoorGoat)
                        ()
                        ChangeChoice
                        ()
playerChangeChoice = [opengame|
   inputs    : winningDoor,decision1,revealedDoor ;
   feedback  :  ;
   :----------------------------:
   inputs    : winningDoor,decision1,revealedDoor ;
   feedback  : ignoreZeroPayoff ;
   operation : interactWithEnv ;
   outputs   : decision2 ;
   returns   : 0 ;
   // reverse or keep first choice
   :----------------------------:
   outputs   : decision2 ;
   returns   :     ;
  |]

playerFinalPayoff :: OpenGame
                       (MonadOptic IO)
                       (MonadContext IO)
                       '[]
                       '[]
                       (DoorCar, Door, ChangeChoice)
                       ()
                       Double
                       ()
playerFinalPayoff = [opengame|
   inputs    : winningDoor,decision1,decision2 ;
   feedback  :  ;
   :----------------------------:
   inputs    : winningDoor,decision1,decision2 ;
   feedback  :      ;
   operation : fromFunctions (uncurry3 payoffDecision) id ;
   outputs   : payoff ;
   // compute the payoff
   :----------------------------:
   outputs   : payoff ;
   returns   :     ;
  |]


test :: PlayParameters -> IO PlayResult
test PlayParameters { theDoor, playerAction } = do
  -- Game 0: Choose the door behind which the car is.
  let strategy0 :: List '[]
      strategy0 = Nil

      game0' :: MonadOptic IO () () DoorCar ()
      game0' = play chooseInitialDoor strategy0
  game0 <- extractNextState game0' ()

  -- Game 1: Ask the player for a first door choice (integer).
  let strategy1 :: List '[Door]
      strategy1 = theDoor ::- Nil

      game1' :: MonadOptic IO DoorCar () Door ()
      game1' = play playerChooseDoor strategy1
  game1 <- extractNextState game1' (game0)

  -- Game 2: Choose and open a door hiding a goat.
  let strategy2 :: List '[]
      strategy2 = Nil

      game2' :: MonadOptic IO (DoorCar, Door) () DoorGoat ()
      game2' = play chooseGoatDoor strategy2
  game2 <- extractNextState game2' (game0, game1)

  -- Game 3: Ask the player whether they want to change their choice (boolean).
  let strategy3 :: List '[ChangeChoice]
      strategy3 = playerAction ::- Nil

      game3' :: MonadOptic IO (DoorCar, Door, DoorGoat) () ChangeChoice ()
      game3' = play playerChangeChoice strategy3
  game3 <- extractNextState game3' (game0, game1, game2)

  -- Game 4: Compute the payoff for the player, given (most of) the history.
  let strategy4 :: List '[]
      strategy4 = Nil

      game4' :: MonadOptic IO (DoorCar, Door, ChangeChoice) () Double ()
      game4' = play playerFinalPayoff strategy4
  payoff <- extractNextState game4' (game0, game1, game3) -- NOTE: skipped game2

  return $ PlayResult { playerPayoff = payoff }

switch :: Int -> IO Double
switch n = fmap (/ fromIntegral n) (sum <$> replicateM n (playerPayoff <$> test (PlayParameters { theDoor = 2, playerAction = True })))

dontSwitch :: Int -> IO Double
dontSwitch n = fmap (/ fromIntegral n) (sum <$> replicateM n (playerPayoff <$> test (PlayParameters { theDoor = 2, playerAction = False })))


wsPlay :: PendingConnection -> Handler ()
wsPlay pending = do
  liftIO $ do
    connection <- WS.acceptRequest pending
    handle disconnect . WS.withPingThread connection 10 (pure ()) $ liftIO $ forever $ do

      -- Game 0: Choose the door behind which the car is.
      let strategy0 :: List '[]
          strategy0 = Nil

          game0' :: MonadOptic IO () () DoorCar ()
          game0' = play chooseInitialDoor strategy0
      game0 <- extractNextState game0' ()

      -- Game 1: Ask the player for a first door choice (integer).
      (Just firstDoorChoice) <- decode <$> WS.receiveData @ByteString connection

      let strategy1 :: List '[Door]
          strategy1 = firstDoorChoice ::- Nil

          game1' :: MonadOptic IO DoorCar () Door ()
          game1' = play playerChooseDoor strategy1
      game1 <- extractNextState game1' (game0)

      -- TODO: Ensure that the integer given is in [1..3]

      -- Game 2: Choose and open a door hiding a goat.
      let strategy2 :: List '[]
          strategy2 = Nil

          game2' :: MonadOptic IO (DoorCar, Door) () DoorGoat ()
          game2' = play chooseGoatDoor strategy2
      game2 <- extractNextState game2' (game0, game1)

      -- TODO: Perhaps we should send this information to the python world (obs)

      -- Game 3: Ask the player whether they want to change their choice (boolean).
      (Just changeChoice) <- decode <$> WS.receiveData @ByteString connection

      let strategy3 :: List '[ChangeChoice]
          strategy3 = changeChoice ::- Nil

          game3' :: MonadOptic IO (DoorCar, Door, DoorGoat) () ChangeChoice ()
          game3' = play playerChangeChoice strategy3
      game3 <- extractNextState game3' (game0, game1, game2)

      -- Game 4: Compute the payoff for the player, given (most of) the history.
      let strategy4 :: List '[]
          strategy4 = Nil

          game4' :: MonadOptic IO (DoorCar, Door, ChangeChoice) () Double ()
          game4' = play playerFinalPayoff strategy4

      payoff <- extractNextState game4' (game0, game1, game3) -- NOTE: skipped game2

      WS.sendTextData connection (encode payoff)
  where
    disconnect :: SomeException -> IO ()
    disconnect _ = pure ()

