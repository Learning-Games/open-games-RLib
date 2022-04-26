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


module Examples.ExternalEnvironment.GenericMontyHall where

import Engine.Engine hiding (fromLens, fromFunctions, state, nature)
import Preprocessor.Preprocessor hiding (game)

import Engine.ExternalEnvironment
import Examples.ExternalEnvironment.Common (extractPayoffAndNextState)

import qualified Data.Map as M
import           System.Random

import Data.Maybe    (fromMaybe)
import Control.Monad (replicateM)


-- A slightly different version of the monty-hall; attempting to be a bit more
-- generic; in particular to think about the 'n-p' based version of monty
-- hall;
--
--  n = number of doors
--  p = the number of doors the host opens, after your first choice.
--
-- TODO:
--  - Break up into two games?!
--  - ???

-------------
-- Data types
type Door         = Int
type ChangeChoice = Bool

data Thing = Goat | Car
  deriving (Eq, Show)

type GameState = M.Map Door Thing


prize :: Double
prize = 10

-- | In the beginning, there is Goat.
initialMonty :: Int -> GameState
initialMonty n = M.fromList $ zip [1..n] (repeat Goat)


-- | Then, we decide where to put the car.
setPrize :: Int -> IO GameState
setPrize n = do
  gen <- newStdGen
  let doorNumber = fst $ randomR (1, n) gen
  return $ M.insert doorNumber Car (initialMonty n)

-- | We win if, our first guess was right and we didn't change.
payoffDecision2 :: GameState
                -> Door -- Our first guess
                -> ChangeChoice -- Our second "guess" (did we change?!)
                -> Double
payoffDecision2 gameState firstGuess changeChoice
  | firstGuessThing == Car && not changeChoice = prize

  -- Wrong! For p != n - 2
  | firstGuessThing /= Car && changeChoice     = prize -- Special condition for n = 3

  -- If 1 < p < n - 2, then
  --
  -- Conceptually: "| newDoorGuess == correctDoor = prize"
  --
  --  1. Stateful: Somehow return a list of valid door choices,
  --      Doors: (2,3,4,5,7,8,9)
  --
  --  2. Not stateful: Pick a number between 1 and n - 2, we'll give you a
  --  door that that corresponds to.
  --
  --  3. Even worse than that:
  --
  --    We'll just pick the first non-opened door by our own ordering!
  --
  | otherwise               = 0
  where
    firstGuessThing = fromMaybe Goat $ M.lookup firstGuess gameState
    -- Assumption: There is only one car.
    -- correctDoor     = head $ M.keys $ M.filter (== Car) gameState

test :: List '[Door, ChangeChoice] -> IO Double
test strat = do
  let a :: MonadOptic IO () Double (GameState) ()
      a = play genericMontyHall strat

  (p, ns)  <- extractPayoffAndNextState a () ()

  return p

  -- where
  -- strat = 1 ::- True ::- 7 ::- Nil -- Wrong!
  --
    -- strat = (1 ::- const False ::- Nil)
    -- strat = (1 ::- (\a -> if a == 0 then True else False) ::- Nil)

     -- np.gym.Discrete( ... )
     --
     -- 1 ::- <%@#$%@$#> ::- Nil
     --         |
     --         Arbitrarily rich.
     --
     -- 1 ::- (w1, w2, w3) ::- Nil


switch, dontSwitch :: Int -> IO Double
switch n       = fmap (/ fromIntegral n) (sum <$> replicateM n (test (2 ::- True  ::- Nil)))
dontSwitch   n = fmap (/ fromIntegral n) (sum <$> replicateM n (test (2 ::- False ::- Nil)))


-- "API" for "Stateful"/"Multi-phase" games
--
-- The "full game" is built from two halves:
--
--  "game" = "game0" . "game1"
--
-- To play a full round of the game:
--
-- do
--   game1State <- extractState $ play game0 (1 ::- Nil)                   -- Door choice
--   game2State <- extractState $ play game1 (game1State ::- True ::- Nil) -- Switch choice
--   finalState <- extractState $ play game2 (game2State ::- 1 ::- Nil)    -- Final door choice


-- ???
-- game'  <- play game strategy1
-- game'' <- play game' strategy2


-- For webserver details/busywork:
-- 1. "Pass-through" - Pass it back to Python to pass it back to us.
--    Pros: No state!
--    Cons: Allows them to lie!
--
-- 2. "Real-life" version; WE maintain OUR own PRIVATE state; you will never
-- know it!
--    Pros: Secure!
--    Cons: Annoying. Identify players; sessions, cookies.


-- Python:
--
--  I'm going to play my game.
--
--    move1 = { ... }
--    game_state = make_move( move1 ) # List of valid doors
--
--    move2 = np.random.choice(game_state.valid_doors)
--
--    make_move( move2 )
--

genericMontyHall :: ExternalEnvironmentGame
                      '[Door, ChangeChoice]
                      '[]
                      ()
                      (Double)
                      (GameState)
                      ()
genericMontyHall = [opengame|

   inputs    :      ;
   feedback  :  payoff    ;

   :----------------------------:
   inputs    :     ;
   feedback  :     ;
   operation : nature (setPrize 3);
   outputs   : game ;
   returns   :     ;
   // Chooses behind which door the car is

   inputs    :     ;
   feedback  : ignoreZeroPayoff ;
   operation : interactWithEnv ;
   outputs   : doorDecision ;
   returns   : 0 ;
   // first decision; fix payoff at zero

   inputs    : ;
   feedback  : payoff ;
   operation : interactWithEnv ;
   outputs   : changeChoice ;
   returns   : payoffDecision2 game doorDecision changeChoice ;
   // reverse or keep first choice; if winning prize else zero

   :----------------------------:

   outputs   : game ;
   returns   :    ;
  |]

