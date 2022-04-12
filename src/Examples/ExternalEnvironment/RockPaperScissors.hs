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

import Control.Monad.IO.Class   (liftIO)
import Data.Aeson               (ToJSON, FromJSON)
import GHC.Generics             (Generic)
import Network.Wai.Handler.Warp (setPort, setBeforeMainLoop, runSettings, defaultSettings)
import Servant                  ((:>), (:<|>)((:<|>)), Server, Handler, Application
                                , Proxy(..), JSON, Get, Post, ReqBody, serve)

import Engine.ExternalEnvironment
import Examples.ExternalEnvironment.Common (extractPayoff)

-- Types

data ActionRPS = Rock | Paper | Scissors
  deriving (Ord, Eq, Show, Generic, ToJSON, FromJSON)

data PlayParameters = PlayParameters
  { player1Action :: ActionRPS
  , player2Action :: ActionRPS
  } deriving (Show, Generic, ToJSON, FromJSON)

data PlayResult = PlayResult
  { player1Payoff  :: Double
  , player2Payoff  :: Double
  } deriving (Show, Generic, ToJSON, FromJSON)

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

-- Servant API

-- Using: https://httpie.io/docs/cli/json
--
--  http localhost:3000/healthcheck
--  http POST localhost:3000/play < json-data/rps-game.json
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
runPlay PlayParameters { player1Action, player2Action } = do

  let strategy = player1Action ::- player2Action ::- Nil
      next     = play rockPaperScissorsExternal strategy

  -- TODO: Does this need to be in IO?
  (p1, p2) <- liftIO $ extractPayoff next () ()

  let pr = PlayResult { player1Payoff  = p1
                      , player2Payoff  = p2
                      }
  return pr

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

