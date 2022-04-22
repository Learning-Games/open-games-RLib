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


module Examples.ExternalEnvironment.PD where


import Engine.Engine hiding (fromLens, fromFunctions, state)
import Preprocessor.Preprocessor

import Engine.ExternalEnvironment
import Examples.ExternalEnvironment.Common (extractPayoff)
import Examples.SimultaneousMoves (prisonersDilemmaMatrix,ActionPD(..))

import Servant                  ((:>), (:<|>)((:<|>)), Server, Handler, Application
                                , Proxy(..), JSON, Get, serve)
import Control.Monad.IO.Class   (liftIO)
import Data.Aeson               (ToJSON, FromJSON, encode, decode)
import GHC.Generics             (Generic)
import Network.Wai.Handler.Warp (setPort, setBeforeMainLoop, runSettings, defaultSettings)

import Servant.API.WebSocket    (WebSocketPending)
import Network.WebSockets.Connection (PendingConnection)
import qualified Network.WebSockets as WS
import Control.Exception (SomeException, handle)
import Data.ByteString.Lazy.Internal (ByteString)
import Control.Monad (forever)

data PlayParameters = PlayParameters
  { player1Action :: ActionPD
  , player2Action :: ActionPD
  } deriving (Show, Generic, ToJSON, FromJSON)

data PlayResult = PlayResult
  { player1Payoff  :: Double
  , player2Payoff  :: Double
  } deriving (Show, Generic, ToJSON, FromJSON)

-- Using: https://httpie.io/docs/cli/json
--
--  http localhost:3000/healthcheck
--  http POST localhost:3000/play < json-data/pd-game.json
--
-- Using 'websocat':
--  websocat ws://localhost:3000/play

type Api = "play" :> WebSocketPending
           :<|> "healthcheck" :> Get '[JSON] String

api :: Proxy Api
api = Proxy

server :: Server Api
server =
  wsPlay
  :<|> return "Ok!"

run :: Int -> IO ()
run port = do
  let settings =
        setPort port $
          setBeforeMainLoop (putStrLn ("Listening on port " ++ show port)) $
            defaultSettings
  runSettings settings =<< mkApp

mkApp :: IO Application
mkApp = return $ serve api server

deriving instance Generic  ActionPD
instance ToJSON   ActionPD
instance FromJSON ActionPD

wsPlay :: PendingConnection -> Handler ()
wsPlay pending = do
  liftIO $ do
    connection <- WS.acceptRequest pending
    handle disconnect . WS.withPingThread connection 10 (pure ()) $ liftIO $ forever $ do
      -- Receive and decode the action of each player
      -- TODO: Fix this partial pattern match here.
      playParameters <- WS.receiveData @ByteString connection
      let Just (PlayParameters { player1Action, player2Action }) = decode playParameters

      -- Play the game to compute the payoffs
      let strategy = player1Action ::- player2Action ::- Nil
          nextgame = play prisonersDilemmaExternal strategy
      (p1, p2) <- extractPayoff nextgame () ()

      -- Send the payoffs back to the client
      let playResult = PlayResult { player1Payoff = p1, player2Payoff = p2 }
      let response = encode playResult
      WS.sendTextData connection response
  where
    disconnect :: SomeException -> IO ()
    disconnect _ = putStrLn "Disconnecting.." >> pure ()


prisonersDilemmaExternal :: OpenGame
                                    (MonadOptic IO)
                                    (MonadContext IO)
                                    '[ActionPD, ActionPD]
                                    '[]
                                    ()
                                    (Double, Double)
                                    (ActionPD, ActionPD)
                                    ()
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

