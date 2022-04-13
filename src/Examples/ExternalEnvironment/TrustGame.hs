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

import Engine.Engine hiding (fromLens, fromFunctions, state, nature)
import Preprocessor.Preprocessor

import Engine.ExternalEnvironment
import Examples.SequentialMoves (trustGamePayoffProposer, trustGamePayoffResponder, Pie, Factor, Sent, SentBack)

import           Data.Tuple.Extra (uncurry3)

import Examples.ExternalEnvironment.Common (extractNextState)

import Servant                  ((:>), (:<|>)((:<|>)), Server, Handler, Application
                                , Proxy(..), JSON, Get, Post, ReqBody, serve)
import Control.Monad.IO.Class   (liftIO)
import Data.Aeson               (ToJSON, FromJSON)
import GHC.Generics             (Generic)
import Network.Wai.Handler.Warp (setPort, setBeforeMainLoop, runSettings, defaultSettings)
import Servant.API.WebSocket    (WebSocketPending)
import Network.WebSockets.Connection (Connection, PendingConnection)
import qualified Network.WebSockets as WS
import Data.Text (pack, Text)
import Control.Exception (SomeException, handle)

-- NOTE: I think that "pie" is supposed to capture the total amount of money
--   that player 1 has. However, observe that it is not used within "trustGame"
--   below at all.

type Api = "play" :> WebSocketPending
           :<|> "healthcheck" :> Get '[JSON] String

api :: Proxy Api
api = Proxy

server :: Server Api
server =
  wsPlay
  :<|> return "Ok!"


-- Using 'websocat':
--  websocat ws://localhost:3000/play
--
wsPlay :: PendingConnection -> Handler ()
wsPlay pending = do
  liftIO $ do
    connection <- WS.acceptRequest pending
    handle disconnect . WS.withPingThread connection 10 (pure ()) $ liftIO $ do
      WS.sendTextData connection (pack "Hello")
      you <- WS.receiveData @Text connection
      WS.sendTextData connection $ pack "You said: " <> you
      WS.sendTextData connection $ pack "Goodbye."
      pure ()
    pure ()
  where
    disconnect :: SomeException -> IO ()
    disconnect _ = pure ()


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

-- 1.1. Trust Game
trustGame :: p
             -> Factor
             -> OpenGame
                  (MonadOptic IO)
                  (MonadContext IO)
                  '[Double, Double]
                  '[]
                  ()
                  (Double, Double)
                  (Double, Double)
                  ()
trustGame pie factor = [opengame|

   inputs    :      ;
   feedback  :  (payoff1,payoff2)  ;

   :----------------------------:
   inputs    :   ;
   feedback  : payoff1     ;
   operation : interactWithEnv ;
   outputs   : sent ;
   returns   : trustGamePayoffProposer factor sent sentBack; // TODO: should factor be pie here?

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

proposerDecision :: OpenGame
                      (MonadOptic IO)
                      (MonadContext IO)
                      '[Double]
                      '[]
                      Pie
                      ()
                      Sent
                      Double -- payoffProposer
proposerDecision = [opengame|
   inputs    : pie ;
   feedback  :   ;

   :----------------------------:
   inputs    : pie ;
   feedback  : ignorePayoff    ;
   operation : interactWithEnv ;
   outputs   : sent ;
   returns   : payoffProposer    ;

   :----------------------------:

   outputs   :  sent        ;
   returns   :  payoffProposer    ;
  |]


responderDecision :: OpenGame
                       (MonadOptic IO)
                       (MonadContext IO)
                       '[Double]
                       '[]
                       Double
                       ()
                       Double
                       Double
responderDecision = [opengame|
   inputs    : sent     ;
   feedback  :   ;

   :----------------------------:
   inputs    : sent  ;
   feedback  : ignorePayoff     ;
   operation : interactWithEnv ;
   outputs   : sentBack ;
   returns   : payoffResponder ;

   :----------------------------:

   outputs   :  sentBack         ;
   returns   :  payoffResponder  ;
   |]


proposerPayoff :: OpenGame
                    (MonadOptic IO)
                    (MonadContext IO)
                    '[]
                    '[]
                    (Pie, Sent, SentBack)
                    ()
                    Payoff
                    ()
proposerPayoff = [opengame|
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

responderPayoff :: OpenGame
                     (MonadOptic IO)
                     (MonadContext IO)
                     '[]
                     '[]
                     (Factor, Sent, SentBack)
                     ()
                     Payoff
                     ()
responderPayoff = [opengame|
   inputs    : factor, sent, sentBack    ;
   feedback  :   ;

   :----------------------------:
   inputs    : factor, sent, sentBack  ;
   feedback  :      ;
   operation : fromFunctions (uncurry3 trustGamePayoffResponder) id;
   outputs   : payoffResponder;
   returns   :  ;

   :----------------------------:

   outputs   :  payoffResponder    ;
   returns   :      ;
  |]


{-


--- Control flow now:

-- 0 extractNextState from proposerDecision -> _sent_
-- 1 extractNextState from responderDecision ->  _sentBack_
-- 2 responderDecision produces _payoff2_ ->
-- 3 senderDecision produces _payoff1_


testSender = senderDecision (3 :: Double)

testResponder = responderDecision (3 :: Double)
--}


-- Trustless
--   I give you 0
--   you get 0
--   you give me back 0
--
-- Trusty
--   I give you X
--   you get 3X             (-2x for the manager)
--   you give me back 2X    (+x p1, +x p2)

-- issue: in proposerDecision: unenforced logic: 0 <= sent <= pie
-- issue: sendBack is unrestricted; it doesn't even depend on factor currently
-- 0 <= sentBack <= factor * sent

-- e.g. (test 100 10 6 3)

test :: Pie -> Sent -> SentBack -> Factor -> IO (Sent, SentBack, Payoff, Payoff)
test pie sentInput sentBackInput factor = do
  -- GAME 1
  let strategy1 :: List '[Double]
      strategy1 = sentInput ::- Nil

      game1 :: MonadOptic IO Pie () Double Double
      game1 = play proposerDecision strategy1
  sent <- extractNextState game1 pie

  -- GAME 2
  let strategy2 :: List '[Double]
      strategy2 = sentBackInput ::- Nil

      game2 :: MonadOptic IO Double () Double Double
      game2 = play responderDecision strategy2
  sentBack <- extractNextState game2 sent

  -- GAME 3
  let game3 :: MonadOptic IO (Pie, Sent, SentBack) () Double ()
      game3 = play proposerPayoff Nil
  payoff1 <- extractNextState game3 (pie, sent, sentBack)

  -- GAME 4
  let game4 :: MonadOptic IO (Factor, Sent, SentBack) () Double ()
      game4 = play responderPayoff Nil
  payoff2 <- extractNextState game4 (factor, sent, sentBack)

  return (sent, sentBack, payoff1, payoff2)



-- proposerEval :: IO Double
-- proposerEval = extractNextState (play proposerDecision (10 {- sent -} ::- Nil)) () -- 10
--                                                                      -- ^ this is the s input in the extractState, the _pie_ in the _proposerGame_
-- responderEval :: IO Double
-- responderEval = extractNextState (play responderDecision (5 {- sentBack -} ::- Nil)) 5 {- sent -}
--
-- proposerPayoffEval :: IO Payoff
-- proposerPayoffEval = extractNextState (play proposerPayoff Nil) (10,10,5)
--
-- responderPayoffEval :: Factor -> IO Payoff
-- responderPayoffEval factor = extractNextState (play (responderPayoff factor) Nil) (10,5)
