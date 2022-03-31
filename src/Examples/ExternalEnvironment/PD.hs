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
import Examples.SimultaneousMoves (prisonersDilemmaMatrix,ActionPD(..))

import Servant                  ((:>), (:<|>)((:<|>)), Server, Handler, Application
                                , Proxy(..), JSON, Get, Post, ReqBody, serve)
import Control.Monad.IO.Class   (liftIO)
import Data.Aeson               (ToJSON, FromJSON)
import GHC.Generics             (Generic)
import Network.Wai.Handler.Warp (setPort, setBeforeMainLoop, runSettings, defaultSettings)

data PlayParameters = PlayParameters
  { player1Action :: ActionPD
  , player2Action :: ActionPD
  , startingState :: State
  } deriving (Show, Generic, ToJSON, FromJSON)


data State = State
  { totalPlayer1 :: Double
  , totalPlayer2 :: Double
  } deriving (Show, Generic, ToJSON, FromJSON)

data PlayResult = PlayResult
  { lastPlayer1Payoff  :: Double
  , lastPlayer2Payoff  :: Double
  , gameState          :: State
  } deriving (Show, Generic, ToJSON, FromJSON)

-- Using: https://httpie.io/docs/cli/json
--
--  http localhost:3000/healthcheck
--  http POST localhost:3000/play < json-data/pd-game.json
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

deriving instance Generic  ActionPD
instance ToJSON   ActionPD
instance FromJSON ActionPD

runPlay :: PlayParameters -> Handler PlayResult
runPlay PlayParameters { player1Action, player2Action, startingState } = do

  let strategy = player1Action ::- player2Action ::- Nil
      next     = play prisonersDilemmaExternal strategy

  -- TODO: Does this need to be in IO?
  (p1, p2) <- liftIO $ extractPayoff next

  -- HACK: We manage state by making the client do it (!)
  --
  -- In general, we'd probably like the state to be tracked by the MonadOptic
  -- machinery; we think, however, that this is enough to demonstrate a
  -- multi-round prisoners-dilemma via RLib; hopefully the essence of this
  -- side stays the same; namely extract the state from the result of `play`.
  let newState = State { totalPlayer1 = totalPlayer1 startingState + p1
                       , totalPlayer2 = totalPlayer2 startingState + p2
                       }

  let pr = PlayResult { lastPlayer1Payoff  = p1
                      , lastPlayer2Payoff  = p2
                      , gameState          = newState
                      }

  return pr

strat :: List '[ActionPD, ActionPD]
strat = Cooperate ::- Cooperate ::- Nil

test :: MonadOptic IO () (Double, Double) (ActionPD, ActionPD) ()
test = play prisonersDilemmaExternal strat




{-- Usage:
extractPayoff test

extractNextState test
-}

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


-- extract continuation
-- extractPayoff :: MonadOptic IO () (Double,Double) (ActionPD,ActionPD) () -> IO (Double,Double)
extractPayoff :: MonadOptic m () t a () -> m t
extractPayoff (MonadOptic v u) = do
  (z,_) <- v ()
  u z ()


-- extract next state (action)
-- extractNextState :: MonadOptic IO () (Double,Double) (ActionPD,ActionPD) () -> IO (ActionPD,ActionPD)
extractNextState :: MonadOptic m () t a b -> m a
extractNextState (MonadOptic v _) = do
  (_,a) <- v ()
  pure a




