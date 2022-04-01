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
                                , Proxy(..), JSON, Get, Post, ReqBody, serve)
import Control.Monad.IO.Class   (liftIO)
import Data.Aeson               (ToJSON, FromJSON)
import GHC.Generics             (Generic)
import Network.Wai.Handler.Warp (setPort, setBeforeMainLoop, runSettings, defaultSettings)

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
runPlay PlayParameters { player1Action, player2Action } = do

  let strategy = player1Action ::- player2Action ::- Nil
      next     = play prisonersDilemmaExternal strategy

  -- TODO: Does this need to be in IO?
  (p1, p2) <- liftIO $ extractPayoff next

  let pr = PlayResult { player1Payoff  = p1
                      , player2Payoff  = p2
                      }
  return pr


{-- Usage:

-- extractPayoff :: MonadOptic IO () (Double,Double) (ActionPD,ActionPD) () -> IO (Double,Double)
-- extractNextState :: MonadOptic IO () (Double,Double) (ActionPD,ActionPD) () -> IO (ActionPD,ActionPD)

strat :: List '[ActionPD, ActionPD]
strat = Cooperate ::- Cooperate ::- Nil

test :: MonadOptic IO () (Double, Double) (ActionPD, ActionPD) ()
test = play prisonersDilemmaExternal strat

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

