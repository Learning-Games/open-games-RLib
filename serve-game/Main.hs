{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Main where

import           Options.Applicative
import           Servant                  ((:>), (:<|>)((:<|>)), Server, Application, Proxy(..), JSON, Get, serve)
import           Network.Wai.Handler.Warp (setPort, setBeforeMainLoop, runSettings, defaultSettings)
import           Servant.API.WebSocket    (WebSocketPending)

import qualified Examples.ExternalEnvironment.PD                as PD
import qualified Examples.ExternalEnvironment.RockPaperScissors as RockPaperScissors
import qualified Examples.ExternalEnvironment.TrustGame         as TrustGame
import qualified Examples.ExternalEnvironment.MontyHall         as MontyHall

type Api
  = "prisoners-dilemma" :> "play" :> WebSocketPending
  :<|> "trust-game" :> "play" :> WebSocketPending
  :<|> "rock-paper-scissors" :> "play" :> WebSocketPending
  :<|> "simple-monty-hall" :> "play" :> WebSocketPending
  :<|> "healthcheck" :> Get '[JSON] String

api :: Proxy Api
api = Proxy

server :: Server Api
server
  =    PD.wsPlay
  :<|> TrustGame.wsPlay
  :<|> RockPaperScissors.wsPlay
  :<|> MontyHall.wsPlay
  :<|> return "Ok!"

mkApp :: IO Application
mkApp = return $ serve api server

run :: Int -> IO ()
run port = do
  let settings =
        setPort port $
          setBeforeMainLoop (putStrLn ("Listening on port " ++ show port)) $
            defaultSettings
  runSettings settings =<< mkApp

data Options = Options
  { port :: Int
  }

opts :: Parser Options
opts = Options
        <$> option auto
              ( long "port"
                <> short 'p'
                <> value 3000
                <> metavar "INT"
              )

main :: IO ()
main = go =<< execParser p
  where
    p = info (opts <**> helper) fullDesc
    go Options { port }  = run port
