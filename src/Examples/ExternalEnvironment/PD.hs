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
{-# OPTIONS_GHC -fno-warn-unused-matches #-} -- due to the code generated for prisonersDilemmaExternal

module Examples.ExternalEnvironment.PD where

import           Control.Exception                   (SomeException, handle)
import           Control.Monad                       (forever)
import           Control.Monad.IO.Class              (liftIO)
import           Data.Aeson                          (ToJSON, FromJSON, encode, decode)
import           Data.ByteString.Lazy.Internal       (ByteString)
import           Engine.Engine hiding                (fromLens, fromFunctions, state)
import           Engine.ExternalEnvironment          (ExternalEnvironmentGame, fromFunctions, fromLens, interactWithEnv)
import           Examples.ExternalEnvironment.Common (extractPayoffAndNextState)
import           Examples.SimultaneousMoves          (prisonersDilemmaMatrix,ActionPD(..))
import           GHC.Generics                        (Generic)
import           Network.WebSockets.Connection       (PendingConnection)
import           Preprocessor.Preprocessor
import           Servant                             (Handler)
import qualified Network.WebSockets                  as WS

data PlayParameters = PlayParameters
  { player1Action :: ActionPD
  , player2Action :: ActionPD
  } deriving (Show, Generic, ToJSON, FromJSON)

data PlayResult = PlayResult
  { player1Payoff  :: Double
  , player2Payoff  :: Double
  } deriving (Show, Generic, ToJSON, FromJSON)

deriving instance Generic  ActionPD

-- Orphans
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
      ((p1, p2), _) <- extractPayoffAndNextState nextgame () ()

      -- Send the payoffs back to the client
      let playResult = PlayResult { player1Payoff = p1, player2Payoff = p2 }
      let response = encode playResult
      WS.sendTextData connection response
  where
    disconnect :: SomeException -> IO ()
    disconnect _ = pure ()


prisonersDilemmaExternal :: ExternalEnvironmentGame
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

