{-# LANGUAGE NamedFieldPuns #-}

module Main where

import           Options.Applicative
import qualified Examples.ExternalEnvironment.PD                as PD
import qualified Examples.ExternalEnvironment.RockPaperScissors as RockPaperScissors
import qualified Examples.ExternalEnvironment.TrustGame         as TrustGame

data Game = PrisonersDilemma
          | TrustGame
          | RockPaperScissors
  deriving (Eq, Show, Read)

data Options = Options
  { port :: Int
  , game :: Game
  }

opts :: Parser Options
opts = Options
        <$> option auto
              ( long "port"
                <> short 'p'
                <> value 3000
                <> metavar "INT"
              )
        <*> option auto
              ( long "game"
              <> metavar "GAME"
              )

main = go =<< execParser p
  where
    p = info (opts <**> helper) fullDesc
    go Options { port, game }  =
      case game of
        PrisonersDilemma  -> PD.run port
        RockPaperScissors -> RockPaperScissors.run port
        TrustGame         -> TrustGame.run port

