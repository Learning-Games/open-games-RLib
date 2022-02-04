{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# LANGUAGE EmptyCase, DuplicateRecordFields, TemplateHaskell, QuasiQuotes #-}


import Examples.InteractiveIO.InteractiveIO
import Engine.OpticClass
import Engine.OpenGames
import Engine.InteractiveIO
import Engine.TLL
import Engine.AtomicGames



main = do
  putStrLn "strategy for player 1"
  action1 <- getLine
  putStrLn "strategy for player 2"
  action2 <- getLine
  let strat1 = inputStrat action1
      strat2 = inputStrat action2
      strategyTuple = strat1 ::- strat2 ::- Nil
  generateOutputIO $ evaluate prisonersDilemmaIO strategyTuple void
