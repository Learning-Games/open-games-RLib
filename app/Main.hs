{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# LANGUAGE EmptyCase, DuplicateRecordFields, TemplateHaskell, QuasiQuotes #-}


import Examples.InteractiveIO.InteractiveIO
import Engine.OpticClass
import Engine.OpenGames
import Engine.InteractiveIO
import Engine.TLL
import Engine.AtomicGames



main = do
  action1 <- getLine
  action2 <- getLine
  let strat1 = pureAction $ inputStrat action1
      strat2 = pureAction $ inputStrat action2
      strategyTuple = strat1 ::- strat2 ::- Nil
  generateOutputIO $ evaluate prisonersDilemmaIO strategyTuple void
