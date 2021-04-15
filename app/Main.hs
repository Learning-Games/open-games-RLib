module Main where

import Examples.QLearning.PDDeterministicPlayer2
import Engine.TLL
import Engine.QLearning


main = do
  let results = evalStageLS initiateStrat 300000
  let pairLS = fmap toPair results
  print $ head pairLS
  print $ drop 299900 $ pairLS

