module Main where

import Examples.QLearning.PDDeterministicPlayer
import Engine.TLL
import Engine.QLearning


main = do
  let results = evalStageLS (initiateStrat) 1000
  let pairLS = fmap toPair results
  print pairLS
--  print $ fmap extractSnd pairLS


