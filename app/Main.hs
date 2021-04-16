module Main where

import Examples.QLearning.PDDeterministicPlayer2
import Engine.TLL
import Engine.QLearning2


main = do
  let results = evalStageLS initiateStrat 100000
  let pairLS = fmap toPair results
  print $ head pairLS
--  print $ drop 299900 $ pairLS
  print $ take 100 $ fmap  extractSnd pairLS
  print $ drop 99900 $ fmap extractSnd pairLS
