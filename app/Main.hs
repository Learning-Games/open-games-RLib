module Main where

import Examples.QLearning.CalvanoReplicate
import Engine.TLL
import Engine.QLearning


main = do
  let results = evalStageLS beta (initiateStrat actionSpace 1) 1000
  let pairLS = fmap toPair results
  print $ drop 990 pairLS
--  print $ fmap extractSnd pairLS


