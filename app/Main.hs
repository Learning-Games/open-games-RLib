module Main where

import Examples.QLearning.Pricing
import Engine.TLL
import Engine.QLearning

main = do
  let results = evalStageLS 40 (initiateStrat 3) 10
  let pairLS = fmap toPair results
  print $ head pairLS
  print $ drop 1 $ fmap extractSnd pairLS


