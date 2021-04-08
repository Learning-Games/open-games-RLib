module Main where

import Examples.QLearning.Pricing
import Engine.TLL
import Engine.QLearning


main = do
  let results = evalStageLS (initiateStrat) 1000000
  let pairLS = fmap toPair results
  print $ head pairLS
  print $ drop 999000 $ fmap extractSnd pairLS


