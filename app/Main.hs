module Main where

import Examples.QLearning.BestReply
import Engine.TLL
import Engine.QLearning


main = do
  let results = evalStageLS (initiateStrat actionSpace 6) 100
  let pairLS = fmap toPair results
  print $ pairLS

{-
main = do
  let results = evalStageLS initiateStrat 100
  let pairLS = fmap toPair results
  print $ head pairLS
--  print $ drop 299900 $ pairLS
  print $ take 100 $ fmap  extractSnd pairLS
  print $ drop 99900 $ fmap extractSnd pairLS
--}
