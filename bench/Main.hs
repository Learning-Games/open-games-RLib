module Main where

import Criterion
import Criterion.Main
import Engine.QLearning
import Engine.TLL
import Examples.QLearning.BestReply

main =
  defaultMain
    [ bench
      ("iters: " ++ show i)
      (nf
         (\iters ->
            let results = evalStageLS (initiateStrat actionSpace 6) iters
             in let pairLS = fmap toPair results
                 in last $ pairLS)
         i)
    | i <- [1, 10, 100, 1000, 10000]
    ]

{-
main = do
  let results = evalStageLS initiateStrat 100
  let pairLS = fmap toPair results
  print $ head pairLS
--  print $ drop 299900 $ pairLS
  print $ take 100 $ fmap  extractSnd pairLS
  print $ drop 99900 $ fmap extractSnd pairLS
--}
