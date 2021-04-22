module Main where

import Examples.QLearning.CalvanoReplication
import Engine.TLL
import Engine.QLearning

import qualified Data.ByteString.Lazy as BS
import qualified Data.Aeson.Encoding.Internal as E


main = do
  BS.writeFile "parameters.csv" $ csvParameters
  let results = evalStageLS initialStrat 10
  BS.writeFile "qValues.json" $ E.encodingToLazyByteString $ exportQValuesJSON results
  let pairLS = fmap toPair results
  print $ last pairLS

{-
main = do
  let results = evalStageLS initiateStrat 100
  let pairLS = fmap toPair results
  print $ head pairLS
--  print $ drop 299900 $ pairLS
  print $ take 100 $ fmap  extractSnd pairLS
  print $ drop 99900 $ fmap extractSnd pairLS
--}
