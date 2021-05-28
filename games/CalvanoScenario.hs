{-# LANGUAGE ScopedTypeVariables #-}
module CalvanoScenario where

import Examples.QLearning.CalvanoReplication
import Engine.TLL
import Engine.QLearning

import qualified Data.ByteString.Lazy as BS
import qualified Data.Aeson.Encoding.Internal as E


main = do
  BS.writeFile "parameters.csv" $ csvParameters
  strat <- initialStrat >>= sequenceL
  results <- evalStageM strat 2
  encoding <- exportQValuesJSON  results
  BS.writeFile "qValues.json" $ E.encodingToLazyByteString encoding
  putStrLn "output completed"
{-
main = do
  let results = evalStageLS initiateStrat 100
  let pairLS = fmap toPair results
  print $ head pairLS
--  print $ drop 299900 $ pairLS
  print $ take 100 $ fmap  extractSnd pairLS
  print $ drop 99900 $ fmap extractSnd pairLS
  let pairLS = fmap toPair results
  print $ last pairLS
--}