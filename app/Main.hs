{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}

import Examples.QLearning.AsymmetricLearners.MarkovInternal
import Engine.QLearning.ImportAsymmetricLearners (Action)
import Examples.QLearning.AsymmetricLearners3Phases (Parameters(..),actionSpace1,actionSpace2,randomInitialObservation)
import Engine.QLearning (CTable(..))

import qualified Data.ByteString.Lazy as L -- FIXME
import System.Random



main :: IO ()
main = do
  print "test docker execution"
  L.writeFile "testFile.txt" "1,2,3"

