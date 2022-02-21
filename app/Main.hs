{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}

import Examples.QLearning.AsymmetricLearners.MarkovInternal
import Engine.QLearning.ImportAsymmetricLearners (Action)
import Examples.QLearning.AsymmetricLearners3Phases (Parameters(..),actionSpace1,actionSpace2,randomInitialObservation)
import Engine.QLearning (CTable(..))

import qualified Data.ByteString.Lazy as L -- FIXME
import System.Random

sourcePath :: Int -> [FilePath]
sourcePath exp = fmap (\runNo -> "experiment/e" ++ (show exp) ++ "_phase1_run_" ++ (show runNo) ++ "/") [1]

-- path to qmatrix for a given experiment
pathQMatrix exp = fmap (\x -> x ++ "qvalues.csv") (sourcePath exp)



main :: IO ()
main = do
  print "test docker execution"
  L.writeFile "testFile.txt" "1,2,3"
  content <- L.readFile $ head $ pathQMatrix 11
  print content

