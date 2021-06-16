{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Spec where

import qualified Data.Vector as V
import qualified Data.Vector.Sized as SV
import           RIO hiding (preview)
import           System.Random
import           Test.Hspec

import Engine.QLearning
import Examples.QLearning.CalvanoReplication.Internal

-- TODO Start with fixed values and then extend to more general values
-- TODO Start with Calvano example and the test functionality more generally

-------------
-- 0. Types




---------------------
-- 1. Top-level file
main :: IO ()
main = do
  gEnv1   <- newStdGen
  gEnv2   <- newStdGen
  gPrice1 <- newStdGen
  gPrice2 <- newStdGen
  gObs1   <- newStdGen
  gObs2   <- newStdGen
  let parametersTest =
        Parameters
          { pKsi = 0.1
          , pBeta = (- 0.00001)
          , pBertrandPrice = 1.47
          , pMonopolyPrice = 1.92
          , pGamma = 0.95
          , pLearningRate = 0.15
          , pMu = 0.25
          , pA1 = 2
          , pA2 = 2
          , pA0 = 0
          , pC1 = 1
          , pM  = 14 -- NOTE: Due to the construction, we need to take the orginial value of Calvano and take -1
          , pGeneratorEnv1 = gEnv1
          , pGeneratorEnv2 = gEnv2
          , pGeneratorPrice1 = gPrice1
          , pGeneratorPrice2 = gPrice2
          , pGeneratorObs1 = gObs1
          , pGeneratorObs2 = gObs2
          }
  hspec $ do spec1 parametersTest
             spec2 parametersTest



spec1 par = describe
  "actionSpace" $ do
    it "computes the correct action values?" $ do
        shouldBe
            (fmap value $ population $ actionSpace par)
            (V.fromList [lowerBound par,lowerBound par + dist par .. upperBound par])

spec2 par = describe
  "lowestIndex" $ do
    it "checks the lowest index is correct" $ do
      shouldBe
          (minimum $ fmap idx $ population $ actionSpace par)
          0



