{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}

module Spec where

import           Data.Array.IO as A
import qualified Data.Vector as V
import qualified Data.Vector.Sized as SV
import           Data.List (minimumBy,maximumBy)
import           RIO hiding (preview)
import           System.Random
import           Test.Hspec
import           Test.QuickCheck

import Engine.QLearning
import Examples.QLearning.CalvanoReplication.Internal
import qualified Engine.Memory as Memory

-- TODO Start with fixed values and then extend to more general, arbitrary values
-- TODO Start with Calvano example and the test functionality more generally

-------------
-- 0. Types




---------------------
-- 1. Top-level file
-- Uses just two actions for simplicity
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
          , pM  = 1 -- NOTE: Due to the construction, we need to take the orginial value of Calvano and take -1
          , pGeneratorEnv1 = gEnv1
          , pGeneratorEnv2 = gEnv2
          , pGeneratorPrice1 = gPrice1
          , pGeneratorPrice2 = gPrice2
          , pGeneratorObs1 = gObs1
          , pGeneratorObs2 = gObs2
          }
  hspec $ do spec1 parametersTest
             spec2 parametersTest
             spec3 parametersTest
             --(spec4 parametersTest gObs1 gObs2)



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

spec3 par = describe
  "highestIndex" $ do
    it "checks the lowest index is correct" $ do
      shouldBe
          (maximum $ fmap idx $ population $ actionSpace par)
          1
{-
spec4 :: Parameters -> StdGen -> StdGen -> SpecWith ()
spec4 par gen1 gen2 = describe
   "maxScore" $ do
     it "checks that the highest score is generated" $ do
       arr <- initialArray par
       maxed <- maxScore (Memory.fromSV (SV.replicate (fmap toIdx initialObs))) arr (actionSpace par) 1
       shouldBe
          maxed
          maxLs
   where
    initialObs =
       Obs (samplePopulation_ (actionSpace par) gen1, samplePopulation_ (actionSpace par) gen2)
    filteredLs = [(w,z) |(((x,y),z),w) <- lsValues par, (x,y) == unObs initialObs]
    maxLs = maximumBy (comparing fst) filteredLs
    initialArray :: Parameters -> IO (QTable Player1N Observation PriceSpace)
    initialArray par = do
              arr <- newArray_ (asIdx l, asIdx u)
              traverse_ (\(k, v) -> writeArray arr ( (asIdx k)) v) lsValues'
              pure arr
              where
                lsValues' = lsValues par
                l = minimum $ fmap fst lsValues'
                u = maximum $ fmap fst lsValues'
                asIdx ((x, y), z) = (Memory.fromSV (SV.replicate (Obs (toIdx x, toIdx y))), toIdx z)


-}
