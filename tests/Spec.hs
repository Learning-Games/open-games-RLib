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
import           Data.List (maximumBy)
import           RIO hiding (preview)
import           System.Random
import           Test.Hspec

import Engine.QLearning
import qualified Examples.QLearning.AsymmetricPrices.Internal as AP
import qualified Examples.QLearning.CalvanoReplication.Internal as C
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
  let parametersTestCalvano =
        C.Parameters
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
      parametersTestAsymmetricPrices =
         AP.Parameters
          { pKsi = 0.1
          , pBeta = (- 0.00001)
          , pBertrandPrice1 = 1.47
          , pBertrandPrice2 = 1.47
          , pMonopolyPrice1 = 1.92
          , pMonopolyPrice2 = 1.92
          , pGamma = 0.95
          , pLearningRate = 0.15
          , pMu = 0.25
          , pA1 = 2
          , pA2 = 2
          , pA0 = 0
          , pC1 = 1
          , pC2 = 1
          , pM1 = 1 -- NOTE: Due to the construction, we need to take the orginial value of Calvano and take -1
          , pM2 = 1 -- NOTE: Due to the construction, we need to take the orginial value of Calvano and take -1
          , pGeneratorEnv1 = gEnv1
          , pGeneratorEnv2 = gEnv2
          , pGeneratorPrice1 = gPrice1
          , pGeneratorPrice2 = gPrice2
          , pGeneratorObs1 = gObs1
          , pGeneratorObs2 = gObs2
          }

  hspec $ do spec1 parametersTestCalvano
             spec2 parametersTestCalvano
             spec3 parametersTestCalvano
             (spec4 parametersTestCalvano gObs1 gObs2)
             spec5 parametersTestAsymmetricPrices
             spec6 parametersTestAsymmetricPrices
             spec7 parametersTestAsymmetricPrices
             spec8 parametersTestAsymmetricPrices
             spec9 parametersTestAsymmetricPrices
             spec10 parametersTestAsymmetricPrices
             (spec11 parametersTestAsymmetricPrices gObs1 gObs2)
             (spec12 parametersTestAsymmetricPrices gObs1 gObs2)

spec1 par = describe
  "actionSpace" $ do
    it "computes the correct action values?" $ do
        shouldBe
            (fmap C.value $ population $ C.actionSpace par)
            (V.fromList [C.lowerBound par,C.lowerBound par + C.dist par .. C.upperBound par])

spec2 par = describe
  "lowestIndex" $ do
    it "checks the lowest index is correct" $ do
      shouldBe
          (minimum $ fmap C.idx $ population $ C.actionSpace par)
          0

spec3 par = describe
  "highestIndex" $ do
    it "checks the lowest index is correct" $ do
      shouldBe
          (maximum $ fmap C.idx $ population $ C.actionSpace par)
          1
spec4 :: C.Parameters -> StdGen -> StdGen -> SpecWith ()
spec4 par gen1 gen2 = describe
   "maxScore" $ do
     it "checks that the highest score is generated" $ do
       arr <- initialArray par 
       maxed <- maxScore (Memory.fromSV (SV.replicate (fmap toIdx initialObs))) arr (C.actionSpace par)
       shouldBe
          maxed
          maxLs
   where
    initialObs =
       C.Obs (samplePopulation_ (C.actionSpace par) gen1, samplePopulation_ (C.actionSpace par) gen2)
    filteredLs = [(w,z) |(((x,y),z),w) <- C.lsValues par, (x,y) == C.unObs initialObs]
    maxLs = maximumBy (comparing fst) filteredLs
    initialArray :: C.Parameters -> IO (QTable C.Player1N C.Observation C.PriceSpace)
    initialArray par = do
              arr <- newArray_ (asIdx l, asIdx u)
              traverse_ (\(k, v) -> writeArray arr ( (asIdx k)) v) lsValues'
              pure arr
              where
                lsValues' = C.lsValues par
                l = minimum $ fmap fst lsValues'
                u = maximum $ fmap fst lsValues'
                asIdx ((x, y), z) = (Memory.fromSV (SV.replicate (C.Obs (toIdx x, toIdx y))), toIdx z)

spec5 par = describe
  "actionSpace1" $ do
    it "computes the correct action values for player 1?" $ do
        shouldBe
            (fmap AP.value $ population $ AP.actionSpace1 par)
            (V.fromList [AP.lowerBound1 par,AP.lowerBound1 par + AP.dist1 par .. AP.upperBound1 par])


spec6 par = describe
  "actionSpace2" $ do
    it "computes the correct action values for player 2?" $ do
        shouldBe
            (fmap AP.value $ population $ AP.actionSpace2 par)
            (V.fromList [AP.lowerBound2 par,AP.lowerBound2 par + AP.dist2 par .. AP.upperBound2 par])

spec7 par = describe
  "lowestIndex1" $ do
    it "checks the lowest index is correct for player 1" $ do
      shouldBe
          (minimum $ fmap AP.idx $ population $ AP.actionSpace1 par)
          0

spec8 par = describe
  "lowestIndex2" $ do
    it "checks the lowest index is correct for player 2" $ do
      shouldBe
          (minimum $ fmap AP.idx $ population $ AP.actionSpace2 par)
          0

spec9 par = describe
  "highestIndex 1" $ do
    it "checks the lowest index is correct for player 1" $ do
      shouldBe
          (maximum $ fmap AP.idx $ population $ AP.actionSpace1 par)
          1

spec10 par = describe
  "highestIndex 2" $ do
    it "checks the lowest index is correct for player 2" $ do
      shouldBe
          (maximum $ fmap AP.idx $ population $ AP.actionSpace2 par)
         1 

spec11 :: AP.Parameters -> StdGen -> StdGen -> SpecWith ()
spec11 par gen1 gen2 = describe
   "maxScore1" $ do
     it "checks that the highest score is generated for player 1" $ do
       arr <- initialArray par 
       maxed <- maxScore (Memory.fromSV (SV.replicate (fmap toIdx initialObs))) arr (AP.actionSpace1 par)
       shouldBe
          maxed
          maxLs
   where
    initialObs =
       AP.Obs (samplePopulation_ (AP.actionSpace1 par) gen1, samplePopulation_ (AP.actionSpace2 par) gen2)
    filteredLs = [(w,z) |(((x,y),z),w) <- AP.lsValues1 par, (x,y) == AP.unObs initialObs]
    maxLs = maximumBy (comparing fst) filteredLs
    initialArray :: AP.Parameters -> IO (QTable AP.Player1N AP.Observation AP.PriceSpace)
    initialArray par = do
              arr <- newArray_ (asIdx l, asIdx u)
              traverse_ (\(k, v) -> writeArray arr ( (asIdx k)) v) lsValues'
              pure arr
              where
                lsValues' = AP.lsValues1 par
                l = minimum $ fmap fst lsValues'
                u = maximum $ fmap fst lsValues'
                asIdx ((x, y), z) = (Memory.fromSV (SV.replicate (AP.Obs (toIdx x, toIdx y))), toIdx z)

spec12 :: AP.Parameters -> StdGen -> StdGen -> SpecWith ()
spec12 par gen1 gen2 = describe
   "maxScore2" $ do
     it "checks that the highest score is generated for player 2" $ do
       arr <- initialArray par 
       maxed <- maxScore (Memory.fromSV (SV.replicate (fmap toIdx initialObs))) arr (AP.actionSpace2 par)
       shouldBe
          maxed
          maxLs
   where
    initialObs =
       AP.Obs (samplePopulation_ (AP.actionSpace1 par) gen1, samplePopulation_ (AP.actionSpace2 par) gen2)
    filteredLs = [(w,z) |(((x,y),z),w) <- AP.lsValues2 par, (x,y) == AP.unObs initialObs]
    maxLs = maximumBy (comparing fst) filteredLs
    initialArray :: AP.Parameters -> IO (QTable AP.Player2N AP.Observation AP.PriceSpace)
    initialArray par = do
              arr <- newArray_ (asIdx l, asIdx u)
              traverse_ (\(k, v) -> writeArray arr ( (asIdx k)) v) lsValues'
              pure arr
              where
                lsValues' = AP.lsValues2 par
                l = minimum $ fmap fst lsValues'
                u = maximum $ fmap fst lsValues'
                asIdx ((x, y), z) = (Memory.fromSV (SV.replicate (AP.Obs (toIdx x, toIdx y))), toIdx z)



