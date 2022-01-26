{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}

module QLearningBoltzmannSpec where

import           Control.Monad
import           Control.Monad.IO.Class
import qualified Data.Map.Strict      as M
import qualified Data.Vector.Sized as SV
import qualified Data.Vector       as V
import           Test.Hspec
import           System.Random
import           System.Random.MWC.CondensedTable

import qualified Engine.Memory as Memory
import           Examples.QLearning.CournotBoltzmann.Internal
import           Engine.QLearningBoltzmann

spec :: Spec
spec = do
  testUpdateProbability
  testUpdateProbability2


testParameters =  Parameters
  { pBeta =  0.99999
  , pInitialExploreRate1 = 1000*0.99999
  , pInitialExploreRate2 = 1000*0.99999
  , pLowerQuantity1 = 0
  , pUpperQuantity1 = 40
  , pLowerQuantity2 = 0
  , pUpperQuantity2 = 40
  , pGamma = 0.90
  , pLearningRate = 0.15
  , pA = 40
  , pB = 1
  , pC1 = 4
  , pC2 = 4
  , pM1 = 1 -- NOTE: Due to the construction, we need to take the orginial value of Calvano and take -1
  , pM2 = 1 -- NOTE: Due to the construction, we need to take the orginial value of Calvano and take -1
  , pGeneratorEnv1 = mkStdGen 1
  , pGeneratorEnv2 = mkStdGen 1
  , pGeneratorObs1 = mkStdGen 1
  , pGeneratorObs2 = mkStdGen 1
  }

testParameters2 =  Parameters
  { pBeta =  0.99999
  , pInitialExploreRate1 = 1000*0.99999
  , pInitialExploreRate2 = 1000*0.99999
  , pLowerQuantity1 = 1
  , pUpperQuantity1 = 41
  , pLowerQuantity2 = 1
  , pUpperQuantity2 = 41
  , pGamma = 0.90
  , pLearningRate = 0.15
  , pA = 40
  , pB = 1
  , pC1 = 4
  , pC2 = 4
  , pM1 = 1 -- NOTE: Due to the construction, we need to take the orginial value of Calvano and take -1
  , pM2 = 1 -- NOTE: Due to the construction, we need to take the orginial value of Calvano and take -1
  , pGeneratorEnv1 = mkStdGen 1
  , pGeneratorEnv2 = mkStdGen 1
  , pGeneratorObs1 = mkStdGen 1
  , pGeneratorObs2 = mkStdGen 1
  }

testCTable :: CTable QuantitySpace
testCTable = uniformCTable actionsV
  where actionsV = population $ actionSpace2 testParameters
        action1   = actionsV V.! 0
        action2   = actionsV V.! 1

testCTable2 = uniformCTable actionsV
  where actionsV = population $ actionSpace2 testParameters2
        action1   = actionsV V.! 0
        action2   = actionsV V.! 1

testSampler :: CTable QuantitySpace -> IO QuantitySpace
testSampler x = do
  gen <- newStdGen
  pure $ samplePopulation_ x gen

repeatedSampler i x = do
  ls <- replicateM i (testSampler x)
  let values = fmap value ls
  return $ (sum values / (fromIntegral $ length values))


initialObs = Obs $ head $ quantityPairs testParameters

testUpdateProbability = describe
  "correct update of probability" $ do
     it "correct update of probability" $ do
         shouldBe
            (population testCTable)
            (population testCTable2)

testUpdateProbability2 = describe
  "difference not statistically different" $ do
     it "shift of avg not larger than 1" $ do
         avg1 <- repeatedSampler 1000 testCTable
         avg2 <- repeatedSampler 1000 testCTable2
         shouldNotSatisfy
            (avg2 - avg1)
            (> 1)


