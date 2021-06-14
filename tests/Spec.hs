{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Spec where

import qualified Data.Vector as V
import           RIO hiding (preview)
import           System.Random
import           Test.Hspec

import qualified Engine.QLearning as QLearning
import Examples.QLearning.CalvanoReplication 


--------------------------------------------------------------------------------
-- Test types


parametersTest = Parameters
  { pKsi = 0.1
  , pBeta = (- 0.000004)
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
  , pGeneratorEnv1 = 3
  , pGeneratorEnv2 = 100
  , pGeneratorPrice1 = 90
  , pGeneratorPrice2 = 39
  , pGeneratorObs1 = 2
  , pGeneratorObs2 = 400
  }

main :: IO ()
main = hspec spec



spec =
  describe
    "ActionSpace" $ do
       it "computes the correct list of action values" $ do
          shouldBe 
              (V.toList (QLearning.population $ actionSpace parametersTest))
              [PriceSpace {value = 1.425, idx = 0},PriceSpace {value = 1.4635714285714285, idx = 1},PriceSpace {value = 1.502142857142857, idx = 2},PriceSpace {value = 1.5407142857142855, idx = 3},PriceSpace {value = 1.579285714285714, idx = 4},PriceSpace {value = 1.6178571428571424, idx = 5},PriceSpace {value = 1.656428571428571, idx = 6},PriceSpace {value = 1.6949999999999994, idx = 7},PriceSpace {value = 1.7335714285714279, idx = 8},PriceSpace {value = 1.7721428571428564, idx = 9},PriceSpace {value = 1.8107142857142848, idx = 10},PriceSpace {value = 1.8492857142857133, idx = 11},PriceSpace {value = 1.8878571428571418, idx = 12},PriceSpace {value = 1.9264285714285703, idx = 13},PriceSpace {value = 1.9649999999999987, idx = 14}]



