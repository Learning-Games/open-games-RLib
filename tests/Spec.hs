{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE DuplicateRecordFields #-}

import           RIO hiding (preview)
import           System.Random
import           Test.Hspec

import qualified Engine.QLearning.Export as QLearning
import Examples.QLearning.CalvanoReplicationExportParameters



--------------
-- Preparation

parameters = Parameters
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
  , pGeneratorEnv1 = 3
  , pGeneratorEnv2 = 100
  , pGeneratorPrice1 = 90
  , pGeneratorPrice2 = 39
  , pGeneratorObs1 = 2
  , pGeneratorObs2 = 400
  }

initialStratExport = initialStrat parameters
initialStratOld    = initialStratToTest

--------------------------------------------------------------------------------
-- Test types

main :: IO ()
main = undefined


spec :: SpecWith ()
spec =
  describe
    "TestParameters" $ do
     it "checks whether the export works as expected - initial strategy" $ do
         initialStratExport == initialStratOld
