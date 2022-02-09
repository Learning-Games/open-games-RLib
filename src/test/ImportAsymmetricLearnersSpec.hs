{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}

module ImportAsymmetricLearnersSpec where

import           Control.Monad
import           Control.Monad.IO.Class
import qualified Data.Map.Strict      as M
import qualified Data.Vector.Sized as SV
import qualified Data.Vector       as V
import           Test.Hspec
import           System.Random

import           Engine.QLearning.ImportAsymmetricLearners

spec :: Spec
spec = do
  testTransformList





testTransformList = describe
  "State action index and qvalue csv imports are merged correctly" $ do
     it "transform list correctly" $ do
         shouldBe 
            (transformIndexList testIndex testQValue)
            testQValueTarget
     it "exception empty value" $ do
         shouldBe
            (findElement [] (1 :: Int))
            testEmptyFind
     it "finds correct value" $ do
         shouldBe
            (findElement testIndex (1 :: Int))
            (Right ((10,20,30)))
  where testIndex = [(10,20,30,1),(40,50,60,2),(70,80,90,3)]
        testQValue = [(1,1000),(2,2000),(3,3000)]
        testQValueTarget = [(10,20,30,1000),(40,50,60,2000),(70,80,90,3000)]
        testEmptyFind :: Either String (Action,Action,Action)
        testEmptyFind = Left "Element 1 was not found"
