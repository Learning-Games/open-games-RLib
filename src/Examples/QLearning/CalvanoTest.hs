{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DeriveGeneric, OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveGeneric #-}

module Examples.QLearning.CalvanoTest
  ( evalStageLS
  , initialStrat
  , actionSpace
  , csvParameters
  , sequenceL
  , evalStageM
  , mapStagesM_
  , decreaseFactor
  , PriceSpace(..)
  , Observation(..) 
  , Parameters(..)
  ) where

import           Control.DeepSeq
import           Control.Monad.IO.Class
import qualified Data.Aeson as Aeson
import           Data.Array.IO as A
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.Builder as SB
import           Data.Csv
import           Data.Double.Conversion.ByteString
import           Data.Foldable
import qualified Data.Ix as I
import qualified Data.Vector as V
import qualified Data.Vector.Sized as SV
import qualified Engine.Memory as Memory
import           Engine.OpenGames
import           Engine.OpticClass
import           Engine.QLearning
import           Engine.TLL
import           FastCsv
import           GHC.Generics
import           Preprocessor.Compile
import           RIO (RIO, GLogFunc)
import           System.Random
import qualified System.Random as Rand

----------
-- 0 Types

instance FastCsv.BuildCsvField (PriceSpace, PriceSpace) where
  {-# INLINE buildCsvField #-}
  buildCsvField (PriceSpace {value=p1},PriceSpace {value=p2}) =
    SB.byteString (toShortest p1) <> " " <>
    SB.byteString (toShortest p2)

instance FastCsv.BuildCsvField PriceSpace where
  buildCsvField (PriceSpace {value=p1}) =
    SB.byteString (toShortest p1)
  {-# INLINE buildCsvField #-}

-- Warning: assumes players have same memory arity.
type M = RIO (GLogFunc (QLearningMsg Player1N Observation PriceSpace))
type Player1N = 1
type Player2N = 1


newtype Observation a = Obs
  { unObs :: (a, a)
  } deriving (Show,Generic,I.Ix,Ord, Eq, Functor, NFData, Aeson.ToJSON)

data PriceSpace = PriceSpace {
   value :: !Double
  , idx :: !Int
  } deriving (Show, Eq, Ord, Generic)
instance Aeson.ToJSON PriceSpace where
  toJSON PriceSpace {value} = Aeson.toJSON value
instance NFData PriceSpace where
  rnf x =
    let !_ = x -- PriceSpace is strict in its fields.
    in ()
instance ToField PriceSpace where
  toField (PriceSpace {value, idx}) =
    toField value <> " [idx=" <> toField idx <> "]"
instance ToIdx PriceSpace where
  toIdx PriceSpace {idx} = Idx idx

-- Define parameters needed to specify the game
data Parameters = Parameters
  { pKsi :: Double
  , pBeta :: ExploreRate
  , pBertrandPrice :: Double
  , pMonopolyPrice :: Double
  , pGamma :: DiscountFactor
  , pLearningRate :: LearningRate
  , pMu :: Double
  , pA1 :: Double
  , pA2 :: Double
  , pA0 :: Double
  , pC1 :: Double
  , pM  :: Double
  , pGeneratorEnv1 :: StdGen
  , pGeneratorEnv2 :: StdGen
  , pGeneratorPrice1 :: StdGen
  , pGeneratorPrice2 :: StdGen
  , pGeneratorObs1 :: StdGen
  , pGeneratorObs2 :: StdGen
  } deriving (Generic,Show)


-- Demand follows eq 5 in Calvano et al.
demand :: Floating a => a -> a -> a -> a -> a -> a -> a
demand a0 a1 a2 p1 p2 mu = (exp 1.0)**((a1-p1)/mu) / agg
  where agg = (exp 1.0)**((a1-p1)/mu) + (exp 1.0)**((a2-p2)/mu) + (exp 1.0)**(a0/mu)

-- Profit function
profit :: Double -> Double -> Double -> PriceSpace -> PriceSpace -> Double -> Double -> Double
profit a0 a1 a2 (PriceSpace p1 _) (PriceSpace p2 _) mu c1 = (p1 - c1)* (demand a0 a1 a2 p1 p2 mu)

-- Functional form for decreasing exploration
decreaseFactor :: Floating a => a -> a
decreaseFactor b = (exp 1) ** b
------------------------------------------------------
-- Create index on the basis of the actual prices used
-- Also allows for different price types
-- Follows the concept in Calvano

-- creates the distance in the grid, takes parameter m as input to make the relevant number of steps
dist :: Parameters -> Double
dist par =  (upperBound par - lowerBound par) / pM par



-- determine the bounds within which to search
lowerBound,upperBound :: Parameters -> Double
lowerBound Parameters{pBertrandPrice,pKsi,pMonopolyPrice} = pBertrandPrice - pKsi*(pMonopolyPrice - pBertrandPrice)
upperBound Parameters{pBertrandPrice,pKsi,pMonopolyPrice} = pMonopolyPrice + pKsi*(pMonopolyPrice - pBertrandPrice)

-----------------------------------------------------
-- Transforming bounds into the array and environment
-- create the action space
actionSpace :: Parameters -> CTable PriceSpace
actionSpace par =
  uniformCTable
    (V.imap
       (\idx value -> PriceSpace {value, idx})
       (V.fromList [lowerBound par,lowerBound par + dist par .. upperBound par]))

-- derive possible observations
pricePairs :: Parameters -> [(PriceSpace, PriceSpace)]
pricePairs par =
  [ (x, y)
  | x <- V.toList (population $ actionSpace par)
  , y <- V.toList (population $ actionSpace par)
  ]


-- initiate a first fixed list of values at average
lsValues :: Parameters -> [(((PriceSpace, PriceSpace), PriceSpace), Double)]
lsValues par@Parameters{pGamma,pA0,pA1,pA2,pMu,pC1} = [(((x,y),z),value z)| (x,y) <- xs, (z,_) <- xs]
  where  xs = pricePairs par
         value p1 = (sum  $ fmap (\p2 -> profit pA0 pA1 pA2 p1 p2 pMu pC1) priceLs) / ((1 - pGamma) * (fromIntegral $ length priceLs))
         priceLs = V.toList (population $ actionSpace par)




-- initiate the environment
initialEnv1 :: Parameters -> M (Env Player1N Observation PriceSpace)
initialEnv1 par@Parameters{pBeta,pGeneratorEnv1} =
  liftIO initialArray >>= \arr ->
    pure $
      Env
         "Player1"
         1
         arr
         0
         (decreaseFactor pBeta)
         pGeneratorEnv1
         (Memory.fromSV (SV.replicate (fmap toIdx (initialObservation par))))
         (5 * 0.999)
         "NothingHappenedYet"
  where
    initialArray :: IO (QTable Player1N Observation PriceSpace)
    initialArray = do
      arr <- newArray_ (asIdx l, asIdx u)
      traverse_ (\(k, v) -> writeArray arr ( (asIdx k)) v) lsValues'
      pure arr
      where
        lsValues' = lsValues par
        l = minimum $ fmap fst lsValues'
        u = maximum $ fmap fst lsValues'
        asIdx ((x, y), z) = (Memory.fromSV (SV.replicate (Obs (toIdx x, toIdx y))), toIdx z)

initialEnv2 :: Parameters -> M (Env Player2N Observation PriceSpace)
initialEnv2 par@Parameters{pBeta,pGeneratorEnv2} =
  liftIO initialArray >>= \arr ->
    pure $
    Env
      "Player2" 2
      (arr)
      0
      (decreaseFactor pBeta)
      pGeneratorEnv2
      (Memory.fromSV (SV.replicate (fmap toIdx (initialObservation par))))
      (5 * 0.999)
      "NothingHappenedYet"
  where
    initialArray :: IO (QTable Player2N Observation PriceSpace)
    initialArray = do
      arr <- newArray_ (asIdx l, asIdx u)
      traverse_ (\(k, v) -> writeArray arr (asIdx k) v) lsValues'
      pure arr
      where
        lsValues' = lsValues par
        l = minimum $ fmap fst lsValues'
        u = maximum $ fmap fst lsValues'
        asIdx ((x, y), z) = (Memory.fromSV (SV.replicate (Obs (toIdx x, toIdx y))), toIdx z)

-----------------------------
-- 4. Constructing initial state

-- First observation, randomly determined
initialObservation :: Parameters -> Observation PriceSpace
initialObservation par@Parameters{pGeneratorPrice1,pGeneratorPrice2} =
  Obs (samplePopulation_ (actionSpace par) pGeneratorPrice1, samplePopulation_ (actionSpace par) pGeneratorPrice2)

-- Initiate strategy: start with random price
initialStrat :: Parameters -> M (List '[M (PriceSpace, Env Player1N Observation PriceSpace), M (PriceSpace, Env Player2N Observation PriceSpace)])
initialStrat par@Parameters{pGeneratorObs1,pGeneratorObs2}= do
  e1 <- initialEnv1 par
  e2 <- initialEnv2 par
  pure
    (pure (samplePopulation_ (actionSpace par) pGeneratorObs1, e1) ::-
     pure (samplePopulation_ (actionSpace par) pGeneratorObs2, e2) ::-
     Nil)


------------------------------
-- Updating state

toObs :: Monad m => m (a,Env Player1N Observation a) -> m (a, Env Player2N Observation a) -> m ((), (Observation a, Observation a))
toObs a1 a2 = do
             (act1,_env1) <- a1
             (act2,_env2) <- a2
             let obs1 = Obs (act1,act2)
                 obs2 = Obs (act2,act1)
                 in return ((),(obs1,obs2))

toObsFromLS :: Monad m => List '[m (a,Env Player1N Observation a), m (a,Env Player2N Observation a)] -> m ((),(Observation a,Observation a))
toObsFromLS (x ::- (y ::- Nil))= toObs x y


-- From the outputted list of strategies, derive the context
fromEvalToContext :: Monad m =>  List '[m (a,Env Player1N Observation a), m (a,Env Player2N Observation a)] ->
                     MonadContext m (Observation a, Observation a) () (a,a) ()
fromEvalToContext ls = MonadContext (toObsFromLS ls) (\_ -> (\_ -> pure ()))



------------------------------
-- Game stage
stageSimple :: Parameters -> OpenGame (MonadOptic M) (MonadContext M) ('[M (PriceSpace, Env Player1N Observation PriceSpace), M (PriceSpace, Env Player1N Observation PriceSpace)] +:+ '[]) ('[M (PriceSpace, Env Player1N Observation PriceSpace), M (PriceSpace, Env Player1N Observation PriceSpace)] +:+ '[]) (Observation PriceSpace, Observation PriceSpace) () (PriceSpace, PriceSpace) ()
stageSimple par@Parameters{pBeta,pLearningRate,pGamma,pA0,pA1,pA2,pMu,pC1} = [opengame|
   inputs    : (state1,state2) ;
   feedback  :      ;

   :-----------------:
   inputs    :  state1    ;
   feedback  :      ;
   operation : pureDecisionQStageDiagnostics (actionSpace par) "Player1" chooseExploreActionDiag (chooseLearnDecrExploreQTableDiag pLearningRate pGamma (decreaseFactor pBeta)) ;
   outputs   :  p1 ;
   returns   :  (profit pA0 pA1 pA2 p1 p2 pMu pC1, Obs (p1,p2)) ;

   inputs    : state2     ;
   feedback  :      ;
   operation : pureDecisionQStageDiagnostics (actionSpace par) "Player2" chooseExploreActionDiag (chooseLearnDecrExploreQTableDiag pLearningRate pGamma (decreaseFactor pBeta)) ;
   outputs   :  p2 ;
   returns   :  (profit pA0 pA1 pA2 p2 p1 pMu pC1, Obs (p1,p2))    ;
   :-----------------:

   outputs   :  (p1, p2)    ;
   returns   :      ;


|]

----------------------------------
-- Defining the iterator structure
evalStage ::
  Parameters
  -> List '[ M (PriceSpace, Env Player1N Observation PriceSpace), M ( PriceSpace
                                                                      , Env Player2N Observation PriceSpace)]
  -> MonadContext M (Observation PriceSpace, Observation PriceSpace) () ( PriceSpace
                                                                         , PriceSpace) ()
  -> List '[ M (PriceSpace, Env Player1N Observation PriceSpace), M ( PriceSpace
                                                                      , Env Player2N Observation PriceSpace)]
evalStage par = evaluate (stageSimple par)


-- Explicit list constructor much better
evalStageLS :: (Ord t, Num t) =>
  Parameters
  -> List '[M (PriceSpace, Env Player1N Observation PriceSpace), M (PriceSpace, Env Player2N Observation PriceSpace)]
  -> t
  -> [List '[M (PriceSpace, Env Player1N Observation PriceSpace), M (PriceSpace, Env Player2N Observation PriceSpace)]]
evalStageLS par startValue n =
          let context  = fromEvalToContext startValue
              newStrat = evalStage par startValue context
              in if n > 0 then newStrat : evalStageLS par newStrat (n-1)
                          else [newStrat]

evalStageM ::
  Parameters
  -> List '[ (PriceSpace, Env Player1N Observation PriceSpace), ( PriceSpace
                                                                , Env Player2N Observation PriceSpace)]
  -> Int
  -> M [List '[ (PriceSpace, Env Player1N Observation PriceSpace), ( PriceSpace
                                                                    , Env Player2N Observation PriceSpace)]]
evalStageM par _ 0 = pure []
evalStageM par startValue n = do
  newStrat <-
    sequenceL
      (evalStage par (hoist startValue) (fromEvalToContext (hoist startValue)))
  rest <- evalStageM par newStrat (pred n)
  pure (newStrat : rest)

{-# INLINE mapStagesM_ #-}
mapStagesM_ ::
  Parameters
  -> (s ->  List '[ (PriceSpace, Env Player1N Observation PriceSpace), ( PriceSpace
                                                                        , Env Player2N Observation PriceSpace)] -> M s)
  -> List '[ (PriceSpace, Env Player1N Observation PriceSpace), ( PriceSpace
                                                                , Env Player2N Observation PriceSpace)]
  -> Int
  -> s
  -> M ()
mapStagesM_ par f startValue n0 s0 = go s0 startValue n0
  where
    go _ _ 0 = pure ()
    go s value !n = do
      newStrat <-
        sequenceL (evalStage par (hoist value) (fromEvalToContext (hoist value)))
      s' <- f s newStrat
      go s' newStrat (pred n)

hoist ::
     Applicative f
  => List '[ (PriceSpace, Env Player1N Observation PriceSpace), ( PriceSpace
                                                                , Env Player2N Observation PriceSpace)]
  -> List '[ f (PriceSpace, Env Player1N Observation PriceSpace), f ( PriceSpace
                                                                    , Env Player2N Observation PriceSpace)]
hoist (x ::- y ::- Nil) = pure x ::- pure y ::- Nil

sequenceL ::
     Monad m
  => List '[ m (PriceSpace, Env Player1N Observation PriceSpace), m (PriceSpace, Env Player2N Observation PriceSpace)]
  -> m (List '[ (PriceSpace, Env Player1N Observation PriceSpace), (PriceSpace, Env Player2N Observation PriceSpace)])
sequenceL (x ::- y ::- Nil) = do
  v <- x
  v' <- y
  pure (v ::- v' ::- Nil)


------------------------
-- Export extended config data  as csv
-- Parameters

data ExportParameters = ExportParameters
  { expKsi :: !Double
  , expBeta :: !ExploreRate
  , expDecreaseFactor :: !ExploreRate
  , expBertrandPrice :: !Double
  , expMonpolyPrice :: !Double
  , expGamma :: !DiscountFactor
  , expLearningRate :: !LearningRate
  , expMu :: !Double
  , expA1 :: !Double
  , expA2 :: !Double
  , expA0 :: !Double
  , expC1 :: !Double
  , expM  :: !Double
  , expLowerBound :: !Double
  , expUpperBound :: !Double
  , expGeneratorEnv1 :: !String
  , expGeneratorEnv2 :: !String
  , expGeneratorPrice1 :: !String
  , expGeneratorPrice2 :: !String
  , expGeneratorObs1 :: !String
  , expGeneratorObs2 :: !String
  , expInitialObservation1 :: !PriceSpace
  , expInitialObservation2 :: !PriceSpace
  , expInitialPrice1 :: !PriceSpace
  , expInitialPrice2 :: !PriceSpace
  } deriving (Generic,Show)

instance ToNamedRecord ExportParameters
instance DefaultOrdered ExportParameters

-- | Instantiate the export parameters with the used variables
exportParameters :: Parameters -> ExportParameters
exportParameters par = ExportParameters
  (pKsi par)
  (pBeta par)
  (decreaseFactor $ pBeta par)
  (pBertrandPrice par)
  (pMonopolyPrice par)
  (pGamma par)
  (pLearningRate par)
  (pMu par)
  (pA1 par)
  (pA2 par)
  (pA0 par)
  (pC1 par)
  (pM par)
  (lowerBound par)
  (upperBound par)
  (show $ pGeneratorEnv1 par)
  (show $ pGeneratorEnv2 par)
  (show $ pGeneratorPrice1 par)
  (show $ pGeneratorPrice2 par)
  (show $ pGeneratorObs1 par)
  (show $ pGeneratorObs2 par)
  (samplePopulation_ (actionSpace par) (pGeneratorObs1 par))
  (samplePopulation_ (actionSpace par) (pGeneratorObs2 par))
  (samplePopulation_ (actionSpace par) (pGeneratorPrice1 par))
  (samplePopulation_ (actionSpace par) (pGeneratorPrice2 par))

-- | export to CSV
csvParameters :: Parameters -> L.ByteString
csvParameters par = encodeDefaultOrderedByName  [exportParameters par]





