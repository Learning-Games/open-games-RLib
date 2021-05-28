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

module Examples.QLearningSimple.CalvanoReplication
  ( evalStageLS
  , initialStrat
  , beta
  , actionSpace
  , csvParameters
  , exportQValuesJSON
  , sequenceL
  , evalStageM
  , PriceSpace(..)
  ) where
import           Data.Foldable
import qualified Data.HashMap.Strict as HM
import           Data.HashMap.Strict (HashMap)
import qualified Engine.Memory as Memory
import           Control.DeepSeq
import qualified Data.Vector.Sized as SV
import           Data.Function
import qualified Data.Vector as V
import           Data.Vector (Vector)
import           Data.Bifunctor
import           Data.Hashable
import qualified Data.ByteString.Char8 as S8
import           Data.ByteString (ByteString)
import           Data.Array.IO as A
import           Debug.Trace
import           Control.DeepSeq
import           Data.Aeson
import           Data.Csv
import           Language.Haskell.TH

import qualified Data.List as L
import qualified Data.Ix as I
import           GHC.Generics
import qualified System.Random as Rand
import           System.Random

import           Engine.QLearningSimple
import           Engine.OpenGames
import           Engine.TLL
import           Engine.OpticClass
import           Preprocessor.AbstractSyntax
import           Preprocessor.Compile
import           Preprocessor.THSyntax

----------
-- 0 Types

type Player1N = 1
type Player2N = 1

newtype Observation a = Obs
  { unObs :: (a, a)
  } deriving (Show,Generic,I.Ix,Ord, Eq, ToJSON, Functor, NFData)

data PriceSpace = PriceSpace {
   value :: !Double
  , idx :: !Int
  } deriving (Show, Eq, Ord)
instance NFData PriceSpace where
  rnf x =
    let !_ = x -- PriceSpace is strict in its fields.
    in ()
instance ToField PriceSpace where
  toField (PriceSpace {value, idx}) =
    toField value <> " [idx=" <> toField idx <> "]"
instance ToIdx PriceSpace where
  toIdx PriceSpace {idx} = Idx idx

instance ToField Rand.StdGen


type Price = Integer

type Index = Integer

------------------------
-- 1. Export Data as csv
-- 1.1. Parameters

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
  , expGeneratorEnv1 :: !Int
  , expGeneratorEnv2 :: !Int
  , expGeneratorPrice1 :: !Int
  , expGeneratorPrice2 :: !Int
  , expGeneratorObs1 :: !Int
  , expGeneratorObs2 :: !Int
  , expInitialObservation1 :: !PriceSpace
  , expInitialObservation2 :: !PriceSpace
  , expInitialPrice1 :: !PriceSpace
  , expInitialPrice2 :: !PriceSpace
  } deriving (Generic,Show)

instance ToNamedRecord ExportParameters
instance DefaultOrdered ExportParameters

-- | Instantiate the export parameters with the used variables
parameters = ExportParameters
  ksi
  beta
  (decreaseFactor beta)
  bertrandPrice
  monopolyPrice
  gamma
  learningRate
  mu
  a1
  a2
  a0
  c1
  m
  lowerBound
  upperBound
  generatorState1
  generatorState2
  generatorPrice1
  generatorPrice2
  generatorObs1
  generatorObs2
  (samplePopulation_ actionSpace (mkStdGen generatorObs1))
  (samplePopulation_ actionSpace (mkStdGen generatorObs2))
  (samplePopulation_ actionSpace (mkStdGen generatorPrice1))
  (samplePopulation_ actionSpace (mkStdGen generatorPrice2))

-- | export to CSV
csvParameters = encodeDefaultOrderedByName  [parameters]

instance ToNamedRecord ExportQValues
instance DefaultOrdered ExportQValues

-------------------------
-- 2. Export Data as JSON
-- TODO need to deal with Rand.StdGen

-- 2.1. Parameters

-- 2.2. Q-values

data ExportQValues = ExportQValues
   { expName :: !Agent
   , expIteration :: !Int
   , expObs  :: !(FieldAsJson (Memory.Vector Player1N (Observation (Idx PriceSpace))))
   , expQValues  :: !(FieldAsJson [((Memory.Vector Player1N (Observation (Idx PriceSpace)),Idx PriceSpace),Double)])
   } deriving (Generic,Show)
instance ToJSON ExportQValues

-- | Easy way to have a CSV field encoded by JSON.
newtype FieldAsJson a = FieldAsJson { unFieldAsJson :: a}
  deriving (Generic, Show)
instance ToJSON a => ToField (FieldAsJson a) where
  toField = toField . Data.Aeson.encode . unFieldAsJson
instance ToJSON a => ToJSON (FieldAsJson a) where
  toJSON = toJSON . unFieldAsJson

-- | Extract relevant information into a record to be exported
fromTLLToExport :: List '[ (PriceSpace, State Player1N Observation PriceSpace), (PriceSpace, State Player2N Observation PriceSpace)] -> IO [ExportQValues]
fromTLLToExport (p1 ::- p2 ::- Nil) = do
  let ((_, state1)) = p1
      ((_, state2)) = p2
  let name1 = _name state1
      name2 = _name state2
      expIteration1 = _iteration state1
      expIteration2 = _iteration state2
      expObs1 = _obsAgent state1
      expObs2 = _obsAgent state2
  expQValues1 <- A.getAssocs $ _qTable state1
  expQValues2 <- A.getAssocs $ _qTable state2
  let
      expPlayer1 = ExportQValues name1 expIteration1 (FieldAsJson expObs1) (FieldAsJson expQValues1)
      expPlayer2 = ExportQValues name2 expIteration2 (FieldAsJson expObs2) (FieldAsJson expQValues2)
  pure $ [expPlayer1, expPlayer2]

fromTLLListToExport :: [List '[ (PriceSpace, State Player1N Observation PriceSpace), (PriceSpace, State Player2N Observation PriceSpace)]]-> IO [ExportQValues]
fromTLLListToExport = fmap concat . traverse fromTLLToExport

exportQValuesJSON ls = fmap foldable $ fromTLLListToExport ls

indexMapObject :: HashMap (Idx PriceSpace) PriceSpace
indexMapObject = HM.fromList (toList indexMapVector)

indexMapVector :: V.Vector (Idx PriceSpace, PriceSpace)
indexMapVector = V.imap (\i v -> (Idx i, v)) (population actionSpace)

------------------------------------------
-- 3. Environment variables and parameters
-- Fixing the parameters

ksi :: Double
ksi = 0.1

-- Decrease temp per iteration
beta =  (- 0.00001)

decreaseFactor b = (log 1) ** b

-- NOTE that the the prices are determined by calculations for the exact values
bertrandPrice = 1
monopolyPrice = 6


-- fixing outside parameters
gamma = 0.95

learningRate = 0.15

mu = 0.25

a1 = 1.5

a2 = 1

a0 = 1

c1 = 0.5

-- NOTE: Due to the construction, we need to take the orginial value of Calvano and take -1
m = 14

-- Demand follows eq 5 in Calvano et al.
demand a0 a1 a2 p1 p2 mu = (exp 1.0)**((a1-p1)/mu) / agg
  where agg = (exp 1.0)**((a1-p1)/mu) + (exp 1.0)**((a2-p2)/mu) + (exp 1.0)**(a0/mu)

-- Profit function
profit a0 a1 a2 (PriceSpace p1 _) (PriceSpace p2 _) mu c1 = (p1 - c1)* (demand a0 a1 a2 p1 p2 mu)

-- Fixing initial generators
generatorObs1 = 2
generatorObs2 = 400
generatorState1 = 3
generatorState2 = 100
generatorPrice1 = 90
generatorPrice2 = 39

------------------------------------------------------
-- Create index on the basis of the actual prices used
-- Also allows for different price types
-- Follows the concept in Calvano

-- creates the distance in the grid, takes parameter m as input to make the relevant number of steps
dist :: Double
dist =  (upperBound - lowerBound) / m



-- determine the bounds within which to search
lowerBound,upperBound :: Double
lowerBound = bertrandPrice - ksi*(monopolyPrice - bertrandPrice)
upperBound = monopolyPrice + ksi*(monopolyPrice - bertrandPrice)

priceBounds = (lowerBound,upperBound)

-----------------------------------------------------
-- Transforming bounds into the array and environment
-- create the action space
actionSpace :: CTable PriceSpace
actionSpace =
  uniformCTable
    (V.imap
       (\idx value -> PriceSpace {value, idx})
       (V.fromList [lowerBound,lowerBound + dist .. upperBound]))

-- derive possible observations
pricePairs =
  [ (x, y)
  | x <- V.toList (population actionSpace)
  , y <- V.toList (population actionSpace)
  ]

-- initiate a first fixed list of values at average
-- TODO change later to random values
lsValues  = [(((x,y),z),avg)| (x,y) <- xs, (z,_) <- xs]
  where  xs = pricePairs
         avg = (lowerBound + upperBound) / 2

-- initiate the environment
initialState1 :: IO (State Player1N Observation PriceSpace)
initialState1 =
  initialArray >>= \arr ->
    pure $
      State
         "Player1"
         arr
         0
         (decreaseFactor beta)
         (Rand.mkStdGen generatorState1)
         (Memory.fromSV (SV.replicate (fmap toIdx initialObservation)))
         initialObservation
         undefined -- NOTE: temp not used in this model
  where
    initialArray :: IO (QTable Player1N Observation PriceSpace)
    initialArray = do
      arr <- newArray_ (asIdx l, asIdx u)
      traverse (\(k, v) -> writeArray arr ( (asIdx k)) v) lsValues
      pure arr
      where
        l = minimum $ fmap fst lsValues
        u = maximum $ fmap fst lsValues
        asIdx ((x, y), z) = (Memory.fromSV (SV.replicate (Obs (toIdx x, toIdx y))), toIdx z)

initialState2 :: IO (State Player2N Observation PriceSpace)
initialState2 =
  initialArray >>= \arr ->
    pure $
    State
      "Player2"
      (arr)
      0
      (decreaseFactor beta)
      (Rand.mkStdGen generatorState2)
      (Memory.fromSV (SV.replicate (fmap toIdx initialObservation)))
      initialObservation
      undefined -- NOTE: temp not used in this model
  where
    initialArray :: IO (QTable Player2N Observation PriceSpace)
    initialArray = do
      arr <- newArray_ (asIdx l, asIdx u)
      traverse (\(k, v) -> writeArray arr (asIdx k) v) lsValues
      pure arr
      where
        l = minimum $ fmap fst lsValues
        u = maximum $ fmap fst lsValues
        asIdx ((x, y), z) = (Memory.fromSV (SV.replicate (Obs (toIdx x, toIdx y))), toIdx z)

-----------------------------
-- 4. Constructing initial state

-- First observation, randomly determined
initialObservation :: Observation PriceSpace
initialObservation =
  Obs (samplePopulation_ actionSpace (mkStdGen generatorPrice1), samplePopulation_ actionSpace (mkStdGen generatorPrice2))

-- Initiate strategy: start with random price
initialStrat :: IO (List '[IO (PriceSpace, State Player1N Observation PriceSpace), IO (PriceSpace, State Player2N Observation PriceSpace)])
initialStrat = do
  e1 <- initialState1
  e2 <- initialState2
  pure
    (pure (samplePopulation_ actionSpace (mkStdGen generatorObs1), e1) ::-
     pure (samplePopulation_ actionSpace (mkStdGen generatorObs2), e2) ::-
     Nil)


------------------------------
-- Updating state

toObs :: Monad m => m (a,State Player1N Observation a) -> m (a, State Player2N Observation a) -> m ((), ())
toObs _ _ = return ((),())

toObsFromLS :: Monad m => List '[m (a,State Player1N Observation a), m (a,State Player2N Observation a)] -> m ((),())
toObsFromLS (x ::- (y ::- Nil))= toObs x y


-- From the outputted list of strategies, derive the context
fromEvalToContext :: Monad m =>  List '[m (a,State Player1N Observation a), m (a,State Player2N Observation a)] ->
                     MonadContext m () () (a,a) ()
fromEvalToContext ls = MonadContext (return ((),())) (\_ -> (\_ -> pure ()))



------------------------------
-- Game stage
stageSimple beta = [opengame|
   inputs    :  ;
   feedback  :  ;

   :-----------------:
   inputs    :  ;
   feedback  :  ;
   operation : pureDecisionQStage actionSpace "Player1" chooseExploreAction (chooseLearnDecrExploreQTable learningRate gamma (decreaseFactor beta)) ;
   outputs   :  p1 ;
   returns   :  (profit a0 a1 a2 p1 p2 mu c1, Obs (p1,p2)) ;

   inputs    :  ;
   feedback  :  ;
   operation : pureDecisionQStage actionSpace "Player2" chooseExploreAction (chooseLearnDecrExploreQTable learningRate gamma (decreaseFactor beta)) ;
   outputs   :  p2 ;
   returns   :  (profit a0 a1 a2 p2 p1 mu c1, Obs (p1,p2))    ;
   :-----------------:

   outputs   :  (p1, p2)    ;
   returns   :      ;


|]

----------------------------------
-- Defining the iterator structure
evalStage ::
     List '[ IO (PriceSpace, State Player1N Observation PriceSpace), IO ( PriceSpace
                                                                      , State Player2N Observation PriceSpace)]
  -> MonadContext IO () () ( PriceSpace, PriceSpace) ()
  -> List '[ IO (PriceSpace, State Player1N Observation PriceSpace), IO ( PriceSpace
                                                                      , State Player2N Observation PriceSpace)]
evalStage = evaluate (stageSimple beta)


-- Explicit list constructor much better
evalStageLS startValue n =
          let context  = fromEvalToContext startValue
              newStrat = evalStage startValue context
              in if n > 0 then newStrat : evalStageLS newStrat (n-1)
                          else [newStrat]

evalStageM ::
     List '[ (PriceSpace, State Player1N Observation PriceSpace), ( PriceSpace
                                                                , State Player2N Observation PriceSpace)]
  -> Int
  -> IO [List '[ (PriceSpace, State Player1N Observation PriceSpace), ( PriceSpace
                                                                    , State Player2N Observation PriceSpace)]]
evalStageM startValue 0 = pure []
evalStageM startValue n = do
  newStrat <-
    sequenceL
      (evalStage (hoist startValue) (fromEvalToContext (hoist startValue)))
  rest <- evalStageM newStrat (pred n)
  pure (newStrat : rest)

hoist ::
     Applicative f
  => List '[ (PriceSpace, State Player1N Observation PriceSpace), ( PriceSpace
                                                                , State Player2N Observation PriceSpace)]
  -> List '[ f (PriceSpace, State Player1N Observation PriceSpace), f ( PriceSpace
                                                                    , State Player2N Observation PriceSpace)]
hoist (x ::- y ::- Nil) = pure x ::- pure y ::- Nil

sequenceL ::
     Monad m
  => List '[ m (PriceSpace, State Player1N Observation PriceSpace), m (PriceSpace, State Player2N Observation PriceSpace)]
  -> m (List '[ (PriceSpace, State Player1N Observation PriceSpace), (PriceSpace, State Player2N Observation PriceSpace)])
sequenceL (x ::- y ::- Nil) = do
  v <- x
  v' <- y
  pure (v ::- v' ::- Nil)
