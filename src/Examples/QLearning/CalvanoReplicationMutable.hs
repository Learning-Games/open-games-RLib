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

module Examples.QLearning.CalvanoReplicationMutable
  ( evalStageLS
  , initialStrat
  , beta
  , actionSpace
  -- , csvParameters               --
  , exportQValuesJSON
  , sequenceL
  , evalStageM
  , PriceSpace(..)
  ) where
import           Data.Function
import qualified Data.Vector as V
import           Data.Vector (Vector)
import           Data.Bifunctor
import           Data.Hashable
import qualified Data.ByteString.Char8 as S8
import           Data.ByteString (ByteString)
import           Data.Array.IO
import           Debug.Trace
import           Control.DeepSeq
import           Data.Aeson
import           Data.Csv
import           Language.Haskell.TH

import qualified Data.List as L
import qualified Data.Ix as I
import qualified GHC.Arr as A
import           GHC.Generics
import qualified System.Random as Rand
import           System.Random

import           Engine.QLearningMutable
import           Engine.OpenGames
import           Engine.TLL
import           Engine.OpticClass
import           Preprocessor.AbstractSyntax
import           Preprocessor.Compile
import           Preprocessor.THSyntax

----------
-- 0 Types

data PriceSpace = PriceSpace {
   value :: !Double
  , idx :: !Int
  } deriving (Show, Eq, Ord)
  -- deriving (Generic,Random,Num,Fractional,Enum,Ord,Show,Eq,ToField,ToJSON,NFData, Hashable)
instance NFData PriceSpace where
  rnf x =
    let !_ = x -- PriceSpace is strict in its fields.
     in ()

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
  , expInitialObservation1 :: !Double
  , expInitialObservation2 :: !Double
  , expInitialPrice1 :: !Double
  , expInitialPrice2 :: !Double
  } deriving (Generic,Show)

instance ToNamedRecord ExportParameters
instance DefaultOrdered ExportParameters

-- | Instantiate the export parameters with the used variables
{-parameters = ExportParameters
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
  generatorEnv1
  generatorEnv2
  generatorPrice1
  generatorPrice2
  generatorObs1
  generatorObs2
  (createRandomPrice actionSpace generatorObs1)
  (createRandomPrice actionSpace generatorObs2)
  (createRandomPrice actionSpace generatorPrice1)
  (createRandomPrice actionSpace generatorPrice2)-}

-- | export to CSV
-- csvParameters = encodeDefaultOrderedByName  [parameters]

--instance ToNamedRecord ExportQValues
--instance DefaultOrdered ExportQValues

-------------------------
-- 2. Export Data as JSON
-- TODO need to deal with Rand.StdGen

-- 2.1. Parameters

-- 2.2. Q-values

data ExportQValues = ExportQValues
   { expName :: !Agent
   , expIteration :: !Int
   , expObs  :: !(Double,Double)
   , expQValues  :: ![((Observation Double,Double),Double)]
   } deriving (Generic,Show)

instance ToJSON ExportQValues

-- | Extract relevant information into a record to be exported
fromTLLToExport :: List '[IO (PriceSpace, Env PriceSpace), IO (PriceSpace, Env PriceSpace)] -> [ExportQValues]
fromTLLToExport (p1 ::- p2 ::- Nil) =
  undefined {-let (IO (_, env1)) = p1
      (IO (_, env2)) = p2
      name1 = _name env1
      name2 = _name env2
      expIteration1 = _iteration env1
      expIteration2 = _iteration env2
      expObs1 = _obsAgent env1
      expObs2 = _obsAgent env2
      expQValues1 = A.assocs $ _qTable env1
      expQValues2 = A.assocs $ _qTable env2
      expPlayer1 = ExportQValues name1 expIteration1 expObs1 expQValues1
      expPlayer2 = ExportQValues name2 expIteration2 expObs2 expQValues2
      in [expPlayer1,expPlayer2]-}


fromTLLListToExport :: [List '[IO (PriceSpace, Env PriceSpace), IO (PriceSpace, Env PriceSpace)]]-> [ExportQValues]
fromTLLListToExport = concatMap fromTLLToExport

exportQValuesJSON ls = foldable $ fromTLLListToExport ls

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
generatorObs1 = mkStdGen 2
generatorObs2 = mkStdGen 400
generatorEnv1 = 3
generatorEnv2 = 100
generatorPrice1 = mkStdGen 90
generatorPrice2 = mkStdGen 39

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
actionSpace :: V.Vector PriceSpace
actionSpace =
  V.imap
    (\idx value -> PriceSpace {value, idx})
    (V.fromList [lowerBound,lowerBound + dist .. upperBound])

-- derive possible observations
pricePairs = [(x,y) | x <- V.toList actionSpace, y <- V.toList actionSpace]

-- initiate a first fixed list of values at average
-- TODO change later to random values
lsValues  = [(((x,y),z),avg)| (x,y) <- xs, (z,_) <- xs]
  where  xs = pricePairs
         avg = (lowerBound + upperBound) / 2

-- initialArray
initialArray :: IO (QTable PriceSpace)
initialArray
  -- S8.putStrLn "Making initialArray"
 = do
  arr <- newArray_ (asIdx l, asIdx u)
  traverse
    (\(k, v) -> writeArray arr (asIdx k) v)
    lsValues
  pure arr
  where
    l = minimum $ fmap fst lsValues
    u = maximum $ fmap fst lsValues
    asIdx ((x,y),z) = ((toIdx x, toIdx y), toIdx z)

-- initiate the environment
initialEnv1  = initialArray>>= \arr-> pure (Env "Player1" arr 0 (decreaseFactor beta)  (Rand.mkStdGen generatorEnv1) initialObservation (5 * 0.999))
initialEnv2  = initialArray>>= \arr->  pure $ Env "Player2" (arr )  0 (decreaseFactor beta)  (Rand.mkStdGen generatorEnv2) initialObservation (5 * 0.999)

-----------------------------
-- 4. Constructing initial state

-- First observation, randomly determined
initialObservation :: Observation PriceSpace
initialObservation = (samplePopulation_ actionSpace generatorPrice1, samplePopulation_ actionSpace generatorPrice2)

-- Initiate strategy: start with random price
initialStrat :: IO (List '[IO (PriceSpace, Env PriceSpace), IO (PriceSpace, Env PriceSpace)])
initialStrat = do
  e1 <- initialEnv1
  e2 <- initialEnv2
  pure
    (pure (samplePopulation_ actionSpace generatorObs1, e1) ::-
     pure (samplePopulation_ actionSpace generatorObs2, e2) ::-
     Nil)


------------------------------
-- Updating state

toObs :: Monad m => m (a,Env a) -> m (a, Env a) -> m ((), (Observation a, Observation a))
toObs a1 a2 = do
             (act1,env1) <- a1
             (act2,env2) <- a2
             let obs1 = (act1,act2)
                 obs2 = (act2,act1)
                 in return ((),(obs1,obs2))

toObsFromLS :: Monad m => List '[m (a,Env a), m (a,Env a)] -> m ((),(Observation a,Observation a))
toObsFromLS (x ::- (y ::- Nil))= toObs x y


-- From the outputted list of strategies, derive the context
fromEvalToContext :: Monad m =>  List '[m (a,Env a), m (a,Env a)] ->
                     MonadContext m (Observation a, Observation a) () (a,a) ()
fromEvalToContext ls = MonadContext (toObsFromLS ls) (\_ -> (\_ -> pure ()))



------------------------------
-- Game stage
generateGame "stageSimple" ["beta'"]
                (Block ["state1"::String, "state2"] []
                [ Line [[|state1|]] [] [|pureDecisionQStage actionSpace "Player1" chooseExploreAction (chooseLearnDecrExploreQTable learningRate gamma (decreaseFactor beta'))|] ["p1"]  [[|(profit a0 a1 a2 p1 p2 mu c1, (p1,p2))|]]
                , Line [[|state2|]] [] [|pureDecisionQStage actionSpace "Player2" chooseExploreAction (chooseLearnDecrExploreQTable learningRate gamma (decreaseFactor beta'))|] ["p2"]  [[|(profit a0 a1 a2 p2 p1 mu c1, (p1,p2))|]]]
                [[|(p1, p2)|]] [])



----------------------------------
-- Defining the iterator structure
evalStage :: List '[IO (PriceSpace, Env PriceSpace), IO (PriceSpace, Env PriceSpace)]
             -> MonadContext
                  IO
                  (Observation PriceSpace, Observation PriceSpace)
                  ()
                  (PriceSpace, PriceSpace)
                  ()
             -> List '[IO (PriceSpace, Env PriceSpace), IO (PriceSpace, Env PriceSpace)]
evalStage = evaluate (stageSimple beta)


-- Explicit list constructor much better
evalStageLS startValue n =
          let context  = fromEvalToContext startValue
              newStrat = evalStage startValue context
              in if n > 0 then newStrat : evalStageLS newStrat (n-1)
                          else [newStrat]

evalStageM ::
     List '[ (PriceSpace, Env PriceSpace), (PriceSpace, Env PriceSpace)]
  -> Int
  -> IO [List '[ (PriceSpace, Env PriceSpace), (PriceSpace, Env PriceSpace)]]
evalStageM startValue 0 = pure []
evalStageM startValue n = do
  newStrat <-
    sequenceL
      (evalStage (hoist startValue) (fromEvalToContext (hoist startValue)))
  rest <- evalStageM newStrat (pred n)
  pure (newStrat : rest)

hoist ::
     Applicative f
  => List '[ (PriceSpace, Env PriceSpace), (PriceSpace, Env PriceSpace)]
  -> List '[ f (PriceSpace, Env PriceSpace), f (PriceSpace, Env PriceSpace)]
hoist (x ::- y ::- Nil) = pure x ::- pure y ::- Nil

sequenceL ::
     Monad m
  => List '[ m (PriceSpace, Env PriceSpace), m (PriceSpace, Env PriceSpace)]
  -> m (List '[ (PriceSpace, Env PriceSpace), (PriceSpace, Env PriceSpace)])
sequenceL (x ::- y ::- Nil) = do
  v <- x
  v' <- y
  pure (v ::- v' ::- Nil)
