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
                     , csvParameters
                     , exportQValuesJSON
                                          , sequenceL
                                          , evalStageM
                     )
                     where
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

newtype PriceSpace = PriceSpace Double
    deriving (Generic,Random,Num,Fractional,Enum,Ord,Show,Eq,ToField,ToJSON,NFData)


instance ToField Rand.StdGen

{-

Baseline:

benchmarking CalvanoReplication/iters/1000
time                 225.9 ms   (185.7 ms .. 284.4 ms)
                     0.974 R²   (0.934 R² .. 0.999 R²)
mean                 229.6 ms   (216.1 ms .. 247.0 ms)
std dev              19.94 ms   (13.85 ms .. 27.32 ms)
variance introduced by outliers: 16% (moderately inflated)

-}

-- make PriceSpace an instance of Ix so that the information can be used as an index for Array
-- TODO this is messy; needs to change
-- instance  I.Ix PriceSpace where
--   range (PriceSpace a, PriceSpace b) = [PriceSpace a, PriceSpace a + dist..PriceSpace b]
--   index (PriceSpace a, PriceSpace b)  (PriceSpace c) =
--     head $  L.elemIndices (PriceSpace c) [PriceSpace a, PriceSpace a + dist..PriceSpace b]
--   inRange (PriceSpace a, PriceSpace b)  (PriceSpace c) = a <= c && c <= b

instance  I.Ix PriceSpace where
  range (PriceSpace a, PriceSpace b) = [PriceSpace a, PriceSpace a + dist..PriceSpace b]
  index (PriceSpace a, PriceSpace b)  (PriceSpace c) = -- traceEvent "Ix.index" $

    head $ L.elemIndices (PriceSpace c) [PriceSpace a, PriceSpace a + dist..PriceSpace b]
  inRange (PriceSpace a, PriceSpace b)  (PriceSpace c) = {-traceEvent "Ix.inRange"-} a <= c && c <= b

type Price = Integer

type Index = Integer

------------------------
-- 1. Export Data as csv
-- 1.1. Parameters

data ExportParameters = ExportParameters
  { expKsi :: !PriceSpace
  , expBeta :: !ExploreRate
  , expDecreaseFactor :: !ExploreRate
  , expBertrandPrice :: !PriceSpace
  , expMonpolyPrice :: !PriceSpace
  , expGamma :: !DiscountFactor
  , expLearningRate :: !LearningRate
  , expMu :: !Double
  , expA1 :: !Double
  , expA2 :: !Double
  , expA0 :: !Double
  , expC1 :: !Double
  , expM  :: !PriceSpace
  , expLowerBound :: !PriceSpace
  , expUpperBound :: !PriceSpace
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
  generatorEnv1
  generatorEnv2
  generatorPrice1
  generatorPrice2
  generatorObs1
  generatorObs2
  (createRandomPrice actionSpace generatorObs1)
  (createRandomPrice actionSpace generatorObs2)
  (createRandomPrice actionSpace generatorPrice1)
  (createRandomPrice actionSpace generatorPrice2)

-- | export to CSV
csvParameters = encodeDefaultOrderedByName  [parameters]

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
   , expObs  :: !(PriceSpace,PriceSpace)
   , expQValues  :: ![((Observation PriceSpace,PriceSpace),Double)]
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

ksi :: PriceSpace
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
profit a0 a1 a2 (PriceSpace p1) (PriceSpace p2) mu c1 = (p1 - c1)* (demand a0 a1 a2 p1 p2 mu)

-- Fixing initial generators
generatorObs1 = 2
generatorObs2 = 400
generatorEnv1 = 3
generatorEnv2 = 100
generatorPrice1 = 90
generatorPrice2 = 39

------------------------------------------------------
-- Create index on the basis of the actual prices used
-- Also allows for different price types
-- Follows the concept in Calvano

-- creates the distance in the grid, takes parameter m as input to make the relevant number of steps
dist :: PriceSpace
dist =  (upperBound - lowerBound) / m



-- determine the bounds within which to search
lowerBound,upperBound :: PriceSpace
lowerBound = bertrandPrice - ksi*(monopolyPrice - bertrandPrice)
upperBound = monopolyPrice + ksi*(monopolyPrice - bertrandPrice)

priceBounds = (lowerBound,upperBound)

-----------------------------------------------------
-- Transforming bounds into the array and environment
-- create the action space
actionSpace = [lowerBound,lowerBound+dist..upperBound]

-- derive possible observations
pricePairs = [(x,y) | x <- actionSpace, y <- actionSpace]

-- initiate a first fixed list of values at average
-- TODO change later to random values
lsValues  = [(((x,y),z),avg)| (x,y) <- xs, (z,_) <- xs]
  where  xs = pricePairs
         PriceSpace avg = (lowerBound + upperBound) / 2

-- initialArray
initialArray :: IO (IOArray (Observation PriceSpace, PriceSpace) Double)
initialArray = do
  -- S8.putStrLn "Making initialArray"
  arr <- newArray_ (l, u)
  traverse (\(k,v) -> writeArray arr k v) lsValues
  pure arr
  where
    l = minimum $ fmap fst lsValues
    u = maximum $ fmap fst lsValues

-- initiate the environment
initialEnv1  = initialArray>>= \arr-> pure (Env "Player1" arr 0 (decreaseFactor beta)  (Rand.mkStdGen generatorEnv1) initialObservation (5 * 0.999))
initialEnv2  = initialArray>>= \arr->  pure $ Env "Player2" (arr )  0 (decreaseFactor beta)  (Rand.mkStdGen generatorEnv2) initialObservation (5 * 0.999)

-----------------------------
-- 4. Constructing initial state
-- First create a price randomly with seed
createRandomPrice :: [PriceSpace] -> Int -> PriceSpace
createRandomPrice support i =
  let  gen         = mkStdGen i
       (index,_) = randomR (0, (length support) - 1) gen
       in support !! index

-- First observation, randomly determined
initialObservation :: Observation PriceSpace
initialObservation = (createRandomPrice actionSpace generatorPrice1, createRandomPrice actionSpace generatorPrice2)

-- Initiate strategy: start with random price
initialStrat :: IO (List '[IO (PriceSpace, Env PriceSpace), IO (PriceSpace, Env PriceSpace)])
initialStrat = do
  e1 <- initialEnv1
  e2 <- initialEnv2
  pure
    (pure (createRandomPrice actionSpace generatorObs1, e1) ::-
     pure (createRandomPrice actionSpace generatorObs2, e2) ::-
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
