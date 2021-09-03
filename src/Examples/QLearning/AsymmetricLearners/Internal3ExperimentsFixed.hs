{-# LANGUAGE OverloadedStrings #-}
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

module Examples.QLearning.AsymmetricLearners.Internal3ExperimentsFixed
  where

import           Control.DeepSeq
import           Control.Monad.IO.Class
import qualified Data.Aeson as Aeson
import           Data.Array.IO as A
import qualified Data.Array.MArray as MA
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.Builder as SB
import           Data.Csv
import           Data.Double.Conversion.ByteString
import           Data.Foldable
import           Data.Hashable
import           Data.HashMap
import qualified Data.Ix as I
import qualified Data.Vector as V
import qualified Data.Vector.Sized as SV
import qualified Engine.Memory as Memory
import           Engine.OpenGames
import           Engine.OpticClass
import           Engine.QLearning
import qualified Engine.QLearning.ExportAsymmetric as ExportAsymmetric
import qualified Engine.QLearning.ExportAsymmetricLearners as ExportAsymmetricLearners
import           Engine.TLL
import           FastCsv
import           GHC.Generics
import           Path
import qualified Path.IO as IO
import           Preprocessor.Compile
import           RIO (RIO, GLogFunc, runRIO)
import           System.Random
import qualified System.Random as Rand
import           System.Process.Typed


-- In this module the setup for three players is fixed

------------------------------------
-- Configure observations and memory

-- WARNING: assumes players have same memory arity.
type M = RIO (GLogFunc (QLearningMsg Player1N Observation PriceSpace))
type Player1N = 1
type Player2N = 1

-- Mutual observation, fixes the number of players in the game
newtype Observation a = Obs
  { unObs :: (a, a)
  } deriving (Show,Generic,I.Ix,Ord, Eq, Functor, NFData, Aeson.ToJSON, Hashable)

-- Action space
-- Type is constructed as a product of actual price, _Double_, and an associated index, _Int_
data PriceSpace = PriceSpace {
   value :: !Double
  , idx :: !Int
  } deriving (Show, Eq, Ord, Generic)
-- Instances for export
instance Aeson.ToJSON PriceSpace where
  toJSON PriceSpace {value} = Aeson.toJSON value
instance NFData PriceSpace where
  rnf x =
    let !_ = x -- PriceSpace is strict in its fields.
    in ()
instance ToField PriceSpace where
  toField (PriceSpace {value, idx}) =
    toField value <> " [idx=" <> toField idx <> "]"
-- Instance for creating an index
instance ToIdx PriceSpace where
  toIdx PriceSpace {idx} = Idx idx

----------------------
-- Define parameters needed to specify the game
data Parameters = Parameters
  { pKsi :: Double
  , pBeta :: ExploreRate
  , pInitialExploreRate1 :: ExploreRate
  , pInitialExploreRate2 :: ExploreRate
  , pBertrandPrice1 :: Double
  , pMonopolyPrice1 :: Double
  , pBertrandPrice2 :: Double
  , pMonopolyPrice2 :: Double
  , pGamma :: DiscountFactor
  , pLearningRate :: LearningRate
  , pMu :: Double
  , pA1 :: Double
  , pA2 :: Double
  , pA0 :: Double
  , pC1 :: Double
  , pC2 :: Double
  , pM1  :: Double
  , pM2  :: Double
  , pGeneratorEnv1 :: StdGen
  , pGeneratorEnv2 :: StdGen
  , pGeneratorObs1 :: StdGen
  , pGeneratorObs2 :: StdGen
  } deriving (Generic,Show)

-- Configure QLearning
-- NOTE Identical for both players
configQL  Parameters {..} = ConfigQLearning
  chooseExploreAction
  (chooseLearnDecrExploreQTable pLearningRate pGamma pBeta)
  RewardExtendedExport

---------------
-- Export types

instance FastCsv.BuildCsvField (PriceSpace, PriceSpace) where
  {-# INLINE buildCsvField #-}
  buildCsvField (PriceSpace {value=p1},PriceSpace {value=p2}) =
    SB.byteString (toShortest p1) <> " " <>
    SB.byteString (toShortest p2)

instance FastCsv.BuildCsvField PriceSpace where
  buildCsvField (PriceSpace {value=p1}) =
    SB.byteString (toShortest p1)
  {-# INLINE buildCsvField #-}


-----------------------
-- Game characteristics
-- Demand follows eq 5 in Calvano et al.
demand :: Floating a => a -> a -> a -> a -> a -> a -> a
demand a0 a1 a2 p1 p2 mu = (exp 1.0)**((a1-p1)/mu) / agg
  where agg = (exp 1.0)**((a1-p1)/mu) + (exp 1.0)**((a2-p2)/mu) + (exp 1.0)**(a0/mu)

-- Profit function
profit :: Double -> Double -> Double -> PriceSpace -> PriceSpace -> Double -> Double -> Double
profit a0 a1 a2 (PriceSpace p1 _) (PriceSpace p2 _) mu c1 = (p1 - c1)* (demand a0 a1 a2 p1 p2 mu)

-- demand player 1
demand1 :: Parameters -> Double -> Double -> Double
demand1 Parameters {..} p1 p2 = (exp 1.0)**((pA1-p1)/pMu) / agg
  where agg = (exp 1.0)**((pA1-p1)/pMu) + (exp 1.0)**((pA2-p2)/pMu) + (exp 1.0)**(pA0/pMu)

-- demand player 2
demand2 :: Parameters -> Double -> Double -> Double
demand2 Parameters {..} p1 p2 = (exp 1.0)**((pA2-p2)/pMu) / agg
  where agg = (exp 1.0)**((pA1-p1)/pMu) + (exp 1.0)**((pA2-p2)/pMu) + (exp 1.0)**(pA0/pMu)

-- profit player 1
profit1 :: Parameters -> PriceSpace -> PriceSpace -> Double
profit1 par@Parameters {..} (PriceSpace p1 _) (PriceSpace p2 _) = (p1 - pC1)* (demand1 par p1 p2)

-- profit player 2
profit2 :: Parameters -> PriceSpace -> PriceSpace -> Double
profit2 par@Parameters {..} (PriceSpace p1 _) (PriceSpace p2 _) = (p2 - pC2)* (demand2 par p1 p2)


------------------------------------------------------
-- Create index on the basis of the actual prices used
-- Also allows for different price types
-- Follows the concept in Calvano

-- creates the distance in the grid, takes parameter m as input to make the relevant number of steps
dist1 :: Parameters -> Double
dist1 par =  (upperBound1 par - lowerBound1 par) / pM1 par

dist2 :: Parameters -> Double
dist2 par =  (upperBound2 par - lowerBound2 par) / pM2 par


-- determine the bounds within which to search
lowerBound1,upperBound1 :: Parameters -> Double
lowerBound1 Parameters{pBertrandPrice1,pKsi,pMonopolyPrice1} = pBertrandPrice1 - pKsi*(pMonopolyPrice1 - pBertrandPrice1)
upperBound1 Parameters{pBertrandPrice1,pKsi,pMonopolyPrice1} = pMonopolyPrice1 + pKsi*(pMonopolyPrice1 - pBertrandPrice1)

-- determine the bounds within which to search
lowerBound2,upperBound2 :: Parameters -> Double
lowerBound2 Parameters{pBertrandPrice2,pKsi,pMonopolyPrice2} = pBertrandPrice2 - pKsi*(pMonopolyPrice2 - pBertrandPrice2)
upperBound2 Parameters{pBertrandPrice2,pKsi,pMonopolyPrice2} = pMonopolyPrice2 + pKsi*(pMonopolyPrice2 - pBertrandPrice2)



-----------------------------------------------------
-- Transforming bounds into the array and environment
-- create the action space
actionSpace1 :: Parameters -> CTable PriceSpace
actionSpace1 par =
  uniformCTable
    (V.imap
       (\idx value -> PriceSpace {value, idx})
       -- ^ Uses the index of the vector and the price associated with it to map into _PriceSpace_
       (V.fromList [lowerBound1 par,lowerBound1 par + dist1 par .. upperBound1 par]))
       -- ^ Creates a vector for the price grid

actionSpace2 :: Parameters -> CTable PriceSpace
actionSpace2 par =
  uniformCTable
    (V.imap
       (\idx value -> PriceSpace {value, idx})
       -- ^ Uses the index of the vector and the price associated with it to map into _PriceSpace_
       (V.fromList [lowerBound2 par,lowerBound2 par + dist2 par .. upperBound2 par]))
       -- ^ Creates a vector for the price grid

-- derive possible observations
pricePairs :: Parameters -> [(PriceSpace, PriceSpace)]
pricePairs par =
  [ (x, y)
  | x <- V.toList (population $ actionSpace1 par)
  , y <- V.toList (population $ actionSpace2 par)
  ]


-- initiate a first fixed list of values at average for player 1
-- this assumes that player 2 randomizes uniformly
lsValues1 :: Parameters -> [(((PriceSpace, PriceSpace), PriceSpace), Double)]
lsValues1 par@Parameters{pGamma,pA0,pA1,pA2,pMu,pC1} = [(((x,y),z),value z)| (x,y) <- xs, (z,_) <- xs]
  where  xs = pricePairs par
         value p1 = (sum  $ fmap (\p2 -> profit pA0 pA1 pA2 p1 p2 pMu pC1) priceLs) / ((1 - pGamma) * (fromIntegral $ length priceLs))
         priceLs = V.toList (population $ actionSpace2 par)

-- initiate a first fixed list of values at average for player 2
-- this assumes that player 1 randomizes uniformly
lsValues2 :: Parameters -> [(((PriceSpace, PriceSpace), PriceSpace), Double)]
lsValues2 par@Parameters{pGamma,pA0,pA1,pA2,pMu,pC2} = [(((x,y),z),value z)| (x,y) <- xs, (z,_) <- xs]
  where  xs = pricePairs par
         value p2 = (sum  $ fmap (\p1 -> profit pA0 pA1 pA2 p1 p2 pMu pC2) priceLs) / ((1 - pGamma) * (fromIntegral $ length priceLs))
         priceLs = V.toList (population $ actionSpace1 par)



-- Initiate the environment given the parameters and an initial qmatrix for player 1
initialEnv1 :: Parameters -> IO (QTable Player1N Observation PriceSpace) -> M (Env Player1N Observation PriceSpace)
initialEnv1 par@Parameters{pInitialExploreRate1,pGeneratorEnv1} initialArray =
  liftIO initialArray >>= \arr ->
    pure $
      Env
         "Player1"
         1
         arr
         0
         pInitialExploreRate1
         pGeneratorEnv1
         (Memory.fromSV (SV.replicate (fmap toIdx (initialObservation par))))
         (5 * 0.999)
         "NothingHappenedYet"
         0
         (Memory.fromSV (SV.replicate (fmap toIdx (initialObservation par))))

-- Initiate the environment given the parameters and an initial qmatrix for player 2
initialEnv2 :: Parameters -> IO (QTable Player2N Observation PriceSpace) -> M (Env Player2N Observation PriceSpace)
initialEnv2 par@Parameters{pInitialExploreRate2,pGeneratorEnv2} initialArray =
  liftIO initialArray >>= \arr ->
    pure $
    Env
      "Player2"
      2
      (arr)
      0
      pInitialExploreRate2
      pGeneratorEnv2
      (Memory.fromSV (SV.replicate (fmap toIdx (initialObservation par))))
      (5 * 0.999)
      "NothingHappenedYet"
      0
      (Memory.fromSV (SV.replicate (fmap toIdx (initialObservation par))))

-- Create an initial qTable from the default values and parameters for player 1
-- Typical usage is when the env is created from scratch (and not reinitiated)
initialArray1 :: Parameters -> IO (QTable Player1N Observation PriceSpace)
initialArray1 par = do
  arr <- newArray_ (asIdx l, asIdx u)
  traverse_ (\(k, v) -> writeArray arr ( (asIdx k)) v) lsValues'
  pure arr
  where
    lsValues' = lsValues1 par
    l = minimum $ fmap fst lsValues'
    u = maximum $ fmap fst lsValues'
    asIdx ((x, y), z) = (Memory.fromSV (SV.replicate (Obs (toIdx x, toIdx y))), toIdx z)

-- Create an initial qTable from the default values and parameters for player 2
-- Typical usage is when the env is created from scratch (and not reinitiated)
initialArray2 :: Parameters -> IO (QTable Player2N Observation PriceSpace)
initialArray2 par = do
      arr <- newArray_ (asIdx l, asIdx u)
      traverse_ (\(k, v) -> writeArray arr (asIdx k) v) lsValues'
      pure arr
      where
        lsValues' = lsValues2 par
        l = minimum $ fmap fst lsValues'
        u = maximum $ fmap fst lsValues'
        asIdx ((x, y), z) = (Memory.fromSV (SV.replicate (Obs (toIdx x, toIdx y))), toIdx z)

-----------------------------
-- 4. Constructing initial state

-- First observation, randomly determined
-- NOTE: Dependency between initial strategy and initialobservation
initialObservation :: Parameters -> Observation PriceSpace
initialObservation par@Parameters{pGeneratorObs1,pGeneratorObs2} =
  Obs (samplePopulation_ (actionSpace1 par) pGeneratorObs1, samplePopulation_ (actionSpace2 par) pGeneratorObs2)

-- Initiate strategy: start with random price
initialStrat :: Parameters
             -> IO (QTable Player1N Observation PriceSpace)
             -> IO (QTable Player2N Observation PriceSpace)
             ->  M (List '[M (PriceSpace, Env Player1N Observation PriceSpace), M (PriceSpace, Env Player2N Observation PriceSpace)])
initialStrat par@Parameters{pGeneratorObs1,pGeneratorObs2} initialArr1 initialArr2= do
  e1 <- initialEnv1 par initialArr1
  e2 <- initialEnv2 par initialArr2
  pure
    (pure (samplePopulation_ (actionSpace1 par) pGeneratorObs1, e1) ::-
     pure (samplePopulation_ (actionSpace2 par) pGeneratorObs2, e2) ::-
     Nil)


------------------------------
-- Updating state

toObs :: Monad m => m (a,Env Player1N Observation a) -> m (a, Env Player2N Observation a) -> m ((), (Observation a, Observation a))
toObs a1 a2 = do
             (act1,_env1) <- a1
             (act2,_env2) <- a2
             let obs = Obs (act1,act2)
                 in return ((),(obs,obs))

toObsFromLS :: Monad m => List '[m (a,Env Player1N Observation a), m (a,Env Player2N Observation a)] -> m ((),(Observation a,Observation a))
toObsFromLS (x ::- (y ::- Nil))= toObs x y


-- From the outputted list of strategies, derive the context
fromEvalToContext :: Monad m =>  List '[m (a,Env Player1N Observation a), m (a,Env Player2N Observation a)] ->
                     MonadContext m (Observation a, Observation a) () (a,a) ()
fromEvalToContext ls = MonadContext (toObsFromLS ls) (\_ -> (\_ -> pure ()))



------------------------------
-- Game stage
stageSimple :: Parameters -> OpenGame (MonadOptic M) (MonadContext M) ('[M (PriceSpace, Env Player1N Observation PriceSpace), M (PriceSpace, Env Player1N Observation PriceSpace)] +:+ '[]) ('[M (PriceSpace, Env Player1N Observation PriceSpace), M (PriceSpace, Env Player1N Observation PriceSpace)] +:+ '[]) (Observation PriceSpace, Observation PriceSpace) () (PriceSpace, PriceSpace) ()
stageSimple par@Parameters {..} = [opengame|
   inputs    : (state1,state2) ;
   feedback  :      ;

   :-----------------:
   inputs    :  state1    ;
   feedback  :      ;
   operation : pureDecisionQStage (configQL par) (actionSpace1 par) "Player1" ;
   outputs   :  p1 ;
   returns   :  (profit1 par p1 p2, Obs (p1,p2)) ;

   inputs    : state2     ;
   feedback  :      ;
   operation : pureDecisionQStage (configQL par) (actionSpace2 par) "Player2"  ;
   outputs   :  p2 ;
   returns   :  (profit2 par p1 p2, Obs (p1,p2))    ;
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
                                                                        , Env Player2N Observation PriceSpace)] -> M (Decision s))
  -> List '[ (PriceSpace, Env Player1N Observation PriceSpace), ( PriceSpace
                                                                , Env Player2N Observation PriceSpace)]
  -> Int
  -> s
  -> M ()
mapStagesM_ par f startValue n0 s0 = go s0 startValue n0
  where
     goInitial s value n0 = do
      newStrat <-
        sequenceL (evalStage par (hoist value) (fromEvalToContext (hoist value)))
      decision <- f s newStrat
      case decision of
        Continue s' -> go s' newStrat (pred n0)
        Stop -> pure ()
     go _ _ 0 = pure ()
     go s value !n = do
      let ((p1,env1) ::- (p2,env2) ::- Nil) = value
          mem1      = (_obsAgentPrevious env1)
          index1    = (mem1, toIdx p1)
          table1    = _qTable env1
          newValue1 = _stageNewValue env1
          mem2      = (_obsAgentPrevious env2)
          index2    = (mem2, toIdx p2)
          table2    = _qTable env2
          newValue2 = _stageNewValue env2
      liftIO $ A.writeArray table1 index1 newValue1
      liftIO $ A.writeArray table2 index2 newValue2
      newStrat <-
        sequenceL (evalStage par (hoist value) (fromEvalToContext (hoist value)))
      decision <- f s newStrat
      case decision of
        Continue s' -> go s' newStrat (pred n)
        Stop -> pure ()


{-# INLINE mapStagesMFinalResult #-}
mapStagesMFinalResult ::
  Parameters
  -> (s ->  List '[ (PriceSpace, Env Player1N Observation PriceSpace), ( PriceSpace
                                                                        , Env Player2N Observation PriceSpace)] -> M (Decision s))
  -> List '[ (PriceSpace, Env Player1N Observation PriceSpace), ( PriceSpace
                                                                , Env Player2N Observation PriceSpace)]
  -> Int
  -> s
  -> M (List '[ (PriceSpace, Env Player1N Observation PriceSpace), ( PriceSpace
                                                                        , Env Player2N Observation PriceSpace)])
mapStagesMFinalResult par f startValue n0 s0 = go s0 startValue n0
  where
     goInitial s value n0 = do
      newStrat <-
        sequenceL (evalStage par (hoist value) (fromEvalToContext (hoist value)))
      decision <- f s newStrat
      case decision of
        Continue s' -> go s' newStrat (pred n0)
        Stop -> pure value
     go _ value 0 = pure value
     go s value !n = do
      let ((p1,env1) ::- (p2,env2) ::- Nil) = value
          mem1      = (_obsAgentPrevious env1)
          index1    = (mem1, toIdx p1)
          table1    = _qTable env1
          newValue1 = _stageNewValue env1
          mem2      = (_obsAgentPrevious env2)
          index2    = (mem2, toIdx p2)
          table2    = _qTable env2
          newValue2 = _stageNewValue env2
      liftIO $ A.writeArray table1 index1 newValue1
      liftIO $ A.writeArray table2 index2 newValue2
      newStrat <-
        sequenceL (evalStage par (hoist value) (fromEvalToContext (hoist value)))
      decision <- f s newStrat
      case decision of
        Continue s' -> go s' newStrat (pred n)
        Stop -> pure value




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


--------------------------------------------------------------------
-- Experiment comprised of a number of runs of a given specification
--------------------------------------------------------------------
-- Specify directories

-- File names for results
parametersFile          = [relfile|parameters.csv|]
rewardsExtendedFile     = [relfile|rewardsExtended.csv|]
rewardsExtendedEndNFile = [relfile|rewardsExtendedEndNLines.csv|]



-- One single run for both experiments and then rematched
executeAndRematchSingleRun name parameters1 parameters2 parameters3 exportConfigGameLearning keepOnlyNLastIterations p1e1p2e2 p1e1p2e3 p1e2p2e1 p1e2p2e3 p1e3p2e1 p1e3p2e2 p1e1p2e1 p1e2p2e2 p1e3p2e3 exportConfigGameRematching = do
  expLowC  <- firstStageLearning name parameters1 exportConfigGameLearning
  -- ^ Instantiate run with low costs
  expMedC  <- firstStageLearning name parameters2 exportConfigGameLearning
  -- ^ Instantiate run with medium costs
  expHighC <- firstStageLearning name parameters3 exportConfigGameLearning
  -- ^ Instantiate run with high costs
  rematchedLearning name keepOnlyNLastIterations p1e1p2e2 p1e1p2e3 p1e2p2e1 p1e2p2e3 p1e3p2e1 p1e3p2e2 p1e1p2e1 p1e2p2e2 p1e3p2e3 exportConfigGameRematching  expLowC expMedC expHighC
  -- ^ Rematching

------------------------
-- Single learning stage
------------------------
-- NOTE the difference is the input by the initial games parameters _parametersGameLowC_
firstStageLearning :: String
                   -> (StdGen -> StdGen -> StdGen -> StdGen -> Parameters)
                   -- ^ The specific parameter function for that learning run
                   -> (String
                         -> Parameters
                         -> ExportAsymmetricLearners.ExportConfig
                              Player1N
                              Observation
                              PriceSpace
                              (RIO.RIO
                                (RIO.GLogFunc
                                    (QLearningMsg
                                      Player1N Observation PriceSpace))))
                   -- ^ The specific run configuration for that learning run
                   -> IO (QTable
                           Player1N Observation PriceSpace,
                        QTable
                           Player2N Observation PriceSpace)
firstStageLearning name parametersFunction exportConfigFunction  = do
  gEnv1   <- newStdGen
  gEnv2   <- newStdGen
  gObs1   <- newStdGen
  gObs2   <- newStdGen
  let parameters = parametersFunction gEnv1 gEnv2 gObs1 gObs2
      exportConfig = exportConfigFunction name parameters
  list <- ExportAsymmetricLearners.runQLearningExportingDiagnostics exportConfig
  let (x1 ::- x2 ::- Nil) = list
      (_,env1) = x1
      (_,env2) = x2
      q1       = _qTable env1
      q2       = _qTable env2
  pure (q1,q2)
----------------------------------
-- Single rematched Learning stage
----------------------------------
-- Takes the information from the learning phase runs (here two different experiments) and computes the exploration output
rematchedLearning :: String
                  -> Int
                  -> (StdGen -> StdGen -> StdGen -> StdGen -> Parameters)
                  -> (StdGen -> StdGen -> StdGen -> StdGen -> Parameters)
                  -> (StdGen -> StdGen -> StdGen -> StdGen -> Parameters)
                  -> (StdGen -> StdGen -> StdGen -> StdGen -> Parameters)
                  -> (StdGen -> StdGen -> StdGen -> StdGen -> Parameters)
                  -> (StdGen -> StdGen -> StdGen -> StdGen -> Parameters)
                  -> (StdGen -> StdGen -> StdGen -> StdGen -> Parameters)
                  -> (StdGen -> StdGen -> StdGen -> StdGen -> Parameters)
                  -> (StdGen -> StdGen -> StdGen -> StdGen -> Parameters)
                  -> (String
                      -> Parameters
                      -> IO
                            (QTable
                              Player1N Observation PriceSpace)
                      -> IO
                            (QTable
                              Player2N Observation PriceSpace)
                      -> ExportAsymmetric.ExportConfig
                            Player1N
                            Observation
                            PriceSpace
                            (RIO.RIO
                              (RIO.GLogFunc
                                  (QLearningMsg
                                    Player1N Observation PriceSpace))))

                  -> (QTable
                              Player1N Observation PriceSpace,
                          QTable
                              Player2N Observation PriceSpace)
                  -> (QTable
                              Player1N Observation PriceSpace,
                          QTable
                              Player2N Observation PriceSpace)
                  -> (QTable
                              Player1N Observation PriceSpace,
                          QTable
                              Player2N Observation PriceSpace)
                  -> IO ()
rematchedLearning name keepOnlyNLastIterations p1e1p2e2 p1e1p2e3 p1e2p2e1 p1e2p2e3 p1e3p2e1 p1e3p2e2 p1e1p2e1 p1e2p2e2 p1e3p2e3 exportConfigGameRematching (x1,x2) (y1,y2) (z1,z2)  = do
  x1'      <- copyArray x1
  x1''     <- copyArray x1
  y1'      <- copyArray y1
  y1''     <- copyArray y1
  z1'      <- copyArray z1
  z1''     <- copyArray z1
  x2'      <- copyArray x2
  x2''     <- copyArray x2
  y2'      <- copyArray y2
  y2''     <- copyArray y2
  z2'      <- copyArray z2
  z2''     <- copyArray z2
  pairing2 ("p1e1p2e2_run" ++ name) keepOnlyNLastIterations p1e1p2e2 exportConfigGameRematching (x1,y2)
  pairing2 ("p1e1p2e3_run" ++ name) keepOnlyNLastIterations p1e1p2e3 exportConfigGameRematching (x1',z2)
  pairing2 ("p1e2p2e1_run" ++ name) keepOnlyNLastIterations p1e2p2e1 exportConfigGameRematching (y1,x2)
  pairing2 ("p1e2p2e3_run" ++ name) keepOnlyNLastIterations p1e2p2e3 exportConfigGameRematching (y1',z2')
  pairing2 ("p1e3p2e1_run" ++ name) keepOnlyNLastIterations p1e3p2e1 exportConfigGameRematching (z1,x2')
  pairing2 ("p1e3p2e2_run" ++ name) keepOnlyNLastIterations p1e3p2e2 exportConfigGameRematching (z1',y2')
  pairing2 ("p1e1p2e1_run" ++ name) keepOnlyNLastIterations p1e1p2e1 exportConfigGameRematching (x1'',x2'')
  pairing2 ("p1e2p2e2_run" ++ name) keepOnlyNLastIterations p1e2p2e2 exportConfigGameRematching (y1'',y2'')
  pairing2 ("p1e3p2e3_run" ++ name) keepOnlyNLastIterations p1e3p2e3 exportConfigGameRematching (z1'',z2'')


-- Copy original array to a new location so that we do not affect the original array when computing on the copy
copyArray :: QTable Player1N Observation PriceSpace -> IO (QTable Player1N Observation PriceSpace)
copyArray x = do
  bounds <- MA.getBounds x
  elems  <- MA.getElems  x
  MA.newListArray bounds elems


-- Reinitiates the exploitation phase for the starting conditions of two players
pairing2 :: String
         -> Int
         -> (StdGen -> StdGen -> StdGen -> StdGen ->  Parameters)
         -- ^ Parameters specific for the rematching players
         -> (String
            -> Parameters
            -> IO
                  (QTable
                    Player1N Observation PriceSpace)
            -> IO
                  (QTable
                    Player2N Observation PriceSpace)
            -> ExportAsymmetric.ExportConfig
                  Player1N
                  Observation
                  PriceSpace
                  (RIO.RIO
                    (RIO.GLogFunc
                        (QLearningMsg
                          Player1N Observation PriceSpace))))
         -- ^ Parameters for the rematched runs
         -> (QTable
                  Player1N Observation PriceSpace,
              QTable
                  Player2N Observation PriceSpace)
         -- ^ Relevant restarting conditions for players
         -> IO ()
pairing2 name keepOnlyNLastIterations parametersGameRematchingFunction exportConfigGameRematchingFunction (qt1,qt2) = do
  gEnv1   <- newStdGen
  gEnv2   <- newStdGen
  gObs1   <- newStdGen
  gObs2   <- newStdGen
  let newParameters = parametersGameRematchingFunction gEnv1 gEnv2 gObs1 gObs2
      newExportConfig = exportConfigGameRematchingFunction name newParameters (pure qt1) (pure qt2)
  dirResultIteration <- parseRelDir name
  IO.createDirIfMissing True dirResultIteration
  L.writeFile (fromRelDir  (dirResultIteration </> parametersFile)) (csvParameters newParameters)
  ExportAsymmetric.runQLearningExportingDiagnostics newExportConfig
  runProcess_
    (proc
        "touch"
        [fromRelDir (dirResultIteration </> rewardsExtendedEndNFile)]
       )
  runProcess_
    (shell
        ( "tail "
         ++ "-"
         ++ (show $  keepOnlyNLastIterations * ExportAsymmetric.players newExportConfig)
         ++ " "
         ++ fromRelDir (dirResultIteration </> rewardsExtendedFile)
         ++ " > "
         ++ fromRelDir (dirResultIteration </> rewardsExtendedEndNFile)
        ))
  putStrLn ("completed task: " ++ name)



------------------------
-- Export extended config data  as csv
-- Parameters

data ExportParameters = ExportParameters
  { expKsi :: !Double
  , expBeta :: !ExploreRate
  , expInitialExploreRate1 :: !ExploreRate
  , expInitialExploreRate2 :: !ExploreRate
  , expDecreaseFactor :: !ExploreRate
  , expBertrandPrice1 :: !Double
  , expBertrandPrice2 :: !Double
  , expMonpolyPrice1 :: !Double
  , expMonpolyPrice2 :: !Double
  , expGamma :: !DiscountFactor
  , expLearningRate :: !LearningRate
  , expMu :: !Double
  , expA1 :: !Double
  , expA2 :: !Double
  , expA0 :: !Double
  , expC1 :: !Double
  , expC2 :: !Double
  , expM1  :: !Double
  , expM2  :: !Double
  , expLowerBound1 :: !Double
  , expLowerBound2 :: !Double
  , expUpperBound1 :: !Double
  , expUpperBound2 :: !Double
  , expGeneratorEnv1 :: !String
  , expGeneratorEnv2 :: !String
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
  (pInitialExploreRate1 par)
  (pInitialExploreRate2 par)
  (pBeta par)
  (pBertrandPrice1 par)
  (pBertrandPrice2 par)
  (pMonopolyPrice1 par)
  (pMonopolyPrice2 par)
  (pGamma par)
  (pLearningRate par)
  (pMu par)
  (pA1 par)
  (pA2 par)
  (pA0 par)
  (pC1 par)
  (pC2 par)
  (pM1 par)
  (pM2 par)
  (lowerBound1 par)
  (lowerBound2 par)
  (upperBound1 par)
  (upperBound2 par)
  (show $ pGeneratorEnv1 par)
  (show $ pGeneratorEnv2 par)
  (show $ pGeneratorObs1 par)
  (show $ pGeneratorObs2 par)
  (samplePopulation_ (actionSpace1 par) (pGeneratorObs1 par))
  (samplePopulation_ (actionSpace2 par) (pGeneratorObs2 par))
  (samplePopulation_ (actionSpace1 par) (pGeneratorObs1 par))
  (samplePopulation_ (actionSpace2 par) (pGeneratorObs2 par))

-- | export to CSV
csvParameters :: Parameters -> L.ByteString
csvParameters par = encodeDefaultOrderedByName  [exportParameters par]





