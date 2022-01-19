{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE NamedFieldPuns #-}
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

module Examples.QLearning.OwnPriceObservation.Internal
  where

import           Control.DeepSeq
import           Control.Monad.IO.Class
import qualified Data.Aeson as Aeson
import           Data.Array.IO as A
import qualified Data.Array.MArray as MA
import qualified Data.Array.IArray as IA
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
import qualified Engine.QLearning.ExportAsymmetricLearnersOwnPrice as ExportAsymmetricLearners
import           Engine.TLL
import           FastCsv
import           GHC.Generics
import           Path
import qualified Path.IO as IO
import           Preprocessor.Compile
import           RIO (RIO, GLogFunc, runRIO, when)
import           System.Random
import qualified System.Random as Rand
import           System.Process.Typed

--------------------------------------------------------------------------------------------------------
-- This file contains the specification for a repeated matching of learning agents
-- The exact definition of the learning and matching scenario happens in the main file of the experiment



------------------------------------
-- Configure observations and memory

-- WARNING: assumes players have same memory arity.
type M = RIO (GLogFunc (QLearningMsg Player1N Observation PriceSpace))
type Player1N = 1
type Player2N = 1

-- Immutable array; needed for reproducing a fresh copy of the mutable array
type QTableIm n o a = IA.Array (Memory.Vector n (o (Idx a)), Idx a) Double

-- Match type with information about the experiment string

data ReMatchType = ReMatchType
  { experiment1 :: String
  , experiment2 :: String
  , randomMatch :: Bool
  } deriving (Show,Eq)

-- For rematch with exogenous strategy
data ReMatchTypeExog = ReMatchTypeExog
  { experiment1Ex :: String
  , experiment2Ex :: String
  , priceSpaceEx  :: Int
  , randomMatchEx :: Bool
  } deriving (Show,Eq)

-- Mutual observation, fixes the number of players in the game
newtype Observation a = Obs
  { unObs :: a
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



configQLNoLearning Parameters {..} = ConfigQLearning
  chooseNoExploreAction
  (chooseNoLearnDecrExploreQTable pLearningRate pGamma pBeta)
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
pricePairs :: Parameters -> [PriceSpace]
pricePairs par =
  V.toList (population $ actionSpace1 par)



-- initiate a first fixed list of values at average for player 1
-- this assumes that player 2 randomizes uniformly
lsValues1 :: Parameters -> [((PriceSpace, PriceSpace), Double)]
lsValues1 par@Parameters{pGamma} = [((x,z),value z)| x <- xs, z <- xs]
  where  xs = pricePairs par
         value p1 = (sum  $ fmap (\p2 -> profit2 par p1 p2) priceLs) / ((1 - pGamma) * (fromIntegral $ length priceLs))
         priceLs = V.toList (population $ actionSpace2 par)

-- initiate a first fixed list of values at average for player 2
-- this assumes that player 1 randomizes uniformly
lsValues2 :: Parameters -> [((PriceSpace, PriceSpace), Double)]
lsValues2 par@Parameters{pGamma} = [((x,z),value z)| x <- xs, z <- xs]
  where  xs = pricePairs par
         value p2 = (sum  $ fmap (\p1 -> profit1 par p1 p2) priceLs) / ((1 - pGamma) * (fromIntegral $ length priceLs))
         priceLs = V.toList (population $ actionSpace1 par)



-- Initiate the environment given the parameters and an initial qmatrix for player 1
initialEnv1 :: Parameters -> IO (QTable Player1N Observation PriceSpace) -> Observation PriceSpace -> M (Env Player1N Observation PriceSpace)
initialEnv1 par@Parameters{pInitialExploreRate1,pGeneratorEnv1} initialArray initialObs =
  liftIO initialArray >>= \arr ->
    pure $
      Env
         "Player1"
         1
         arr
         0
         pInitialExploreRate1
         pGeneratorEnv1
         (Memory.fromSV (SV.replicate (fmap toIdx initialObs)))
         (5 * 0.999)
         "NothingHappenedYet"
         0
         (Memory.fromSV (SV.replicate (fmap toIdx initialObs)))

-- Initiate the environment given the parameters and an initial qmatrix for player 2
initialEnv2 :: Parameters -> IO (QTable Player2N Observation PriceSpace) -> Observation PriceSpace -> M (Env Player2N Observation PriceSpace)
initialEnv2 par@Parameters{pInitialExploreRate2,pGeneratorEnv2} initialArray initialObs=
  liftIO initialArray >>= \arr ->
    pure $
    Env
      "Player2"
      2
      (arr)
      0
      pInitialExploreRate2
      pGeneratorEnv2
      (Memory.fromSV (SV.replicate (fmap toIdx initialObs)))
      (5 * 0.999)
      "NothingHappenedYet"
      0
      (Memory.fromSV (SV.replicate (fmap toIdx initialObs)))

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
    asIdx (x, z) = (Memory.fromSV (SV.replicate (Obs (toIdx x))), toIdx z)

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
        asIdx (x, z) = (Memory.fromSV (SV.replicate (Obs (toIdx x))), toIdx z)

-----------------------------
-- 4. Constructing initial state
-- First observation, randomly determined
randomInitialObservation :: Parameters -> (Observation PriceSpace, Observation PriceSpace)
randomInitialObservation par@Parameters{pGeneratorObs1, pGeneratorObs2} =
  (Obs (samplePopulation_ (actionSpace1 par) pGeneratorObs1), Obs (samplePopulation_ (actionSpace2 par) pGeneratorObs2))


-- Initiate strategy: start with two qMatrices and the common initial observation
initialStrat :: Parameters
             -> IO (QTable Player1N Observation PriceSpace)
             -> IO (QTable Player2N Observation PriceSpace)
             -> (Observation PriceSpace, Observation PriceSpace)
             ->  M (List '[M (PriceSpace, Env Player1N Observation PriceSpace), M (PriceSpace, Env Player2N Observation PriceSpace)])
initialStrat par@Parameters{pGeneratorObs1,pGeneratorObs2} initialArr1 initialArr2 (initialObs1, initialObs2)= do
  e1 <- initialEnv1 par initialArr1 initialObs1
  e2 <- initialEnv2 par initialArr2 initialObs2
  pure
    (pure (samplePopulation_ (actionSpace1 par) pGeneratorObs1, e1) ::-
     pure (samplePopulation_ (actionSpace2 par) pGeneratorObs2, e2) ::-
     Nil)

------------------------------
-- Updating state
-- From both outputs of players construct the next observation
toObs :: Monad m => m (a,Env Player1N Observation a) -> m (a, Env Player2N Observation a) -> m ((), (Observation a, Observation a))
toObs a1 a2 = do
             (act1,_env1) <- a1
             (act2,_env2) <- a2
             let obs1 = Obs act1
                 obs2 = Obs act2
                 in return ((),(obs1,obs2))

-- From the TLL of outputs (coming out of _evaluate_) construct new observation
toObsFromLS :: Monad m => List '[m (a,Env Player1N Observation a), m (a,Env Player2N Observation a)] -> m ((),(Observation a,Observation a))
toObsFromLS (x ::- (y ::- Nil))= toObs x y


-- From the outputted list of strategies, derive the context
fromEvalToContext :: Monad m =>  List '[m (a,Env Player1N Observation a), m (a,Env Player2N Observation a)] ->
                     MonadContext m (Observation a, Observation a) () (a,a) ()
fromEvalToContext ls = MonadContext (toObsFromLS ls) (\_ -> (\_ -> pure ()))



------------------------------
-- Game stage
stageSimple :: (Parameters -> ConfigQLearning Player1N Observation PriceSpace M)
            -> (Parameters -> ConfigQLearning Player2N Observation PriceSpace M)
            -> Parameters
            -> OpenGame (MonadOptic M) (MonadContext M) ('[M (PriceSpace, Env Player1N Observation PriceSpace), M (PriceSpace, Env Player1N Observation PriceSpace)] +:+ '[]) ('[M (PriceSpace, Env Player1N Observation PriceSpace), M (PriceSpace, Env Player1N Observation PriceSpace)] +:+ '[]) (Observation PriceSpace, Observation PriceSpace) () (PriceSpace, PriceSpace) ()
stageSimple configQLearningSpec1 configQLearningSpec2 par@Parameters {..} = [opengame|
   inputs    : (state1,state2) ;
   feedback  :      ;

   :-----------------:
   inputs    :  state1    ;
   feedback  :      ;
   operation : pureDecisionQStage (configQLearningSpec1 par) (actionSpace1 par) "Player1" ;
   outputs   :  p1 ;
   returns   :  (profit1 par p1 p2, Obs p1) ;

   inputs    : state2     ;
   feedback  :      ;
   operation : pureDecisionQStage (configQLearningSpec2 par) (actionSpace2 par) "Player2"  ;
   outputs   :  p2 ;
   returns   :  (profit2 par p1 p2, Obs p2)    ;
   :-----------------:

   outputs   :  (p1, p2)    ;
   returns   :      ;

|]

----------------------------------
-- Defining the iterator structure
-- Given a strategy and a context, evaluate the simple game
evalStage :: (Parameters -> ConfigQLearning Player1N Observation PriceSpace M)
          -> (Parameters -> ConfigQLearning Player2N Observation PriceSpace M)
          -> Parameters
          -> List '[ M (PriceSpace, Env Player1N Observation PriceSpace), M ( PriceSpace
                                                                              , Env Player2N Observation PriceSpace)]
          -> MonadContext M (Observation PriceSpace, Observation PriceSpace) () ( PriceSpace
                                                                                , PriceSpace) ()
          -> List '[ M (PriceSpace, Env Player1N Observation PriceSpace), M ( PriceSpace
                                                                              , Env Player2N Observation PriceSpace)]
evalStage configQLearningSpec1 configQLearningSpec2 par = evaluate (stageSimple configQLearningSpec1 configQLearningSpec2 par)

{-# INLINE mapStagesM_ #-}
mapStagesM_ :: (Parameters -> ConfigQLearning Player1N Observation PriceSpace M)
            -> (Parameters -> ConfigQLearning Player2N Observation PriceSpace M)
            -> Parameters
            -> (s ->  List '[ (PriceSpace, Env Player1N Observation PriceSpace), ( PriceSpace
                                                                                  , Env Player2N Observation PriceSpace)] -> M (Decision s))
            -> List '[ (PriceSpace, Env Player1N Observation PriceSpace), ( PriceSpace
                                                                          , Env Player2N Observation PriceSpace)]
            -> Int
            -> s
            -> M ()
mapStagesM_ configQLearningSpec1 configQLearningSpec2 par f startValue n0 s0 = go s0 startValue n0
  where
     goInitial s value n0 = do
      newStrat <-
        sequenceL (evalStage configQLearningSpec1 configQLearningSpec2 par (hoist value) (fromEvalToContext (hoist value)))
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
        sequenceL (evalStage configQLearningSpec1 configQLearningSpec2 par (hoist value) (fromEvalToContext (hoist value)))
      decision <- f s newStrat
      case decision of
        Continue s' -> go s' newStrat (pred n)
        Stop -> pure ()

-- Same as mapStagesM_ but keeps the final result in tact for reuse
-- TODO unify with mapStagesM_
{-# INLINE mapStagesMFinalResult #-}
mapStagesMFinalResult :: (Parameters -> ConfigQLearning Player1N Observation PriceSpace M)
                      -> (Parameters -> ConfigQLearning Player2N Observation PriceSpace M)
                      -> Parameters
                      -> (s ->  List '[ (PriceSpace, Env Player1N Observation PriceSpace), ( PriceSpace
                                                                                            , Env Player2N Observation PriceSpace)] -> M (Decision s))
                      -> List '[ (PriceSpace, Env Player1N Observation PriceSpace), ( PriceSpace
                                                                                    , Env Player2N Observation PriceSpace)]
                      -> Int
                      -> s
                      -> M (List '[ (PriceSpace, Env Player1N Observation PriceSpace), ( PriceSpace
                                                                                            , Env Player2N Observation PriceSpace)])
mapStagesMFinalResult configQLearningSpec1 configQLearningSpec2 par f startValue n0 s0 = go s0 startValue n0
  where
     goInitial s value n0 = do
      newStrat <-
        sequenceL (evalStage configQLearningSpec1 configQLearningSpec2 par (hoist value) (fromEvalToContext (hoist value)))
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
        sequenceL (evalStage configQLearningSpec1 configQLearningSpec2 par (hoist value) (fromEvalToContext (hoist value)))
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
seedsFile               = [relfile|seeds.csv|]
rewardsExtendedFile     = [relfile|rewardsExtended.csv|]
rewardsExtendedEndNFile = [relfile|rewardsExtendedEndNLines.csv|]


------------------------
-- Single learning stage
------------------------
-- For a specific experiment rematchingType produce the q table map
firstStageLearningMap :: String
                      -- ^ name
                      -> ExportAsymmetricLearners.RunNumber
                      -- ^ The number of the run for the same estimation
                      -> Int
                      -- ^ number of last iterations
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
                      -- ^ The specific experiment configuration for that learning run
                      -> Map String (StdGen -> StdGen -> StdGen -> StdGen -> Parameters)
                      -- ^ The specific parameter function for that experiment
                      -> String
                      -- ^ The experiment string
                      -> IO (Map String ((QTable
                                          Player1N Observation PriceSpace), Observation PriceSpace)
                              , Map String ((QTable
                                          Player2N Observation PriceSpace), Observation PriceSpace))
firstStageLearningMap name runNo keepOnlyNLastIterations exportConfigFunction parametersMap exp= do
   let parametersFunction = parametersMap ! exp
   ((q1,q2),(lastObs1,lastObs2)) <- firstStageLearning (exp ++ name) runNo keepOnlyNLastIterations exportConfigFunction parametersFunction
   pure $ (fromList [(exp,(q1,lastObs1))],fromList [(exp,(q2,lastObs2))])



-- NOTE the difference is the input by the initial games parameters _parametersGameLowC_
{-# INLINE firstStageLearning #-}
firstStageLearning :: String
                    -- ^ Name of the run
                    -> ExportAsymmetricLearners.RunNumber
                    -- ^ The number of the run for the same estimation
                    -> Int
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
                    -> (StdGen -> StdGen -> StdGen -> StdGen -> Parameters)
                    -- ^ The specific parameter function for that learning run
                    -> IO ((QTable
                            Player1N Observation PriceSpace,
                          QTable
                            Player2N Observation PriceSpace), (Observation PriceSpace, Observation PriceSpace))
firstStageLearning name runNo keepOnlyNLastIterations exportConfigFunction parametersFunction = do
  gEnv1   <- newStdGen
  gEnv2   <- newStdGen
  gObs1   <- newStdGen
  gObs2   <- newStdGen
  let parameters = parametersFunction gEnv1 gEnv2 gObs1 gObs2
      exportConfig = exportConfigFunction name parameters
  dirResultIteration <- parseRelDir name
  IO.createDirIfMissing True dirResultIteration
  L.writeFile (fromRelDir  (dirResultIteration </> seedsFile)) (csvSeeds parameters)
  L.writeFile (fromRelDir  (dirResultIteration </> parametersFile)) (csvParameters parameters)
  list <- ExportAsymmetricLearners.runQLearningExportingDiagnostics exportConfig runNo
  runProcess_
    (proc
        "touch"
        [fromRelDir (dirResultIteration </> rewardsExtendedEndNFile)]
       )
  runProcess_
    (shell
        ( "tail "
         ++ "-"
         ++ (show $  keepOnlyNLastIterations * ExportAsymmetricLearners.players exportConfig)
         ++ " "
         ++ fromRelDir (dirResultIteration </> rewardsExtendedFile)
         ++ " > "
         ++ fromRelDir (dirResultIteration </> rewardsExtendedEndNFile)
        ))
  runProcess_
    (shell
        ( "rm "
         ++ fromRelDir (dirResultIteration </> rewardsExtendedFile)
        ))
  -- ^ FIXME: once proper buffering implemented get rid of this part
  putStrLn ("completed task: " ++ name)
  let (x1 ::- x2 ::- Nil) = list
      (p1,env1) = x1
      (p2,env2) = x2
      q1       = _qTable env1
      q2       = _qTable env2
      lastObs1      = Obs p1
      lastObs2      = Obs p2
  liftIO $ pure ((q1,q2),(lastObs1,lastObs2))

----------------------------------
-- Single rematched Learning stage
----------------------------------
-- Same as rematchedLearning but requires an additional action to compose the new observation
rematchedLearningWithExogObs :: String
                    -- ^ Run name
                    -> ExportAsymmetricLearners.RunNumber
                    -- ^ The number of the run for the same estimation
                    -> Int
                    -- ^ Keep only last number of iterations
                    -> Map (String,String,Int) (StdGen -> StdGen -> StdGen -> StdGen -> Parameters)
                    -- ^ Map of identifier into parameters
                    -> (String
                        -> Parameters
                        -> IO
                              (QTable
                                Player1N Observation PriceSpace)
                        -> IO
                              (QTable
                                Player2N Observation PriceSpace)
                        -> (Observation PriceSpace, Observation PriceSpace)
                        -> ExportAsymmetricLearners.ExportConfig
                              Player1N
                              Observation
                              PriceSpace
                              (RIO.RIO
                                (RIO.GLogFunc
                                    (QLearningMsg
                                      Player1N Observation PriceSpace))))
                    -> (Map String (QTable Player1N Observation PriceSpace,Observation PriceSpace), Map String (QTable Player2N Observation PriceSpace, Observation PriceSpace))
                    -> (ReMatchTypeExog,PriceSpace)
                    -- ^ Rematching information and manual action for player 2, creates fixed observation
                    -> IO (Map String ((QTable
                                          Player1N Observation PriceSpace), Observation PriceSpace)
                              , Map String ((QTable
                                          Player2N Observation PriceSpace), Observation PriceSpace))
rematchedLearningWithExogObs name runNo keepOnlyNLastIterations parametersGameRematchingMap exportConfigGameRematching (qTablesMap1,qTablesMap2) (rematchType@ReMatchTypeExog{..}, manualAction2) = do
  let parametersGameRematching = parametersGameRematchingMap ! (experiment1Ex,experiment2Ex,priceSpaceEx)
      (x1,lastObs1)            = qTablesMap1 ! experiment1Ex
      (x2,lastObs2)                   = qTablesMap2 ! experiment2Ex
      manualLastObs            = Obs manualAction2
      newNameExp               = experiment1Ex ++ experiment2Ex ++ (show priceSpaceEx)
  ((q1New,q2New),lastObs) <- rematchedLearningSingleRun
                                 (name ++ "_action_" ++ show priceSpaceEx)
                                 runNo
                                 keepOnlyNLastIterations
                                 (ReMatchType experiment1Ex experiment2Ex randomMatchEx)
                                 parametersGameRematching
                                 exportConfigGameRematching
                                 x1
                                 x2
                                 (lastObs1,manualLastObs)
  pure  (fromList [(newNameExp,(q1New,lastObs1))],fromList [(newNameExp,(q2New,lastObs2))])



-- Same as rematchedLearning but with a target name to store output
rematchedLearningWithName :: String
                    -- ^ Run name
                    -> ExportAsymmetricLearners.RunNumber
                    -- ^ The number of the run for the same estimation
                    -> Int
                    -- ^ Keep only last number of iterations
                    -> Map (String,String) (StdGen -> StdGen -> StdGen -> StdGen -> Parameters)
                    -- ^ Map of identifier into parameters
                    -> (String
                        -> Parameters
                        -> IO
                              (QTable
                                Player1N Observation PriceSpace)
                        -> IO
                              (QTable
                                Player2N Observation PriceSpace)
                        -> (Observation PriceSpace, Observation PriceSpace)
                        -> ExportAsymmetricLearners.ExportConfig
                              Player1N
                              Observation
                              PriceSpace
                              (RIO.RIO
                                (RIO.GLogFunc
                                    (QLearningMsg
                                      Player1N Observation PriceSpace))))
                    -> (Map String (QTable Player1N Observation PriceSpace,Observation PriceSpace), Map String (QTable Player2N Observation PriceSpace, Observation PriceSpace))
                    -> String
                    -- ^ Target name for storing output
                    -> ReMatchType
                    -- ^ Rematching information
                    -> IO (Map String ((QTable
                                          Player1N Observation PriceSpace), Observation PriceSpace)
                              , Map String ((QTable
                                          Player2N Observation PriceSpace), Observation PriceSpace))
rematchedLearningWithName name runNo keepOnlyNLastIterations parametersGameRematchingMap exportConfigGameRematching (qTablesMap1,qTablesMap2) targetName rematchType@ReMatchType{..}  = do
  let parametersGameRematching = parametersGameRematchingMap ! (experiment1,experiment2)
      (x1,lastObs1)            = qTablesMap1 ! experiment1
      (x2,lastObs2)            = qTablesMap2 ! experiment2
      newNameExp               = experiment1 ++ experiment2
  ((q1New,q2New),lastObs) <- rematchedLearningSingleRun
                                 name
                                 runNo
                                 keepOnlyNLastIterations
                                 rematchType
                                 parametersGameRematching
                                 exportConfigGameRematching
                                 x1
                                 x2
                                 (lastObs1,lastObs2)
  pure $ (fromList [(targetName,(q1New,lastObs1))],fromList [(targetName,(q2New,lastObs2))])
  -- ^ Format: e1e2 ; e1e2


-- Takes the information given for rematching pairing, qvalues from previous runs, and rematches them for a specific identifier
-- FIXME treat the last observation differently; check maybe whether they are identical?
rematchedLearning :: String
                    -- ^ Run name
                    -> ExportAsymmetricLearners.RunNumber
                    -- ^ The number of the run for the same estimation
                    -> Int
                    -- ^ Keep only last number of iterations
                    -> Map (String,String) (StdGen -> StdGen -> StdGen -> StdGen -> Parameters)
                    -- ^ Map of identifier into parameters
                    -> (String
                        -> Parameters
                        -> IO
                              (QTable
                                Player1N Observation PriceSpace)
                        -> IO
                              (QTable
                                Player2N Observation PriceSpace)
                        -> (Observation PriceSpace, Observation PriceSpace)
                        -> ExportAsymmetricLearners.ExportConfig
                              Player1N
                              Observation
                              PriceSpace
                              (RIO.RIO
                                (RIO.GLogFunc
                                    (QLearningMsg
                                      Player1N Observation PriceSpace))))
                    -> (Map String (QTable Player1N Observation PriceSpace,Observation PriceSpace), Map String (QTable Player2N Observation PriceSpace, Observation PriceSpace))
                    -> ReMatchType
                    -- ^ Rematching information
                    -> IO (Map String ((QTable
                                          Player1N Observation PriceSpace), Observation PriceSpace)
                              , Map String ((QTable
                                          Player2N Observation PriceSpace), Observation PriceSpace))
rematchedLearning name runNo keepOnlyNLastIterations parametersGameRematchingMap exportConfigGameRematching (qTablesMap1,qTablesMap2) rematchType@ReMatchType{..} = do
  let parametersGameRematching = parametersGameRematchingMap ! (experiment1,experiment2)
      (x1,lastObs1)            = qTablesMap1 ! experiment1
      (x2,lastObs2)            = qTablesMap2 ! experiment2
      newNameExp               = experiment1 ++ experiment2
  ((q1New,q2New),lastObs) <- rematchedLearningSingleRun
                                 name
                                 runNo
                                 keepOnlyNLastIterations
                                 rematchType
                                 parametersGameRematching
                                 exportConfigGameRematching
                                 x1
                                 x2
                                 (lastObs1, lastObs2)
  pure $ (fromList [(newNameExp,(q1New,lastObs1))],fromList [(newNameExp,(q2New,lastObs2))])



-- Takes the information from two learning phase runs and computes the exploration output
-- NOTE this creates a proper copy of each qmatrix. In principle not needed, there could be reuse but probably safer
rematchedLearningSingleRun :: String
                           -- ^ Run name
                           -> ExportAsymmetricLearners.RunNumber
                           -- ^ The number of the run for the same estimation
                           -> Int
                           -- ^ Keep only last number of iterations
                           -> ReMatchType
                           -- ^ Rematching information
                           -> (StdGen -> StdGen -> StdGen -> StdGen -> Parameters)
                           -- ^ Parameters specific for the rematching player1 from experiment i and player 2 from experiment j
                           -> (String
                               -> Parameters
                               -> IO
                                     (QTable
                                       Player1N Observation PriceSpace)
                               -> IO
                                     (QTable
                                       Player2N Observation PriceSpace)
                               -> (Observation PriceSpace, Observation PriceSpace)
                               -> ExportAsymmetricLearners.ExportConfig
                                     Player1N
                                     Observation
                                     PriceSpace
                                     (RIO.RIO
                                       (RIO.GLogFunc
                                           (QLearningMsg
                                             Player1N Observation PriceSpace))))
                           -> QTable Player1N Observation PriceSpace
                           -> QTable Player2N Observation PriceSpace
                           -> (Observation PriceSpace, Observation PriceSpace)
                           -> IO ((QTable
                                      Player1N Observation PriceSpace,
                                   QTable
                                      Player2N Observation PriceSpace), (Observation PriceSpace,Observation PriceSpace))

rematchedLearningSingleRun name runNo keepOnlyNLastIterations ReMatchType{..} parametersGameRematching exportConfigGameRematching x1 x2 lastObs = do
  x1'      <- copyArrayP1 x1
  x2'      <- copyArrayP2 x2
  if randomMatch
    then
         pairing2
             ("p1" ++ experiment1 ++ "p2" ++ experiment2 ++ name)
             runNo
             keepOnlyNLastIterations
             parametersGameRematching
             exportConfigGameRematching
             (x1',x2') randomInitialObservation
    else
         pairing2
             ("p1" ++ experiment1 ++ "p2" ++ experiment2 ++ name)
             runNo
             keepOnlyNLastIterations
             parametersGameRematching
             exportConfigGameRematching
             (x1',x2') (\_ -> lastObs)


-- Copy original array for player 1 to a new location so that we do not affect the original array when computing on the copy
copyArrayP1 :: QTable Player1N Observation PriceSpace -> IO (QTable Player1N Observation PriceSpace)
copyArrayP1 x = do
   x' <- MA.freeze x :: IO (QTableIm Player1N Observation PriceSpace)
   MA.thaw x'


-- Copy original array for player 2 to a new location so that we do not affect the original array when computing on the copy
copyArrayP2 :: QTable Player2N Observation PriceSpace -> IO (QTable Player2N Observation PriceSpace)
copyArrayP2 x = do
   x' <- MA.freeze x :: IO (QTableIm Player2N Observation PriceSpace)
   MA.thaw x'

-- Reinitiates the exploitation phase for the starting conditions of two players
pairing2 :: String
         -> ExportAsymmetricLearners.RunNumber
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
            -> (Observation PriceSpace, Observation PriceSpace)
            -> ExportAsymmetricLearners.ExportConfig
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
         -> (Parameters -> (Observation PriceSpace, Observation PriceSpace))
         -- ^ Function that creates new observation
         -- Two uses: Randomly created or constant from outside
         ->  IO ((QTable
                         Player1N Observation PriceSpace,
                      QTable
                         Player2N Observation PriceSpace), (Observation PriceSpace, Observation PriceSpace))
pairing2 name runNo keepOnlyNLastIterations parametersGameRematchingFunction exportConfigGameRematchingFunction (qt1,qt2) newObsF = do
  gEnv1   <- newStdGen
  gEnv2   <- newStdGen
  gObs1   <- newStdGen
  gObs2   <- newStdGen
  let newParameters = parametersGameRematchingFunction gEnv1 gEnv2 gObs1 gObs2
      newExportConfig = exportConfigGameRematchingFunction name newParameters (pure qt1) (pure qt2) (newObsF newParameters)
  dirResultIteration <- parseRelDir name
  IO.createDirIfMissing True dirResultIteration
  L.writeFile (fromRelDir  (dirResultIteration </> seedsFile)) (csvSeeds newParameters)
  L.writeFile (fromRelDir  (dirResultIteration </> parametersFile)) (csvParameters newParameters)
  list <- ExportAsymmetricLearners.runQLearningExportingDiagnostics newExportConfig runNo
  runProcess_
    (proc
        "touch"
        [fromRelDir (dirResultIteration </> rewardsExtendedEndNFile)]
       )
  runProcess_
    (shell
        ( "tail "
         ++ "-"
         ++ (show $  keepOnlyNLastIterations * ExportAsymmetricLearners.players newExportConfig)
         ++ " "
         ++ fromRelDir (dirResultIteration </> rewardsExtendedFile)
         ++ " > "
         ++ fromRelDir (dirResultIteration </> rewardsExtendedEndNFile)
        ))
  runProcess_
    (shell
        ( "rm "
         ++ fromRelDir (dirResultIteration </> rewardsExtendedFile)
        ))
  putStrLn ("completed task: " ++ name)
  let (x1 ::- x2 ::- Nil) = list
      (p1,env1) = x1
      (p2,env2) = x2
      q1       = _qTable env1
      q2       = _qTable env2
      lastObs1      = Obs p1
      lastObs2      = Obs p2
  liftIO $ pure ((q1,q2),(lastObs1,lastObs2))
  -- ^ FIXME the last observation construction should be made more robust




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
  } deriving (Generic,Show)

instance ToNamedRecord ExportParameters
instance DefaultOrdered ExportParameters

data ExportSeeds = ExportSeeds
  { expGeneratorEnv1 :: !String
  , expGeneratorEnv2 :: !String
  , expGeneratorObs1 :: !String
  , expGeneratorObs2 :: !String
  , expInitialObservation1 :: !PriceSpace
  , expInitialObservation2 :: !PriceSpace
  , expInitialPrice1 :: !PriceSpace
  , expInitialPrice2 :: !PriceSpace
  } deriving (Generic,Show)

instance ToNamedRecord ExportSeeds
instance DefaultOrdered ExportSeeds


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

-- | Instantiate the export of seed information
-- NOTE this tracks information which changes from run to run
exportSeeds :: Parameters -> ExportSeeds
exportSeeds par = ExportSeeds
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

csvSeeds :: Parameters -> L.ByteString
csvSeeds par = encodeDefaultOrderedByName  [exportSeeds par]

parametersGame1 :: Parameters
parametersGame1  = Parameters
  { pKsi = 0.1
  , pBeta =  0.000004
  , pInitialExploreRate1 = ((exp 1) ** 0)
  , pInitialExploreRate2 = ((exp 1) ** 0)
  , pBertrandPrice1 = 1.47
  , pBertrandPrice2 = 1.47
  , pMonopolyPrice1 = 2.62
  , pMonopolyPrice2 = 2.62
  , pGamma = 0.95
  , pLearningRate = 0.15
  , pMu = 0.25
  , pA1 = 2
  , pA2 = 2
  , pA0 = 0
  , pC1 = 1
  , pC2 = 1
  , pM1 = 19 -- NOTE: Due to the construction, we need to take the orginial value of Calvano and take -1
  , pM2 = 19 -- NOTE: Due to the construction, we need to take the orginial value of Calvano and take -1
  , pGeneratorEnv1 = mkStdGen 1
  , pGeneratorEnv2 = mkStdGen 1
  , pGeneratorObs1 = mkStdGen 1
  , pGeneratorObs2 = mkStdGen 1
  }