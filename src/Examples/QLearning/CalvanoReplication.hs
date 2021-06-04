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

module Examples.QLearning.CalvanoReplication
  ( evalStageLS
  , initialStrat
  , beta
  , actionSpace
  , csvParameters
  , exportQValuesSqlite
  , sequenceL
  , evalStageM
  , PriceSpace(..)
  ) where
import           RIO (RIO, GLogFunc)
import qualified Data.HashMap.Strict as HM
import           Data.HashMap.Strict (HashMap)
import           Control.Monad.Trans.State
import qualified Data.Sequence as Seq
import           Data.Sequence (Seq(..))
import qualified Data.Ix as Ix
import qualified Data.Text.Encoding as T
import           Data.Text (Text)
import qualified Data.ByteString.Lazy as L
import           Control.Monad.Trans.Reader
import           Data.Pool
import           Database.Persist.TH
import           Database.Persist
import           Control.Monad.IO.Class
import           Control.Monad.Logger
import           Database.Persist.Sqlite
import           Data.Foldable
import qualified Engine.Memory as Memory
import           Control.DeepSeq
import qualified Data.Vector.Sized as SV
import qualified Data.Vector as V
import           Data.ByteString (ByteString)
import           Data.Array.IO as A
import qualified Data.Aeson as Aeson
import           Data.Csv

import qualified Data.Ix as I
import           GHC.Generics
import qualified System.Random as Rand
import           System.Random

import           Engine.QLearning
import           Engine.OpenGames
import           Engine.TLL
import           Engine.OpticClass
import           Preprocessor.Compile

----------
-- 0 Types

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
parameters :: ExportParameters
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
  (samplePopulation_ actionSpace (mkStdGen generatorObs1))
  (samplePopulation_ actionSpace (mkStdGen generatorObs2))
  (samplePopulation_ actionSpace (mkStdGen generatorPrice1))
  (samplePopulation_ actionSpace (mkStdGen generatorPrice2))

-- | export to CSV
csvParameters :: L.ByteString
csvParameters = encodeDefaultOrderedByName  [parameters]

-- instance ToNamedRecord ExportQValues
-- instance DefaultOrdered ExportQValues

-------------------------
-- 2. Export Data as JSON
-- TODO need to deal with Rand.StdGen

-- 2.1. Parameters

-- 2.2. Q-values

data ExportQValues = ExportQValues
   { expQValues  :: ![((Memory.Vector Player1N (Observation (Idx PriceSpace)),Idx PriceSpace),Double)]
   , expQBounds :: !(Bounds Player1N Observation PriceSpace) -- Warning: hard-codes memory size of player1.
   , expKeys :: ![(Memory.Vector Player1N (Observation (Idx PriceSpace)), Idx PriceSpace)]
   } deriving (Generic,Show)

type Bounds n o a
   = ( (Memory.Vector n (o (Idx a)), Idx a)
     , (Memory.Vector n (o (Idx a)), Idx a))

-- | Extract relevant information into a record to be exported
fromTLLToExport :: List '[ (PriceSpace, Env Player1N Observation PriceSpace), (PriceSpace, Env Player2N Observation PriceSpace)] -> IO [ExportQValues]
fromTLLToExport (p1 ::- p2 ::- Nil) = do
  let ((_, env1)) = p1
      ((_, env2)) = p2

  expQValues1 <- A.getAssocs $ _qTable env1
  expQValues2 <- A.getAssocs $ _qTable env2
  bounds1 <- A.getBounds (_qTable env1)
  bounds2 <- A.getBounds (_qTable env2)
  keys1 <- fmap (fmap fst) (A.getAssocs (_qTable env1))
  keys2 <- fmap (fmap fst) (A.getAssocs (_qTable env1))
  let
      expPlayer1 = ExportQValues expQValues1 bounds1 keys1
      expPlayer2 = ExportQValues expQValues2 bounds2 keys2
  pure $ [expPlayer1, expPlayer2]

------------------------------------------
-- 3. Environment variables and parameters
-- Fixing the parameters

ksi :: Double
ksi = 0.1

-- Decrease temp per iteration
beta :: Double
beta =  (- 0.00001)

decreaseFactor :: Floating a => a -> a
decreaseFactor b = (log 1) ** b

-- NOTE that the the prices are determined by calculations for the exact values
bertrandPrice :: Double
bertrandPrice = 1
monopolyPrice :: Double
monopolyPrice = 6


-- fixing outside parameters
gamma :: DiscountFactor
gamma = 0.95

learningRate :: LearningRate
learningRate = 0.15

mu :: Double
mu = 0.25

a1 :: Double
a1 = 1.5

a2 :: Double
a2 = 1

a0 :: Double
a0 = 1

c1 :: Double
c1 = 0.5

-- NOTE: Due to the construction, we need to take the orginial value of Calvano and take -1
m :: Double
m = 14

-- Demand follows eq 5 in Calvano et al.
demand :: Floating a => a -> a -> a -> a -> a -> a -> a
demand a0 a1 a2 p1 p2 mu = (exp 1.0)**((a1-p1)/mu) / agg
  where agg = (exp 1.0)**((a1-p1)/mu) + (exp 1.0)**((a2-p2)/mu) + (exp 1.0)**(a0/mu)

-- Profit function
profit :: Double -> Double -> Double -> PriceSpace -> PriceSpace -> Double -> Double -> Double
profit a0 a1 a2 (PriceSpace p1 _) (PriceSpace p2 _) mu c1 = (p1 - c1)* (demand a0 a1 a2 p1 p2 mu)

-- Fixing initial generators
generatorObs1 :: Int
generatorObs1 = 2
generatorObs2 :: Int
generatorObs2 = 400
generatorEnv1 :: Int
generatorEnv1 = 3
generatorEnv2 :: Int
generatorEnv2 = 100
generatorPrice1 :: Int
generatorPrice1 = 90
generatorPrice2 :: Int
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
pricePairs :: [(PriceSpace, PriceSpace)]
pricePairs =
  [ (x, y)
  | x <- V.toList (population actionSpace)
  , y <- V.toList (population actionSpace)
  ]

-- initiate a first fixed list of values at average
-- TODO change later to random values
lsValues :: [(((PriceSpace, PriceSpace), PriceSpace), Double)]
lsValues  = [(((x,y),z),avg)| (x,y) <- xs, (z,_) <- xs]
  where  xs = pricePairs
         avg = (lowerBound + upperBound) / 2

-- initiate the environment
initialEnv1 :: M (Env Player1N Observation PriceSpace)
initialEnv1 =
  liftIO initialArray >>= \arr ->
    pure
      (Env
         "Player1"
         arr
         0
         (decreaseFactor beta)
         (Rand.mkStdGen generatorEnv1)
         (Memory.fromSV (SV.replicate (fmap toIdx initialObservation)))
         (5 * 0.999))
  where
    initialArray :: IO (QTable Player1N Observation PriceSpace)
    initialArray = do
      arr <- newArray_ (asIdx l, asIdx u)
      traverse_ (\(k, v) -> writeArray arr ( (asIdx k)) v) lsValues
      pure arr
      where
        l = minimum $ fmap fst lsValues
        u = maximum $ fmap fst lsValues
        asIdx ((x, y), z) = (Memory.fromSV (SV.replicate (Obs (toIdx x, toIdx y))), toIdx z)

initialEnv2 :: M (Env Player2N Observation PriceSpace)
initialEnv2 =
  liftIO initialArray >>= \arr ->
    pure $
    Env
      "Player2"
      (arr)
      0
      (decreaseFactor beta)
      (Rand.mkStdGen generatorEnv2)
      (Memory.fromSV (SV.replicate (fmap toIdx initialObservation)))
      (5 * 0.999)
  where
    initialArray :: IO (QTable Player2N Observation PriceSpace)
    initialArray = do
      arr <- newArray_ (asIdx l, asIdx u)
      traverse_ (\(k, v) -> writeArray arr (asIdx k) v) lsValues
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
initialStrat :: M (List '[M (PriceSpace, Env Player1N Observation PriceSpace), M (PriceSpace, Env Player2N Observation PriceSpace)])
initialStrat = do
  e1 <- initialEnv1
  e2 <- initialEnv2
  pure
    (pure (samplePopulation_ actionSpace (mkStdGen generatorObs1), e1) ::-
     pure (samplePopulation_ actionSpace (mkStdGen generatorObs2), e2) ::-
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
stageSimple :: Double -> OpenGame (MonadOptic M) (MonadContext M) ('[M (PriceSpace, Env Player1N Observation PriceSpace), M (PriceSpace, Env Player1N Observation PriceSpace)] +:+ '[]) ('[M (PriceSpace, Env Player1N Observation PriceSpace), M (PriceSpace, Env Player1N Observation PriceSpace)] +:+ '[]) (Observation PriceSpace, Observation PriceSpace) () (PriceSpace, PriceSpace) ()
stageSimple beta = [opengame|
   inputs    : (state1,state2) ;
   feedback  :      ;

   :-----------------:
   inputs    :  state1    ;
   feedback  :      ;
   operation : pureDecisionQStage actionSpace "Player1" chooseExploreAction (chooseLearnDecrExploreQTable learningRate gamma (decreaseFactor beta)) ;
   outputs   :  p1 ;
   returns   :  (profit a0 a1 a2 p1 p2 mu c1, Obs (p1,p2)) ;

   inputs    : state2     ;
   feedback  :      ;
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
     List '[ M (PriceSpace, Env Player1N Observation PriceSpace), M ( PriceSpace
                                                                      , Env Player2N Observation PriceSpace)]
  -> MonadContext M (Observation PriceSpace, Observation PriceSpace) () ( PriceSpace
                                                                         , PriceSpace) ()
  -> List '[ M (PriceSpace, Env Player1N Observation PriceSpace), M ( PriceSpace
                                                                      , Env Player2N Observation PriceSpace)]
evalStage = evaluate (stageSimple beta)


-- Explicit list constructor much better
evalStageLS :: (Ord t, Num t) => List '[M (PriceSpace, Env Player1N Observation PriceSpace), M (PriceSpace, Env Player2N Observation PriceSpace)] -> t -> [List '[M (PriceSpace, Env Player1N Observation PriceSpace), M (PriceSpace, Env Player2N Observation PriceSpace)]]
evalStageLS startValue n =
          let context  = fromEvalToContext startValue
              newStrat = evalStage startValue context
              in if n > 0 then newStrat : evalStageLS newStrat (n-1)
                          else [newStrat]

evalStageM ::
     List '[ (PriceSpace, Env Player1N Observation PriceSpace), ( PriceSpace
                                                                , Env Player2N Observation PriceSpace)]
  -> Int
  -> M [List '[ (PriceSpace, Env Player1N Observation PriceSpace), ( PriceSpace
                                                                    , Env Player2N Observation PriceSpace)]]
evalStageM _ 0 = pure []
evalStageM startValue n = do
  newStrat <-
    sequenceL
      (evalStage (hoist startValue) (fromEvalToContext (hoist startValue)))
  rest <- evalStageM newStrat (pred n)
  pure (newStrat : rest)

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


--------------------------------------------------------------------------------
-- sqlite export

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
StateActionIndex
  state Text
  action Text
QValue
  iteration Int
  player Int
  stateActionIndex StateActionIndexId
  qvalue Double
  deriving Show
|]

data Out = Out
  { qvalues :: !(Seq QValue)
  , keys :: !(HashMap (ByteString, ByteString) Int)
  }
exportQValuesSqlite :: [List '[ (PriceSpace, Env Player1N Observation PriceSpace), (PriceSpace, Env Player2N Observation PriceSpace)]]-> IO ()
exportQValuesSqlite results =
  runNoLoggingT
    (withSqlitePool
       "qvalues.sqlite3"
       1
       (flip
          withResource
          (runReaderT
             (do runMigration migrateAll
                 Out {..} <-
                   flip
                     execStateT
                     Out {qvalues = mempty, keys = mempty}
                     (mapM_
                        (\(iterationIdx, tll) -> do
                           players <- liftIO (fromTLLToExport tll)
                           mapM_
                             (\(player, exportQValues) -> do
                                let stateActionValueTriples =
                                      expQValues exportQValues
                                mapM_
                                  (\key@(state0, action) ->
                                     let idx =
                                           Ix.index
                                             (expQBounds exportQValues)
                                             key
                                         state' =
                                           fmap
                                             (fmap (readTable actionSpace))
                                             state0
                                         action' = readTable actionSpace action
                                      in modify'
                                           (\Out {..} ->
                                              Out
                                                { keys =
                                                    HM.insert
                                                      ( L.toStrict
                                                          (Aeson.encode state')
                                                      , L.toStrict
                                                          (Aeson.encode action'))
                                                      idx
                                                      keys
                                                , ..
                                                }))
                                  (expKeys exportQValues)
                                mapM_
                                  (\(stateAndAction, value) -> do
                                     let newqvalue =
                                           QValue
                                             { qValueIteration = iterationIdx
                                             , qValuePlayer = player
                                             , qValueStateActionIndex =
                                                 toSqlKey
                                                   (fromIntegral
                                                      (Ix.index
                                                         (expQBounds
                                                            exportQValues)
                                                         stateAndAction))
                                             , qValueQvalue = value
                                             }
                                     modify'
                                       (\Out {..} ->
                                          Out
                                            { qvalues = qvalues Seq.|> newqvalue
                                            , ..
                                            }))
                                  stateActionValueTriples)
                             (zip [1 ..] players))
                        (zip [1 ..] results))
                 insertEntityMany
                   (map
                      (\((state', action), idx) ->
                         Entity
                           { entityVal =
                               StateActionIndex
                                 { stateActionIndexState = T.decodeUtf8 state'
                                 , stateActionIndexAction = T.decodeUtf8 action
                                 }
                           , entityKey = toSqlKey (fromIntegral idx)
                           })
                      (HM.toList keys))
                 insertMany_ (toList qvalues)))))
