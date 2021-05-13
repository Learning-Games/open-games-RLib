{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
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



module Examples.QLearning.PD
                     ( initiateStrat
                     )
                     where

import           Control.Comonad
import           Control.Monad.IO.Class
import qualified Control.Monad.Trans.State as ST
import qualified Data.Array.IO as A
import           Data.Coerce
import           Data.Function
import           Data.Functor.Identity
import           Data.Vector (Vector)
import qualified Data.Vector as V
import qualified Data.Vector.Sized as SV
import           GHC.Generics
import           Language.Haskell.TH
import           System.Random
import qualified System.Random as Rand

import           Engine.QLearning
import           Engine.OpenGames
import           Engine.TLL
import           Engine.OpticClass
import           Preprocessor.AbstractSyntax
import           Preprocessor.Compile
import           Preprocessor.THSyntax

-----------
-- Types

type N = 1

newtype Observation a = Obs
  { unObs :: (a, a)
  } deriving (Show,Generic,A.Ix,Ord, Eq, Functor)

newtype Action = Action Bool  deriving (Show, Eq, Ord)
instance ToIdx Action where
  toIdx =
    \case
      Action False -> Idx 0
      Action True -> Idx 1

actionSpace :: CTable Action
actionSpace = uniformCTable (fmap coerce (V.fromList [False,True]))

-------------
-- Parameters

gamma = 0.7

learningRate = 0.40



pdMatrix :: Action -> Action -> Double
pdMatrix = on pdMatrix' coerce

pdMatrix' :: Bool -> Bool -> Double
pdMatrix' True True = 0.3
pdMatrix' True False = 0
pdMatrix' False True = 0.5
pdMatrix' False False = 0.1



-----------------------------
-- Constructing initial state
-- TODO change this
lstIndexValues = [(((Action x, Action y),Action z),v) | (((x,y),z),v) <-[
  (((True,True), True),0),(((True,False),True),0),(((False,True),True), 0),(((False,False),True),0),
   (((True,True), False),0),(((True,False),False),0),(((False,True),False), 0),(((False,False),False),0)]]



lstIndexValues2 = [(((Action x, Action y),Action z),v) | (((x,y),z),v) <-[
  (((True,True), True),4),(((True,False),True),0),(((False,True),True), 2),(((False,False),True),1),
   (((True,True), False),5),(((True,False),False),2),(((False,True),False), 4),(((False,False),False),1)]]

-- TODO check whether this makes sense
-- TODO need a constructor for this more generally
initialArray :: MonadIO m => m (QTable N Observation Action)
initialArray = liftIO (do
   arr <- A.newArray_ (asIdx l, asIdx u)
   traverse (\(k, v) -> A.writeArray arr (asIdx k) v) lstIndexValues
   pure arr)
  where
    l = minimum $ fmap fst lstIndexValues
    u = maximum $ fmap fst lstIndexValues
    asIdx ((x, y), z) = (SV.replicate (Obs (toIdx x, toIdx y)), toIdx z)


initialEnv1 :: IO (Env N Observation Action)
initialEnv1 = initialArray >>= \arr -> pure $ Env "Player1" arr  0  0.2  (Rand.mkStdGen 3) (fmap (fmap toIdx)(SV.replicate initialObservation)) (5 * 0.999)
initialEnv2 :: IO (Env N Observation Action)
initialEnv2 = initialArray >>= \arr ->  pure $ Env "Player2" arr 0  0.2  (Rand.mkStdGen 100) (fmap (fmap toIdx)(SV.replicate initialObservation)) (5 * 0.999)
-- ^ Value is taking from the benchmark paper Sandholm and Crites

---------------------------------------
-- Preparing the context for evaluation
-- Initial observation
-- TODO randomize it
initialObservation :: Observation Action
initialObservation = fmap coerce $ Obs (True,True)

-- | the initial observation is used twice as the context for two players requires it in that form
initialObservationContext :: (Observation Action, Observation Action)
initialObservationContext = (initialObservation,initialObservation)


initialContext :: Monad m => MonadContext m (Observation Action,Observation Action) () (Action,Action) ()
initialContext = MonadContext (pure (() ,initialObservationContext)) (\_ -> (\_ -> pure ()))

-- initialstrategy
initiateStrat :: IO (List '[Identity (Action, Env N Observation Action), Identity (Action, Env N Observation Action)])
initiateStrat = do e1 <- initialEnv1
                   e2 <- initialEnv2
                   pure $ pure (coerce True,e1) ::- pure (coerce True,e2) ::- Nil


------------------------------
-- Updating state

toObs :: Monad m => m (a,Env N Observation a) -> m (a, Env N Observation a) -> m ((), (Observation a, Observation a))
toObs a1 a2 = do
             (act1,env1) <- a1
             (act2,env2) <- a2
             let obs1 = Obs (act1,act2)
                 obs2 = Obs (act2,act1)
                 in return ((),(obs1,obs2))

toObsFromLS :: Monad m => List '[m (a,Env N Observation a), m (a,Env N Observation a)] -> m ((),(Observation a,Observation a))
toObsFromLS (x ::- (y ::- Nil))= toObs x y


-- From the outputted list of strategies, derive the context
fromEvalToContext :: Monad m =>  List '[m (a,Env N Observation a), m (a,Env N Observation a)] ->
                     MonadContext m (Observation a, Observation a) () (a,a) ()
fromEvalToContext ls = MonadContext (toObsFromLS ls) (\_ -> (\_ -> pure ()))



------------------------------
-- Game stage
-- TODO should be able to feed in learning rules
generateGame "stageSimple" ["helper"]
                (Block ["state1", "state2"] []
                [ Line [[|state1|]] [] [|pureDecisionQStage actionSpace  "Player1" chooseExploreAction (updateQTableST learningRate gamma)|] ["act1"]  [[|(pdMatrix act1 act2, Obs (act1,act2))|]]
                , Line [[|state2|]] [] [|pureDecisionQStage actionSpace  "Player2" chooseExploreAction (updateQTableST learningRate gamma)|] ["act2"]  [[|(pdMatrix act2 act1, Obs (act1,act2))|]]]
                [[|(act1, act2)|]] [])


----------------------------------
-- Defining the iterator structure
evalStage :: List '[IO (Action, Env N Observation Action), IO (Action, Env N Observation Action)]
            -> MonadContext
                  IO
                  (Observation Action, Observation Action)
                  ()
                  (Action, Action)
                  ()
            -> List '[IO (Action, Env N Observation Action), IO (Action, Env N Observation Action)]
evalStage  = evaluate (stageSimple "helper")

evalStageLS startValue n =
          let context  = fromEvalToContext startValue
              newStrat = evalStage startValue context
              in if n > 0 then newStrat : evalStageLS newStrat (n-1)
                          else [newStrat]
