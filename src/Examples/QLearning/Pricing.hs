{-# LANGUAGE TypeOperators, ScopedTypeVariables, TupleSections, DataKinds, GADTs, FlexibleContexts, TemplateHaskell, QuasiQuotes #-}


module Examples.QLearning.Pricing
                     ( evalStageLS
                     , initiateStrat
                     )
                     where

import Control.Comonad
import Data.Functor.Identity
import Language.Haskell.TH
import qualified Control.Monad.Trans.State as ST
import qualified GHC.Arr as A
import GHC.Generics
import qualified System.Random as Rand
import           System.Random

import Engine.QLearning
import Engine.OpenGames
import Engine.TLL
import Engine.OpticClass
import Preprocessor.AbstractSyntax
import Preprocessor.Compile
import Preprocessor.THSyntax

----------
-- 0 Types

type Price = Integer

type Index = Integer

------------------------------------------
-- 1. Environment variables and parameters

-- Fixing the parameters, ksi
ksi :: Integer
ksi = 1

-- Note that the the prices are determined by calculations for the exact values
bertrandPrice = 10
monopolyPrice = 20

mu = 0.1

a1 = 1

a2 = 1

a0 = 1

c1 = 1

-- Demand follows eq 5 in Calvano et al.
demand a0 a1 a2 p1 p2 mu = (exp 1.0)**((a1-p1)/mu) / agg
  where agg = (exp 1.0)**((a1-p1)/mu) + (exp 1.0)**((a2-p2)/mu) + (exp 1.0)**(a0/mu)

profit a0 a1 a2 p1 p2 mu c1 = (p1 - c1)* (demand a0 a1 a2 p1 p2 mu)

priceBounds :: (Price,Price)
priceBounds = (bertrandPrice - ksi*(monopolyPrice - bertrandPrice), monopolyPrice*(monopolyPrice - bertrandPrice))

-- derive the individual action space for each player
actionSpace discrete = [fst priceBounds,(fst priceBounds + discrete)..snd priceBounds]

-- translate action space into index
toIndex discrete = [1..(toEnum $ length $ actionSpace discrete)]

-- translate an observed index into a price
fromIndexToPrice discrete n = fst priceBounds + n * discrete - discrete


-- derive possible observations
pricePairs discrete = [(x,y) | x <- indActSpace, y <- indActSpace] 
   where indActSpace = toIndex discrete

-- initiate a first fixed list of values at average
-- TODO change later to random values
lsValues discrete = [(((x,y),z),fromInteger avg)| (x,y) <- xs, (z,_) <- xs]
  where (l,u) = priceBounds
        avg   = l
        xs    = pricePairs discrete


-- initialArray :: Int -> QTable Price
initialArray
  :: Integer -> A.Array ((Index, Index), Index) Double
initialArray discrete =  A.array (l,u) xs
   where  xs = lsValues discrete
          l = minimum $ fmap fst xs
          u = maximum $ fmap fst xs

test = (((lo,lo),lo),((up,up),up))
  where lo = fst priceBounds
        up = snd priceBounds
-- initiate the environment 
initialEnv1 discrete = Env (initialArray discrete)  0.2  (Rand.mkStdGen 3) (5 * 0.999)
initialEnv2 discrete = Env (initialArray discrete)  0.2  (Rand.mkStdGen 100) (5 * 0.999)


-----------------------------
-- 2 Constructing initial state
-- First observation, randomly determined
initialObservation :: Integer -> Int -> (Observation Index, Observation Index)
initialObservation discrete i = (obs,obs)
  where gen       = mkStdGen i
        (d1,gen') = randomR  (1 :: Integer,toEnum $ length $ actionSpace discrete :: Integer) gen
        (d2,_g)   = randomR  (1 :: Integer,toEnum $ length $ actionSpace discrete :: Integer) gen'
        obs       = (d1,d2)

-- initialstrategy: start with lowest price
-- TODO change that later
initiateStrat :: Integer  -> List '[Identity (Index, Env Index), Identity (Index, Env Index)]
initiateStrat discrete  = pure (l,initialEnv1 discrete) ::- pure (l,initialEnv2 discrete) ::- Nil
   where (l,u) = (1 :: Integer,toEnum $ length $ actionSpace discrete :: Integer)


------------------------------
-- Updating state

toObs :: Identity (a,Env a) -> Identity (a, Env a) -> Identity (Observation a,Observation a)
toObs a1 a2 = do
             (act1,env1) <- a1
             (act2,env2) <- a2
             let obs1 = (act1,act2)
                 obs2 = (act2,act1)
                 in pure (obs1,obs2)

toObsFromLS :: List '[Identity (a,Env a), Identity (a,Env a)] -> Identity (Observation a,Observation a)
toObsFromLS (x ::- (y ::- Nil))= toObs x y


-- From the outputted list of strategies, derive the context
fromEvalToContext :: List '[Identity (a,Env a), Identity (a,Env a)] ->
                     MonadicLearnLensContext Identity (Observation a, Observation a) () (a,a) ()
fromEvalToContext ls = MonadicLearnLensContext (toObsFromLS ls) (pure (\_ -> pure ()))



------------------------------
-- Game stage 
-- TODO should be able to feed in learning rules
generateGame "stageSimple" ["discrete"]
                (Block ["state1", "state2"] []
                [ Line [[|state1|]] [] [|pureDecisionQStage priceBounds "Player1" chooseActionQTable chooseLearnQTable|] ["p1"]  [[|(profit a0 a1 a2 (fromInteger $ fromIndexToPrice discrete p1) (fromInteger $ fromIndexToPrice discrete p2) mu c1, (p1,p2)) :: (Double, Observation Integer)|]]
                , Line [[|state2|]] [] [|pureDecisionQStage priceBounds "Player2" chooseActionQTable chooseLearnQTable|] ["p2"]  [[|(profit a0 a1 a2 (fromInteger $ fromIndexToPrice discrete p2) (fromInteger $ fromIndexToPrice discrete p1) mu c1, (p1,p2)) :: (Double, Observation Integer) |]]]
                [[|(p1, p2)|]] [])


----------------------------------
-- Defining the iterator structure
evalStage discrete strat context = evaluate (stageSimple discrete) strat context


-- Explicit list constructor much better
evalStageLS discrete startValue n =
          let context  = fromEvalToContext startValue
              newStrat = evalStage discrete startValue context
              in if n > 0 then newStrat : evalStageLS discrete newStrat (n-1)
                          else [newStrat]




