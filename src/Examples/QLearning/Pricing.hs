{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeOperators, ScopedTypeVariables, TupleSections, DataKinds, GADTs, FlexibleContexts, TemplateHaskell, QuasiQuotes, GeneralizedNewtypeDeriving #-}


module Examples.QLearning.Pricing
                     ( evalStageLS
                     , initiateStrat
                     , evalStageLS'
                     , startStrat'
                     )
                     where

import Control.Comonad
import Data.Functor.Identity
import Language.Haskell.TH
import qualified Control.Monad.Trans.State as ST
import qualified Data.List as L
import qualified Data.Ix as I
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

newtype PriceSpace = PriceSpace Double
    deriving (Random,Num,Fractional,Enum,Ord,Show,Eq)


-- make PriceSpace an instance of Ix so that the information can be used as an index for Array
-- TODO this is messy; needs to change
instance  I.Ix PriceSpace where
  range (PriceSpace a, PriceSpace b) = [PriceSpace a, PriceSpace a + dist..PriceSpace b]
  index (PriceSpace a, PriceSpace b)  (PriceSpace c) =  head $  L.elemIndices (PriceSpace c) [PriceSpace a, PriceSpace a + dist..PriceSpace b] 
  inRange (PriceSpace a, PriceSpace b)  (PriceSpace c) = a <= c && c <= b  

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

-- Profit function 
profit a0 a1 a2 p1 p2 mu c1 = (p1 - c1)* (demand a0 a1 a2 p1 p2 mu)

profit' a0 a1 a2 (PriceSpace p1) (PriceSpace p2) mu c1 = (p1 - c1)* (demand a0 a1 a2 p1 p2 mu)

priceBounds :: (Price,Price)
priceBounds = (1,6)
-- derive the individual action space for each player
actionSpace  = [1..6]


-- translate action space into index
toIndex  = [1..6]


------------------------------------------------------
-- Create index on the basis of the actual prices used
-- Also allows for different price types
-- Follows the concept in Calvano

-- creates the distance in the grid
dist :: PriceSpace
dist = 1 

ksi' = 0

-- Note that the the prices are determined by calculations for the exact values
bertrandPrice' = 1
monopolyPrice' = 6


-- determine the bounds within which to search
lowerBound,upperBound :: PriceSpace
lowerBound = bertrandPrice' - ksi'*(monopolyPrice' - bertrandPrice')
upperBound = monopolyPrice' + ksi'*(monopolyPrice' - bertrandPrice')

priceBounds' = (lowerBound,upperBound)

-----------------------------------------------------
-- Transforming bounds into the array and environment
-- create the action space
actionSpace' = [lowerBound,lowerBound+dist..upperBound]

-- derive possible observations
pricePairs' = [(x,y) | x <- actionSpace', y <- actionSpace'] 

-- initiate a first fixed list of values at average
-- TODO change later to random values
lsValues'  = [(((x,y),z),avg)| (x,y) <- xs, (z,_) <- xs]
  where  xs = pricePairs'
         PriceSpace avg = (lowerBound + upperBound) / 2

-- initialArray :: Int -> QTable Price
initialArray' =  A.array (l,u) lsValues'
    where l = minimum $ fmap fst lsValues'
          u = maximum $ fmap fst lsValues'

-- initiate the environment 
initialEnv1'  = Env (initialArray' )  0.2  (Rand.mkStdGen 3) (5 * 0.999)
initialEnv2'  = Env (initialArray' )  0.2  (Rand.mkStdGen 100) (5 * 0.999)





-- derive possible observations
pricePairs  = [(x,y) | x <- indActSpace, y <- indActSpace] 
   where indActSpace = toIndex 

-- initiate a first fixed list of values at average
-- TODO change later to random values
lsValues  = [(((x,y),z),fromInteger avg)| (x,y) <- xs, (z,_) <- xs]
  where (l,u) = priceBounds
        avg   = l
        xs    = pricePairs 

-- initialArray :: Int -> QTable Price
initialArray :: A.Array ((Index, Index), Index) Double
initialArray  =  A.array (l,u) xs
   where  xs = lsValues 
          l = minimum $ fmap fst xs
          u = maximum $ fmap fst xs

-- initiate the environment 
initialEnv1  = Env (initialArray )  0.2  (Rand.mkStdGen 3) (5 * 0.999)
initialEnv2  = Env (initialArray )  0.2  (Rand.mkStdGen 100) (5 * 0.999)





-----------------------------
-- 2 Constructing initial state
-- First observation, randomly determined
-- TODO check that actual values are taken up (RandomR problem)
initialObservation' :: [PriceSpace] -> Int -> (Observation PriceSpace, Observation PriceSpace)
initialObservation' support i = (obs,obs)
  where gen            = mkStdGen i
        (index1,gen')  = randomR (0, (length support)) gen
        d1             = support !! index1
        (index2,_g)    = randomR (0, (length support)) gen'
        d2             = support !! index1
        obs            = (d1,d2)

-- initialstrategy: start with random price
initiateStrat' :: [PriceSpace] -> Int -> List '[Identity (PriceSpace, Env PriceSpace), Identity (PriceSpace, Env PriceSpace)]
initiateStrat' support i = pure (d1,initialEnv1' ) ::- pure (d2,initialEnv2' ) ::- Nil
   where gen            = mkStdGen i
         (index1,gen')  = randomR (0, (length support)) gen
         d1             = support !! index1
         (index2,_g)    = randomR (0, (length support)) gen'
         d2             = support !! index1





-- First observation, randomly determined
initialObservation :: Int -> (Observation Index, Observation Index)
initialObservation  i = (obs,obs)
  where gen       = mkStdGen i
        (d1,gen') = randomR  (1 :: Integer,toEnum $ length $ actionSpace  :: Integer) gen
        (d2,_g)   = randomR  (1 :: Integer,toEnum $ length $ actionSpace  :: Integer) gen'
        obs       = (d1,d2)



-- initialstrategy: start with lowest price
-- TODO change that later
initiateStrat :: List '[Identity (Index, Env Index), Identity (Index, Env Index)]
initiateStrat   = pure (l,initialEnv1 ) ::- pure (l,initialEnv2 ) ::- Nil
   where (l,u) = (1 :: Integer,toEnum $ length $ actionSpace  :: Integer)


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
generateGame "stageSimple" ["helper"]
                (Block ["state1", "state2"] []
                [ Line [[|state1|]] [] [|pureDecisionQStage actionSpace priceBounds "Player1" chooseActionQTable chooseLearnQTable|] ["p1"]  [[|(profit a0 a1 a2 (fromInteger p1) (fromInteger p2) mu c1, (p1,p2))|]]
                , Line [[|state2|]] [] [|pureDecisionQStage actionSpace priceBounds "Player2" chooseActionQTable chooseLearnQTable|] ["p2"]  [[|(profit a0 a1 a2 (fromInteger p2) (fromInteger p1) mu c1, (p1,p2))|]]]
                [[|(p1, p2)|]] [])

generateGame "stageSimple2" ["helper"]
                (Block ["state1", "state2"] []
                [ Line [[|state1|]] [] [|pureDecisionQStage actionSpace' priceBounds' "Player1" chooseActionQTable chooseLearnQTable|] ["p1"]  [[|(profit' a0 a1 a2 p1 p2 mu c1, (p1,p2))|]]
                , Line [[|state2|]] [] [|pureDecisionQStage actionSpace' priceBounds' "Player2" chooseActionQTable chooseLearnQTable|] ["p2"]  [[|(profit' a0 a1 a2 p2 p1 mu c1, (p1,p2))|]]]
                [[|(p1, p2)|]] [])



----------------------------------
-- Defining the iterator structure
evalStage  strat context = evaluate (stageSimple "helper") strat context




-- Explicit list constructor much better
evalStageLS  startValue n =
          let context  = fromEvalToContext startValue
              newStrat = evalStage  startValue context
              in if n > 0 then newStrat : evalStageLS  newStrat (n-1)
                          else [newStrat]


evalStage' strat context = evaluate (stageSimple2 "helper") strat context

startStrat' = initiateStrat' actionSpace'

-- Explicit list constructor much better
evalStageLS'  startValue n =
          let context  = fromEvalToContext startValue
              newStrat = evalStage'  startValue context
              in if n > 0 then newStrat : evalStageLS'  newStrat (n-1)
                          else [newStrat]




