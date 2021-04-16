{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeOperators, ScopedTypeVariables, TupleSections, DataKinds, GADTs, FlexibleContexts, TemplateHaskell, QuasiQuotes, GeneralizedNewtypeDeriving #-}


module Examples.QLearning.CalvanoReplication
                     ( evalStageLS
                     , initiateStrat
                     , beta
                     , actionSpace
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
initialArray :: A.Array (Observation PriceSpace, PriceSpace) Double
initialArray =  A.array (l,u) lsValues
    where l = minimum $ fmap fst lsValues
          u = maximum $ fmap fst lsValues

-- initiate the environment
initialEnv1  = Env (initialArray )  (decreaseFactor beta)  (Rand.mkStdGen 3) (5 * 0.999)
initialEnv2  = Env (initialArray )  (decreaseFactor beta)  (Rand.mkStdGen 100) (5 * 0.999)

-----------------------------
-- 2 Constructing initial state
-- First observation, randomly determined
initialObservation :: [PriceSpace] -> Int -> Observation PriceSpace
initialObservation support i = obs
  where gen            = mkStdGen i
        (index1,gen')  = randomR (0, (length support)) gen
        d1             = support !! index1
        (index2,_g)    = randomR (0, (length support)) gen'
        d2             = support !! index1
        obs            = (d1,d2)

-- initialstrategy: start with random price
initiateStrat :: [PriceSpace] -> Int -> List '[Identity (PriceSpace, Env PriceSpace), Identity (PriceSpace, Env PriceSpace)]
initiateStrat support i = pure (d1,initialEnv1 ) ::- pure (d2,initialEnv2 ) ::- Nil
   where gen            = mkStdGen i
         (index1,gen')  = randomR (0, (length support)) gen
         d1             = support !! index1
         (index2,_g)    = randomR (0, (length support)) gen'
         d2             = support !! index1


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
                (Block ["state1", "state2"] []
                [ Line [[|state1|]] [] [|pureDecisionQStage actionSpace "Player1" chooseExploreAction (chooseLearnDecrExploreQTable learningRate gamma (decreaseFactor beta'))|] ["p1"]  [[|(profit a0 a1 a2 p1 p2 mu c1, (p1,p2))|]]
                , Line [[|state2|]] [] [|pureDecisionQStage actionSpace "Player2" chooseExploreAction (chooseLearnDecrExploreQTable learningRate gamma (decreaseFactor beta'))|] ["p2"]  [[|(profit a0 a1 a2 p2 p1 mu c1, (p1,p2))|]]]
                [[|(p1, p2)|]] [])



----------------------------------
-- Defining the iterator structure
evalStage :: Double
            -> List '[Identity (PriceSpace, Env PriceSpace), Identity (PriceSpace, Env PriceSpace)]
            -> MonadContext
                  Identity
                  (Observation PriceSpace, Observation PriceSpace)
                  ()
                  (PriceSpace, PriceSpace)
                  ()
            -> List '[Identity (PriceSpace, Env PriceSpace), Identity (PriceSpace, Env PriceSpace)]
evalStage beta'  = evaluate (stageSimple beta') 


-- Explicit list constructor much better
evalStageLS beta' startValue n =
          let context  = fromEvalToContext startValue
              newStrat = evalStage beta' startValue context
              in if n > 0 then newStrat : evalStageLS beta'  newStrat (n-1)
                          else [newStrat]




