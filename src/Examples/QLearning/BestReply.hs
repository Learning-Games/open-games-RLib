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



module Examples.QLearning.BestReply
                     ( evalStageLS
                     , initiateStrat
                     , actionSpace
                     )
                     where

import Data.Functor.Identity
import Language.Haskell.TH
import qualified Control.Monad.Trans.State as ST
import qualified Data.List as L
import qualified Data.Ix as I
import qualified GHC.Arr as A
import GHC.Generics
import qualified System.Random as Rand
import           System.Random

import Engine.IterativeSolutionConcepts
import Engine.OpenGames
import Engine.TLL
import Engine.OpticClass
import Preprocessor.AbstractSyntax
import Preprocessor.Compile
import Preprocessor.THSyntax

----------
-- 0 Types

type Quantity = Integer

-- indirect demand, price
demand :: Integer -> Integer -> Integer -> Integer
demand a q1 q2 = a - q1 - q2

-- Profit function 
profit :: Integer -> Integer -> Integer -> Integer -> Integer
profit a c q1 q2  = ((demand a q1 q2) - c)*q1

a = 30

c = 5 

instance Num (Observation Integer)

-----------------------------------------------------
-- Transforming bounds into the array and environment
-- create the action space
actionSpace :: [Quantity]
actionSpace = [1..10]

-- derive possible observations
pricePairs = [(x,y) | x <- actionSpace, y <- actionSpace] 

-- initiate a first fixed list of values at average
-- TODO change later to random values
lsValues  = [(((x,y),z),avg)| (x,y) <- xs, (z,_) <- xs]
  where  xs = pricePairs
         avg = 5

initiateStrat :: [Quantity] -> Int -> List '[Identity Quantity, Identity Quantity]
initiateStrat actionSpace i = pure q1 ::- pure q2 ::- Nil
   where gen            = mkStdGen i
         (index1,gen')  = randomR (0, (length actionSpace) - 1) gen
         q1             = actionSpace !! index1
         (index2,_g)    = randomR (0, (length actionSpace) - 1) gen'
         q2             = actionSpace !! index1


------------------------------
-- Updating state

toObs :: Monad m => m a -> m a -> m ((), (Observation a, Observation a))
toObs a1 a2 = do
             act1 <- a1
             act2 <- a2
             let obs1 = (act1,act2)
                 obs2 = (act2,act1)
                 in return ((),(obs1,obs2))

toObsFromLS :: Monad m => List '[m a, m a] -> m ((),(Observation a,Observation a))
toObsFromLS (x ::- (y ::- Nil))= toObs x y


-- From the outputted list of strategies, derive the context
fromEvalToContext :: Monad m =>  List '[m a, m a] ->
                     MonadContext m (Observation a, Observation a) () (a,a) ()
fromEvalToContext ls = MonadContext (toObsFromLS ls) (\_ -> (\_ -> pure ()))



------------------------------
-- Game stage 
generateGame "stageSimple" ["helper"]
                (Block ["state1", "state2"] []
                [ Line [[|state1|]] [] [|bestReplyStratStage "Player1" (profit a c) actionSpace |] ["q1"]  [[|0|]]
                , Line [[|state2|]] [] [|bestReplyStratStage "Player1" (profit a c) actionSpace |] ["q2"]  [[|0|]]]
                [[|(q1, q2)|]] [])



----------------------------------
-- Defining the iterator structure
evalStage :: List '[Identity Quantity, Identity Quantity]
             -> MonadContext
                  Identity
                  (Observation Quantity, Observation Quantity)
                  ()
                  (Quantity, Quantity)
                  ()
            -> List '[Identity Quantity, Identity Quantity]
evalStage  = evaluate (stageSimple "helper") 


-- Explicit list constructor much better
evalStageLS startValue n =
          let context  = fromEvalToContext startValue
              newStrat = evalStage startValue context
              in if n > 0 then newStrat : evalStageLS newStrat (n-1)
                          else [newStrat]




