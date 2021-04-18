{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}

{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeOperators, ScopedTypeVariables, TupleSections, DataKinds, GADTs, FlexibleContexts, TemplateHaskell, QuasiQuotes, GeneralizedNewtypeDeriving, TypeSynonymInstances #-}


module Examples.QLearning.IteratedDeletion
 --                    ( evalStageLS
   --                  , initiateStrat
     --                , actionSpace
       --              )
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

import Engine.QLearning
import Engine.OpenGames
import Engine.TLL
import Engine.OpticClass
import Preprocessor.AbstractSyntax
import Preprocessor.Compile
import Preprocessor.THSyntax

----------
-- 0 Types

actionSpace1 = ["A","B","C"]

actionSpace2 = [1,2,3]

payoffMatrix1 "A" 1 =  10
payoffMatrix1 "B" 1 =  0
payoffMatrix1 "C" 1 =  2
payoffMatrix1 "A" 2 =  5
payoffMatrix1 "B" 2 =  4
payoffMatrix1 "C" 2 =  3
payoffMatrix1 "A" 3 =  3
payoffMatrix1 "B" 3 =  6
payoffMatrix1 "C" 3 =  2

payoffMatrix2 1 "A" = 4
payoffMatrix2 2 "A" = 3
payoffMatrix2 3 "A" = 2
payoffMatrix2 1 "B" = 1
payoffMatrix2 2 "B" = 6
payoffMatrix2 3 "B" = 0
payoffMatrix2 1 "C" = 1
payoffMatrix2 2 "C" = 5
payoffMatrix2 3 "C" = 8


------------------------------
-- Updating state

toObs :: Monad m => m [a1] -> m [a2] -> m ((), (([a1],[a2]),([a2],[a1])))
toObs a1 a2 = do
             act1 <- a1
             act2 <- a2
             let obs1 = (act1,act2)
                 obs2 = (act2,act1)
                 in return ((),(obs1,obs2))

toObsFromLS :: Monad m => List '[m [a1], m [a2]]  -> m ((),(([a1],[a2]),([a2],[a1])))
toObsFromLS (x ::- (y ::- Nil))= toObs x y


-- From the outputted list of strategies, derive the context
fromEvalToContext :: Monad m =>  List '[m [a1], m [a2]] ->
                     MonadContext m (([a1],[a2]),([a2],[a1])) () () ()
fromEvalToContext ls = MonadContext (toObsFromLS ls) (\_ -> (\_ -> pure ()))

-- specialize to identity here 
fromEvalToContextId :: List '[Identity [a1], Identity [a2]] ->
                     MonadContext Identity (([a1],[a2]),([a2],[a1])) () () ()
fromEvalToContextId = fromEvalToContext

-- initial Strategy = just the available actionspace
initialStrat :: List '[Identity [[Char]], Identity [Integer]]
initialStrat = pure actionSpace1 ::- pure actionSpace2 ::- Nil

------------------------------
-- Game stage 
generateGame "stageSimple" ["helper"]
                (Block ["stratPair1", "stratPair2"] []
                [ Line [[|stratPair1|]] [] [|deleteDominatedStratStage "Player1" payoffMatrix1 |] []  []
                , Line [[|stratPair2|]] [] [|deleteDominatedStratStage "Player2" payoffMatrix2 |] []  []]
                [] [])



----------------------------------
-- Defining the iterator structure
evalStage  = evaluate (stageSimple "helper") 


-- Explicit list constructor much better
evalStageLS startValue n =
          let context  = fromEvalToContextId startValue
              newStrat = evalStage startValue context
              in if n > 0 then newStrat : evalStageLS newStrat (n-1)
                          else [newStrat]




testMatrix = createMatrix payoffMatrix2 [1,2] ["A"]

lsBools = maxListMap testMatrix testMatrix

testMaxL1 = maxList testMatrix (head testMatrix)

testMaxL2 = maxList testMatrix (head $ tail testMatrix)
