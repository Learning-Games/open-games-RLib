{-# LANGUAGE TypeOperators, ScopedTypeVariables, TupleSections, DataKinds, GADTs, FlexibleInstances, FlexibleContexts, TemplateHaskell, MultiParamTypeClasses, UndecidableInstances, TypeApplications#-}


module Engine.IterativeSolutionConcepts where

import Engine.OpenGames hiding (lift)
import Engine.OpticClass
import Engine.TLL

import           Control.Comonad
import           Control.Monad.State.Class
import qualified Control.Monad.Trans.State as ST
import qualified Data.Vector.Unboxed as V
import           Data.List (maximumBy)
import           Data.Ord (comparing)
import qualified System.Random as Rand
import qualified GHC.Arr as A



import           Optics.TH  (makeLenses)

-- TODO check the random update
-- TODO take care of the range of values being used by array on one side and by the random module on the other side
-- TODO Replace the array access functionality with a lens
-- NOTE The _x_ input needs to be manipulated in the outside game definition

------------------
-- 0 Preliminaries

type Agent = String

type Observation a = (a,a)

type QLearningStageGame m a b x s y r = OpenGame (MonadOptic m) (MonadContext m) a b x s y r


---------------
-- 1 Best reply
-- | choose best reply
-- TODO Test this properly
bestReplyStratStage :: (Num b, Ord b, Monad m) =>  Agent -> (a -> a -> b) -> [a] -> QLearningStageGame m '[m a] '[m a] (Observation a) () a  (Observation a)
bestReplyStratStage name u actionSpace = OpenGame {
  play =  \(_ ::- Nil) -> let v obs = pure $ ((),bestReply u actionSpace obs)
                              in MonadOptic v (\_ -> (\_ -> pure ())),
  evaluate = \(_ ::- Nil) (MonadContext h k) ->
              let
                output = do
                   (_,obs) <- h
                   -- ^ Take the (old observation) from the context
                   pure $ bestReply u actionSpace obs
                in (output ::- Nil)}

-- | Choose the best reply on the basis 
bestReply :: (Num b, Ord b) => (a -> a -> b) -> [a] -> Observation a -> a
bestReply u actionSpace (_, a2) = fst $ maximumBy (comparing snd) [(y, u y a2) | y <- actionSpace]

--------------------------------
-- 2 Delete a dominated strategy

-- | Takes a list of actions in as a strategy and outputs a possibly reduced set of actions out
-- Observation is a pair of actions spaces so that dominated strategies can be computed
-- TODO adapt the auxiliary part
deleteDominatedStratStage :: (Num b, Ord b, Monad m) =>  Agent -> (a1 -> a2 -> b) -> QLearningStageGame m '[m [a1]] '[m [a1]] ([a1],[a2]) () () ()
deleteDominatedStratStage name u = OpenGame {
  play =  \(as ::- Nil) -> let v obs = pure ((),())
                              in MonadOptic v (\_ -> (\_ -> pure ())),
  evaluate = \(as ::- Nil) (MonadContext h k) ->
              let
                output = do
                   (_,obs) <- h
                   -- ^ Take the (old observation) from the context
                   ownStrat <- as
                   let otherStrat = snd obs
                   pure $ deleteDomStrat u ownStrat otherStrat
                in (output ::- Nil)}


-- | Auxiliary functions for deleting of dominant strategies
deleteDomStrat :: (Num b, Ord b) => (a1 -> a2 -> b) -> [a1] -> [a2] -> [a1]
deleteDomStrat u actionSpace1 actionSpace2 =
  let matrix  = createMatrix u actionSpace1 actionSpace2
      lsBools = maxListMap matrix matrix
      in deleteActions lsBools actionSpace1


createMatrix :: (Num b, Ord b) => (a1 -> a2 -> b) -> [a1] -> [a2] -> [[b]]
createMatrix u actions1 actions2 =
  let us act1 = fmap (u act1) actions2
      in fmap us actions1


-- | dominates the first list the second?
-- | We assume lists are of the same length here
compareLists :: (Num b, Ord b) => [b] -> [b] -> [Ordering]
compareLists [] [] = []
compareLists (x:xs) (y:ys) = compare x y : compareLists xs ys 

-- | Implements when a payoff list dominates
dominanceCriterion :: [Ordering] -> Bool
dominanceCriterion ls = all (\x ->x == GT) ls

-- | Check whether a given list is dominated by another element in that list
maxList :: (Num b, Ord b) => [[b]] -> [b] -> Bool
maxList []     x = False
maxList (y:ys) x =
   if dominanceCriterion (compareLists y x) then True
                                            else maxList ys x

-- | Evaluate dominance for the whole matrix
maxListMap :: (Num b, Ord b) => [[b]] -> [[b]] -> [Bool]
maxListMap ys xs = fmap (maxList ys) xs

-- | Delete the those actions which are dominated
deleteActions :: [Bool] -> [a1] -> [a1]
deleteActions [] _          = []
deleteActions (x:xs) (y:ys) =
  if x == True then deleteActions xs ys
               else [y] ++ deleteActions xs ys 


--------------------------
-- 4 Further functionality

fromLens :: Monad m => (x -> y) -> (x -> r -> s) -> QLearningStageGame m '[] '[] x s y r
fromLens v u = OpenGame {
  play = \Nil -> MonadOptic (\x -> pure (x, v x)) (\x -> (\r -> pure $ u x r)),
  evaluate = \Nil _ -> Nil}


fromFunctions :: Monad m => (x -> y) -> (r -> s) -> QLearningStageGame m '[] '[] x s y r
fromFunctions f g = fromLens f (const g)





