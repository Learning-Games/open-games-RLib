{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE AllowAmbiguousTypes #-}



module Engine.InteractiveIO where

import           Engine.Memory (Memory)
import qualified Engine.Memory as Memory
import           Engine.OpenGames hiding (lift)
import           Engine.OpticClass
import           Engine.TLL
import           Engine.QLearning (QTable(..), Idx(..), ToIdx (..), CTable(..))


import           Control.Monad.Reader
import qualified Data.Array.IArray as IA
import qualified Data.Array.IO as A
import           Data.Array.IO
import           Data.Foldable
import           Data.Ix
import qualified Data.Ix as Ix
import           Data.List (maximumBy,nub,sortBy)
import           Data.Ord (comparing)
import qualified Data.Vector as V
import           Numeric.Probability.Distribution hiding (map, lift, filter)
import qualified Optics.Lens as L



---------------------------------------------------------------------
-- This module implements a plain pure input version of an open game.
-- The idea is that the input comes from the outside world.
-- The open games just ties together the information/action flow.
-- Use case is the connection to Ensemble as a game structure for
-- experiments.
---------------------------------------------------------------------



-------------
-- Data types
type AgentIO = String

type InteractiveStageGame a b x s y r = OpenGame PureLens PureLensContext a b x s y r

data DiagnosticInfoInteractive y = DiagnosticInfoInteractive
  { playerIO          :: String
  , optimalMoveIO     :: y
  , optimalPayoffIO   :: Double
  , currentMoveIO     :: y
  , currentPayoffIO   :: Double}


data PrintOutput = PrintOutput

instance (Show y, Ord y) => Apply PrintOutput [DiagnosticInfoInteractive y] String where
  apply _ x = showDiagnosticInfoLInteractive x


data Concat = Concat

instance Apply Concat String (String -> String) where
  apply _ x = \y -> x ++ "\n NEWGAME: \n" ++ y


-- QTable with only actions as index
type QTableNoObs a = A.IOUArray (Idx a) Double

------------------------------------------------
-- Diagnostic information that is used as output

showDiagnosticInfoInteractive :: (Show y, Ord y) => DiagnosticInfoInteractive y -> String
showDiagnosticInfoInteractive info =
     "\n"    ++ "Player: " ++ playerIO info
     ++ "\n" ++ "Optimal Move: " ++ (show $ optimalMoveIO info)
     ++ "\n" ++ "Optimal Payoff: " ++ (show $ optimalPayoffIO info)
     ++ "\n" ++ "Current Move: " ++ (show $ currentMoveIO info)
     ++ "\n" ++ "Current Payoff: " ++ (show $ currentPayoffIO info)



-- Output string information for a subgame expressions containing information from several players - bayesian
showDiagnosticInfoLInteractive :: (Show y, Ord y)  => [DiagnosticInfoInteractive y] -> String
showDiagnosticInfoLInteractive [] = "\n --No more information--"
showDiagnosticInfoLInteractive (x:xs)  = showDiagnosticInfoInteractive x ++ "\n --other game-- " ++ showDiagnosticInfoLInteractive xs


-- Devations in a given context
deviationsInContext :: (Show a, Ord a)
                    =>  AgentIO -> a -> (a -> Double) -> [a] -> [DiagnosticInfoInteractive a]
deviationsInContext name strategy u ys =
   let
     ls              = fmap u ys
     strategicPayoff = u strategy
     zippedLs        = zip ys ls
     (optimalPlay, optimalPayoff) = maximumBy (comparing snd) zippedLs
     in [DiagnosticInfoInteractive
            {  playerIO = name
            , optimalMoveIO = optimalPlay
            , optimalPayoffIO = optimalPayoff
            , currentMoveIO   = strategy
            , currentPayoffIO = strategicPayoff
            }]

-- all information for all players
generateOutputIO :: forall xs.
               ( MapL   PrintOutput xs     (ConstMap String xs)
               , FoldrL Concat String (ConstMap String xs)
               ) => List xs -> IO ()
generateOutputIO hlist = putStrLn $
  "----Analytics begin----" ++ (foldrL Concat "" $ mapL @_ @_ @(ConstMap String xs) PrintOutput hlist) ++ "----Analytics end----\n"


--------------------------
-- Connecting to a qmatrix
--------------------------

-- Transform learned qmatrix into matrix which ignores the observation part
transformLearnedQMatrix ::
   (Ix (o (Idx a)) , Ix (Memory.Vector n (o (Idx a))))
   => QTable n o a -> CTable a -> IO (QTableNoObs a) -- replace with vector
transformLearnedQMatrix qmatrix support = do
  immutMatrixLs <- getAssocs qmatrix
  let reduceOperation = reduceList $ listIndexToNewIndex immutMatrixLs
      actionLs        = nub [a | ((o,a), _) <- immutMatrixLs]
      -- ^ create non-duplicated IDx list of actions
      reducedList     = fmap reduceOperation actionLs
  undefined
  where
    listIndexToNewIndex :: [((o,a),e)] -> [(a,e)]
    listIndexToNewIndex ls = [(a,e)|((_,a),e) <- ls]
    -- ^ transform list by getting rid of observations
    reduceList :: (Eq a, Num e) => [(a,e)] -> a -> (a,e)
    reduceList ls x = (x, sumUpList x ls)
    -- ^ for list and a single action, create action sum of value pairs
      where
          sumUpList :: (Eq a, Num e) => a -> [(a,e)] -> e
          sumUpList _ [] = 0
          sumUpList x ((y,e):ys) =
            if x == y then e + sumUpList x ys
                      else sumUpList x ys




createArray :: [(Idx a,Double)] -> A.IOUArray (Idx a) Double
createArray ls = do
-- ^ creates the new array
  let sortedLs = ls -- sortBy (\(x,_) (y,_) -> compare x y) ls
      lowerBound = minimum $ fmap fst sortedLs
      upperBound = maximum $ fmap fst sortedLs
  arr  <- newArray_ (lowerBound,upperBound)
  traverse_ (\(k,v) -> writeArray arr k v) sortedLs
  pure arr
{--

createArray ::  [(Idx a, Double)] -> QTableNoObs a
createArray ls = do
-- ^ creates the new array
  let sortedLs = sortBy (\(x,_) (y,_) -> compare x y) ls
      (actionLs,valueLs) = unzip sortedLs
      lowerBound = minimum actionLs
      upperBound = maximum actionLs
  newArray_ (lowerBound,upperBound)



createArray :: [(a,Double)] -> QTableNoObs a
createArray ls =
  let sortedLs = sortBy (\(x,_) (y,_) -> compare x y) ls
      (actionLs,valueLs) = unzip sortedLs
      lowerBound = minimum actionLs
      upperBound = maximum actionLs
      in MA.newListArray (lowerBound,upperBound) valueLs


-- Choose maximally given a learned Qmatrix
maxScore2 ::
     ( ToIdx a
     , Ord a
     , Functor o
     , Ix (o (Idx a))
     , Ix (Memory.Vector n (o (Idx a)))
     , MonadIO m
     )
  => QTable n o a
  -> Int
  -> m (Double, a)
maxScore2 table0 player = do
  valuesAndActions <-
    liftIO
      (V.mapM
         (\action -> do
            let index = (obs, toIdx action)
            value <- A.readArray table0 index
            pure (value, action))
         (population support))
  let !maximum' = V.maximum valuesAndActions
  pure maximum'
-}


---------------------
-- Main functionality
-- Takes external input and outputs a move? Does evaluate the game as in the Bayesian case.
interactiveInput ::
  (Show a, Ord a) =>
  AgentIO -> [a] -> InteractiveStageGame  '[a] '[[DiagnosticInfoInteractive a]] () () a Double
interactiveInput name ys = OpenGame {
  play =  \(strat ::- Nil) -> L.lens (\x -> strat) (\ _ _ -> ()),
  -- ^ This finds the optimal action or chooses randomly
  evaluate = \(strat ::- Nil) (PureLensContext h k) ->
       let context = deviationsInContext name strat u ys
           u y     = k  y
              in (context  ::- Nil) }

-- Takes a qmatrix and chooses optimally given that information
externalQLearner ::
  (Show a, Ord a) =>
  AgentIO -> [a] -> InteractiveStageGame  '[a] '[[DiagnosticInfoInteractive a]] () () a Double
externalQLearner name ys = OpenGame {
  play =  \(strat ::- Nil) -> L.lens (\x -> strat) (\ _ _ -> ()),
  -- ^ This finds the optimal action or chooses randomly
  evaluate = \(strat ::- Nil) (PureLensContext h k) ->
       let context = deviationsInContext name strat u ys
           u y     = k  y
              in (context  ::- Nil) }


-- Support functionality for constructing open games
fromLens :: (x -> y) -> (x -> r -> s) -> InteractiveStageGame '[] '[] x s y r
fromLens v u = OpenGame {
  play = \Nil -> L.lens (\x -> v x) (\x r -> (u x r)),
  evaluate = \Nil _ -> Nil}


fromFunctions :: (x -> y) -> (r -> s) -> InteractiveStageGame '[] '[] x s y r
fromFunctions f g = fromLens f (const g)
