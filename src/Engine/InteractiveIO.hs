{-# LANGUAGE DatatypeContexts #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE NamedFieldPuns, PolyKinds #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveGeneric #-}
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
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE MultiParamTypeClasses #-}
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

import           Engine.OpenGames hiding (lift)
import           Engine.OpticClass
import           Engine.TLL

import Control.Monad.Reader
import Data.Foldable
import Data.List (maximumBy)
import Data.Ord (comparing)
import Numeric.Probability.Distribution hiding (map, lift, filter)

import qualified Optics.Lens as L

type InteractiveStageGame a b x s y r = OpenGame PureLens PureLensContext a b x s y r

data DiagnosticInfoInteractive y = DiagnosticInfoInteractive
  { playerIO          :: String
  , optimalMoveIO     :: y
  , optimalPayoffIO   :: Double
  , currentMoveIO     :: y
  , currentPayoffIO   :: Double}

showDiagnosticInfoInteractive :: (Show y, Ord y) => DiagnosticInfoInteractive y -> String
showDiagnosticInfoInteractive info =
     "\n"    ++ "Player: " ++ playerIO info
     ++ "\n" ++ "Optimal Move: " ++ (show $ optimalMoveIO info)
     ++ "\n" ++ "Optimal Payoff: " ++ (show $ optimalPayoffIO info)
     ++ "\n" ++ "Current Move: " ++ (show $ currentMoveIO info)
     ++ "\n" ++ "Current Payoff: " ++ (show $ currentPayoffIO info)



-- output string information for a subgame expressions containing information from several players - bayesian
showDiagnosticInfoLInteractive :: (Show y, Ord y)  => [DiagnosticInfoInteractive y] -> String
showDiagnosticInfoLInteractive [] = "\n --No more information--"
showDiagnosticInfoLInteractive (x:xs)  = showDiagnosticInfoInteractive x ++ "\n --other game-- " ++ showDiagnosticInfoLInteractive xs


data PrintOutput = PrintOutput

instance (Show y, Ord y) => Apply PrintOutput [DiagnosticInfoInteractive y] String where
  apply _ x = showDiagnosticInfoLInteractive x


data Concat = Concat

instance Apply Concat String (String -> String) where
  apply _ x = \y -> x ++ "\n NEWGAME: \n" ++ y


---------------------
-- main functionality

-- all information for all players
generateOutputIO :: forall xs.
               ( MapL   PrintOutput xs     (ConstMap String xs)
               , FoldrL Concat String (ConstMap String xs)
               ) => List xs -> IO ()
generateOutputIO hlist = putStrLn $
  "----Analytics begin----" ++ (foldrL Concat "" $ mapL @_ @_ @(ConstMap String xs) PrintOutput hlist) ++ "----Analytics end----\n"





type AgentIO = String

support :: Stochastic x -> [x]
support = map fst . decons

bayes :: (Eq y) => Stochastic (x, y) -> y -> Stochastic x
bayes a y = mapMaybe (\(x, y') -> if y' == y then Just x else Nothing) a


test :: Monad m =>  (y -> m Double) -> [y] -> m [(y, Double)]
test u ys = do
  ls  <- mapM u ys
  pure $ zip ys ls



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



-- Takes IO and does an evaluate? As in the Bayesian Game?
interactiveInput ::
  (Show a, Ord a) =>
  AgentIO -> [a] -> InteractiveStageGame  '[a] '[[DiagnosticInfoInteractive a]] () () a Double
interactiveInput name ys = OpenGame {
  play =  \(strat ::- Nil) -> let  v obs = v obs
                                        in L.lens v (\_ -> (\_ -> ())),
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
