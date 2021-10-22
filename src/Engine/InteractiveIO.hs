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

type InteractiveStageGame m a b x s y r = OpenGame (MonadOptic m) (MonadContext m) a b x s y r

data DiagnosticInfoInteractive y = DiagnosticInfoInteractive
  { player          :: String
  , optimalMove     :: y
  , optimalPayoff   :: Double
  , currentMove     :: y
  , currentPayoff   :: Double}

showDiagnosticInfoInteractive :: (Show y, Ord y) => DiagnosticInfoInteractive y -> String
showDiagnosticInfoInteractive info =
     "\n"    ++ "Player: " ++ player info
     ++ "\n" ++ "Optimal Move: " ++ (show $ optimalMove info)
     ++ "\n" ++ "Optimal Payoff: " ++ (show $ optimalPayoff info)
     ++ "\n" ++ "Current Move: " ++ (show $ currentMove info)
     ++ "\n" ++ "Current Payoff: " ++ (show $ currentPayoff info)



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
generateOutput :: forall xs.
               ( MapL   PrintOutput xs     (ConstMap String xs)
               , FoldrL Concat String (ConstMap String xs)
               ) => List xs -> IO ()
generateOutput hlist = putStrLn $
  "----Analytics begin----" ++ (foldrL Concat "" $ mapL @_ @_ @(ConstMap String xs) PrintOutput hlist) ++ "----Analytics end----\n"





type Agent = String

support :: Stochastic x -> [x]
support = map fst . decons

bayes :: (Eq y) => Stochastic (x, y) -> y -> Stochastic x
bayes a y = mapMaybe (\(x, y') -> if y' == y then Just x else Nothing) a


test :: Monad m =>  (y -> m Double) -> [y] -> m [(y, Double)]
test u ys = do
  ls  <- mapM u ys
  pure $ zip ys ls



deviationsInContext :: (Show a, Ord a, MonadIO m)
                    =>  Agent -> a -> (a -> m Double) -> [a] -> m [DiagnosticInfoInteractive a]
deviationsInContext name strategy u ys = do
   ls  <- mapM u ys
   strategicPayoff <- u strategy
   let zippedLs =zip ys ls
       (optimalPlay, optimalPayoff) = maximumBy (comparing snd) zippedLs
   pure [DiagnosticInfoInteractive
        {  player = name
         , optimalMove = optimalPlay
         , optimalPayoff = optimalPayoff
         , currentMove   = strategy
         , currentPayoff = strategicPayoff
         }]



-- Takes IO and does an evaluate? As in the Bayesian Game?
interactiveInput ::
  (MonadIO m, Show a, Ord a) =>
  Agent -> [a] -> InteractiveStageGame m '[ m a] '[m [DiagnosticInfoInteractive a]] () () a Double
interactiveInput name ys = OpenGame {
  play =  \(strat ::- Nil) -> let  v obs = do
                                           action' <- strat
                                           pure ((),action')
                                        in MonadOptic v (\_ -> (\_ -> pure ())),
  -- ^ This finds the optimal action or chooses randomly
  evaluate = \(a ::- Nil) (MonadContext h k) ->
       let context = do
              (z,_) <- h
              strategy <- a
              let u y = k z y
              deviationsInContext name strategy u ys
              in (context  ::- Nil) }
