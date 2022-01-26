{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE NamedFieldPuns #-}
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
{-# LANGUAGE DeriveGeneric #-}

module Examples.QLearning.AsymmetricLearners.MarkovInternal
  where

import           Control.DeepSeq
import           Control.Monad.IO.Class
import qualified Data.Aeson as Aeson
import           Data.Array.IO as A
import qualified Data.Array.MArray as MA
import qualified Data.Array.IArray as IA
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.Builder as SB
import           Data.Csv
import           Data.Double.Conversion.ByteString
import           Data.Foldable
import           Data.Hashable
import           Data.HashMap
import qualified Data.Ix as I
import qualified Data.Vector as V
import qualified Data.Vector.Sized as SV
import           Engine.Engine hiding (fromLens,Agent,fromFunctions,discount)
import qualified Engine.Memory as Memory
import           Engine.OpenGames
import           Engine.OpticClass
import           Engine.TLL
import           Engine.IOGames
import           Data.Utils
import           FastCsv
import           GHC.Generics
import           Path
import qualified Path.IO as IO
import           Preprocessor.Compile
import           RIO (RIO, GLogFunc, runRIO, when)
import           System.Random.MWC.CondensedTable
import           System.Random
import qualified System.Random as Rand
import           System.Process.Typed
import           Examples.QLearning.AsymmetricLearners.Internal

stageMC :: Int
        -> [PriceSpace]
        -> Int
        -> [PriceSpace]
        -> Parameters
        -> Double
        -> OpenGame
              MonadOpticMarkov
              MonadContextMarkov
              '[ (Kleisli CondensedTableV (Observation PriceSpace) PriceSpace)
               , (Kleisli CondensedTableV (Observation PriceSpace) PriceSpace)]
              '[(IO (DiagnosticsMC PriceSpace))
               ,(IO (DiagnosticsMC PriceSpace))]
              (Observation PriceSpace, Observation PriceSpace)
              ()
              (Observation PriceSpace, Observation PriceSpace)
              ()
stageMC sample1 actionSpace1 sample2 actionSpace2 parameters discountFactor = [opengame|
   inputs    : (state1,state2) ;
   feedback  :      ;

   :-----------------:
   inputs    :  state1    ;
   feedback  :      ;
   operation : dependentDecisionIO "Player1" sample1 actionSpace1 ;
   outputs   :  p1 ;
   returns   :  profit1 parameters p1 p2 ;

   inputs    : state2     ;
   feedback  :      ;
   operation : dependentDecisionIO "Player2" sample2 actionSpace2  ;
   outputs   :  p2 ;
   returns   :  profit2 parameters p1 p2    ;

   operation : discount "player1" (\x -> x * discountFactor) ;

   operation : discount "player2" (\x -> x * discountFactor) ;

   :-----------------:

   outputs   :  (Obs (p1, p2), Obs (p1,p2))    ;
   returns   :      ;

|]
