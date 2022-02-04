{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTs, OverloadedStrings, MonadComprehensions #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}

-- |

module Engine.QLearning.ExportToStrategies
  (   ) where

import           Control.Arrow  hiding ((+:+))
import           Engine.Engine
import           Engine.BayesianGames 
import qualified Engine.Memory as Memory
import           Engine.QLearning (QTable(..), Idx)
import qualified Engine.QLearning as QLearning
import           Numeric.Probability.Distribution hiding (map, lift, filter)
import           System.IO.Unsafe
---------------------------------------------------------
-- Loads a q matrix from the outside world and transforms
-- it into a proper qmatrix 
---------------------------------------------------------

