{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTs, OverloadedStrings, MonadComprehensions #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE FlexibleInstances #-}


module Engine.QLearning.ImportAsymmetricLearners
  ( --importQMatrixTarget
   importQMatrixAndStateIndex
  ) where

import Engine.QLearning.ExportAsymmetricLearnersLogReduced (iteration,player,QValueRow(iteration,player,QValueRow),StateActionIndex'(..))
import qualified Engine.QLearning.ExportAsymmetricLearnersLogReduced as L
import Examples.QLearning.AsymmetricLearners.Internal

import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Char8 as BC
import Data.Csv
import qualified Data.Vector as V
import System.Directory (doesFileExist)
import Text.Printf

---------------------------------------------------------
-- Imports the csv that is produced in a learning session
-- NOTE: This assumes that the data has been put into the
-- right shape.
-- NOTE: There is also functionality for reading the data
-- from the existing file shapes.
---------------------------------------------------------

----------
-- 0 Types

-- FIXME Target for construction of policy
type QValueMatrixTarget = ((Observation PriceSpace,PriceSpace),Double)
--instance FromNamedRecord QValueMatrixTarget
--instance FromField (Observation PriceSpace, PriceSpace)

instance FromField PriceSpace where
 parseField s = do
   s' <- parseField s
   pure $ PriceSpace s' 1
   -- ^ FIXME we keep the index undefined -- do not need it anymore
   -- this is essentially just to keep the game intact as before

type StateActionInput = StateActionIndex' (Observation PriceSpace) PriceSpace
instance FromField (Observation PriceSpace) where
  parseField s = do 
    let (s1,s2) = BC.break (== ' ') s
    s1' <- parseField s1
    s2' <- parseField s2
    pure $ Obs (s1',s2')

-- instance FromNamedRecor  PriceSpace where

---------------------------------------------------
-- 1 Import prepared data that is already in shape
-- FIXME change this to a more convient type
--importQMatrixTarget  :: FilePath
--                     -> IO (Either String [QValueMatrixTarget])
--importQMatrixTarget filePath = do
--  fileExists <- doesFileExist filePath
--  if fileExists
--     then (fmap . fmap ) V.toList $ (fmap . fmap) snd $ fmap  decodeByName $ BL.readFile filePath
--     else return . Left $ printf "The file %s does not exist" filePath

---------------------------------------------------
-- 2. Import data in the given state-index file and
-- qmatrix file

-- Import the index
importStateIndexRow  :: FilePath
                     -> IO (Either String (Header, V.Vector StateActionInput))
importStateIndexRow filePath = do
  fileExists <- doesFileExist filePath
  if fileExists
     then decodeByName <$> BL.readFile filePath
     else return . Left $ printf "The file %s does not exist" filePath

-- Import the qvalues
importQMatrix  :: FilePath -> IO (Either String (Header, V.Vector QValueRow))
importQMatrix filePath = do
  fileExists <- doesFileExist filePath
  if fileExists
     then decodeByName <$> BL.readFile filePath
     else return . Left $ printf "The file %s does not exist" filePath

-- Extract the last row
lastIteration :: V.Vector QValueRow -> V.Vector QValueRow
lastIteration v = V.filter (\x -> iteration x == maxIteration ) v
  where
    v' :: V.Vector Int
    v' = V.map iteration v
    maxIteration = maximum $  V.toList v'


-- extract for a given player
extractPlayer :: V.Vector QValueRow -> Int -> V.Vector QValueRow
extractPlayer v player' = V.filter (\x -> player x == player') v


-- Translate state/action index and QValueRow (for one player) into list of associations
-- This prepares the existing file structure for translation into a strategy 
toStateActionQValues :: V.Vector StateActionInput -> V.Vector QValueRow -> [QValueMatrixTarget]
toStateActionQValues vStateAction vQValues =
  transformIndexList lsStateActions lsQValues
  where
     lsStateActions = [((st,ac),i)| (StateActionIndex' st ac i) <- V.toList vStateAction]
     lsQValues      = [(saIndex,qv) | (QValueRow _ _ saIndex qv) <- V.toList vQValues]
     transformIndexList :: PrintfArg i =>  [((Observation PriceSpace,PriceSpace),i)] -> [(i,Double)] -> [QValueMatrixTarget]
     transformIndexList [] _  = []
     transformIndexList _  [] = []
     transformIndexList ls ((i,v):ivs) = case (findElement ls i) of
       Left _ -> transformIndexList ls ivs
       Right (s1,a1) -> ((s1,a1),v) : transformIndexList ls ivs
     findElement :: PrintfArg i => [((s,a),i)] -> i -> Either String (s,a)
     findElement [] i = Left $ printf "element %s not found" i
     fineElement (((s1,a1),i1):xs) i
       | i == i1   = Right (s1,a1)
       | otherwise = findElement xs i 

-- TransformFileInput for a player from the given data structure
importQMatrixAndStateIndex :: FilePath
                           -> FilePath
                           -> Int
                           -> IO (Either String [QValueMatrixTarget])
importQMatrixAndStateIndex filePathStateIndex filePathQMatrix player' = do
  stateIndex <- importStateIndexRow filePathStateIndex
  qMatrix    <- importQMatrix filePathQMatrix
  case stateIndex of
    Left str -> return $ Left str
    Right (h,stateAction) -> case qMatrix of
       Left str' -> return $ Left str'
       Right (h',qValue) ->
         let preparedQValue = extractPlayer (lastIteration qValue) player'
             in return $ Right $ toStateActionQValues stateAction preparedQValue


