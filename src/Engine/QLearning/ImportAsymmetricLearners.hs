{-# LANGUAGE TupleSections #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveGeneric #-}
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
  ( importQMatrixAndStateIndex
  , Action

  ) where


import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Char8 as BC
import Data.Csv
import qualified Data.Vector as V
import           GHC.Generics
import System.Directory (doesFileExist)
import Text.Printf

---------------------------------------------------------
-- Imports the csv that is produced in a learning session
-- NOTE: This ignores the reuse of the existing structure
-- TODO: We should improve the export of the existing
-- files to make our lives simpler
---------------------------------------------------------

----------
-- 0 Types
data StateActionInput = StateActionInput
  { state1 :: Action
  , state2 :: Action
  , action :: Action
  , action_index  :: Int
  } deriving (Show,Generic)
instance FromNamedRecord StateActionInput

data QValueRow  = QValueRow
  { iteration :: Int
  , player :: Int
  , state_action_index :: Int
  , qvalue :: Value
  } deriving (Show, Generic)
instance FromNamedRecord QValueRow

type Action = Double
type Value  = Double


type QValueMatrixTarget = (Action,Action,Action, Value)
instance FromNamedRecord QValueMatrixTarget

---------------------------------------------------
-- 1. Import data in the given state-index file and
-- qmatrix file

-- TODO fix parsing


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
     lsStateActions = [(st1,st2,ac,i)| (StateActionInput st1 st2 ac i) <- V.toList vStateAction]
     lsQValues      = [(saIndex,qv) | (QValueRow _ _ saIndex qv) <- V.toList vQValues]
     transformIndexList :: [(Action,Action,Action,Int)] -> [(Int,Value)] -> [QValueMatrixTarget]
     transformIndexList [] _  = []
     transformIndexList _  [] = []
     transformIndexList ls ((i,v):ivs) = case (findElement ls i) of
       Left _ -> transformIndexList ls ivs
       Right (s11,s12,a1) -> (s11,s12,a1,v) : transformIndexList ls ivs
     findElement :: PrintfArg i => [(Action,Action,Action,Int)] -> i -> Either String (Action,Action,Action)
     findElement [] i = Left $ printf "element %s not found" i
     fineElement ((s11,s12,a1,i1):xs) i
       | i == i1   = Right (s11,s12,a1)
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


