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
 -- ( importQMatrixAndStateIndex
 -- , Action
-- )
   where

import           Control.Applicative hiding (many)
import           Control.Monad.Identity (Identity)
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Builder as BB
import           Data.Csv
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Builder as TLB
import qualified Data.Vector as V
import           Data.Void (Void(..))
import           Data.Word (Word8)
import qualified Formatting as F
import           GHC.Generics
import           System.Directory (doesFileExist)
import           System.IO (openFile, hGetContents,IOMode(..),hClose)
import qualified Text.Parsec as Parsec
import           Text.Megaparsec
import           Text.Megaparsec.Char (char, string, space, digitChar, newline, punctuationChar)


---------------------------------------------------------
-- Imports the csv that is produced in a learning session
-- NOTE: This ignores the reuse of the existing structure
-- TODO: We should improve the export of the existing
-- files to make our lives simpler
---------------------------------------------------------

-- TODO: Integrate parser output with external data type


----------
-- 0 Types
-- Import type for state action index
data StateActionInput = StateActionInput
  { state1 :: Action
  , state2 :: Action
  , action :: Action
  , action_index  :: Int
  } deriving (Show,Generic)

-- Import type for qvalue matrix
data QValueRow  = QValueRow
  { iteration :: Int
  , player :: Int
  , state_action_index :: Int
  , qvalue :: Value
  } deriving (Show, Generic)

-- Aliases
type Action = Double
type Value  = Double
type QValueMatrixTarget = (Action,Action,Action, Value)
type MParser = Parsec Void String

---------------------------------------------------
-- 1. Import data in the given state-index file and
-- qmatrix file
-- The import of the qmatrix file needs some cooking
-- as the file shape has to be transformed. 

------------------------------
-- Parse state action csv
--parseCsv1 :: MParser [String]
parseStateAction ::  MParser (String,V.Vector StateActionInput)
parseStateAction = do
  header <- parseHeader
  ls <- endBy parseStateActionRow newline
  return (header,V.fromList $ ls)

testParser = do
  sepBy parseStateActionRow newline

-- Parse the header of the file
parseHeader :: MParser String
parseHeader = do
  fst <- parseState
  _ <- char ','
  snd <- headerWords
  _ <- char ','
  thrd <-headerWords
  _    <- newline
  return $ fst ++ "," ++ snd ++ "," ++ thrd

headerWords :: MParser String
headerWords = choice
    [ string "action_index"
    , string "action"]

-- Parse the state into new variables
parseState = do
  str <- string "state" 
  return ("state1,state2")

-- Parse a single QValueRow
--parseQValueRow :: MParser QValueRow
parseStateActionRow :: MParser StateActionInput
parseStateActionRow = do
  state1 <- many $ digitChar <|> char '.'
  _      <- parseWhiteSpace
  state2 <- many $ digitChar <|> char '.'
  _      <- mySeparator
  action <- many $ digitChar <|> char '.'
  _      <- mySeparator
  index  <- many $ digitChar <|> char '.'
  return $ StateActionInput (read state1) (read state2) (read action) (read index)

--mySeparators :: Parsec.Parsec String () String
mySeparator :: MParser Char
mySeparator = char ','
  <|> char '\n'

-- Replace whitespace with ","
parseWhiteSpace :: MParser String
parseWhiteSpace = do
  _ <- char ' '
  return (",")

--------------------
-- Parse q value csv
parseQValues :: MParser (String, V.Vector QValueRow)
parseQValues = do
  str <- parseHeaderQM
  ls <- endBy parseQValueRow newline
  return (str,V.fromList ls)

-- Parse the header of the file
parseHeaderQM :: MParser String
parseHeaderQM = do
  str <- string "iteration,player,state_action_index,qvalue"
  _   <- newline
  return $ str


-- Parse one row
parseQValueRow :: MParser QValueRow
parseQValueRow = do 
  iteration          <- many $ digitChar
  _                  <- mySeparator
  player             <- many $ digitChar
  _                  <- mySeparator
  state_action_value <- many $ digitChar 
  _                  <- mySeparator
  value              <- many $ digitChar <|> char '.'
  return $ QValueRow (read iteration) (read player) (read state_action_value) (read value)

---------------
-- Import files

importStateIndexRow :: FilePath
                    -> IO (Either
                            (ParseErrorBundle String Void) (String, V.Vector StateActionInput))
importStateIndexRow filePath = do
  contents <- readFile filePath
  return $ parse parseStateAction filePath contents

importQMatrix :: FilePath
                    -> IO (Either
                            (ParseErrorBundle String Void) (String, V.Vector QValueRow))
importQMatrix filePath = do
  contents <- readFile filePath
  return $ parse parseQValues filePath contents


-------------------------------
-- Extract relevant information 

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
     lsQValues      = [(i,qv) | (QValueRow _ _ i qv) <- V.toList vQValues]

-- Merges the two lists into the target for the strategies
transformIndexList :: [(Action,Action,Action,Int)] -> [(Int,Value)] -> [QValueMatrixTarget]
transformIndexList [] _  = []
transformIndexList _  [] = []
transformIndexList ls ((i,v):ivs) = case (findElement ls i) of
  Left _ -> transformIndexList ls ivs
  Right (s11,s12,a1) -> (s11,s12,a1,v) : transformIndexList ls ivs

findElement :: [(Action,Action,Action,Int)] -> Int -> Either String (Action,Action,Action)
findElement [] i = Left $ F.formatToString ("Element " F.% F.int F.% " was not found") i 
findElement ((s11,s12,a1,i1):xs) i =
  if  i == i1 then  Right (s11,s12,a1)
              else  findElement xs i 

-- TransformFileInput for a player from the given data structure
importQMatrixAndStateIndex :: String
                           -> String
                           -> Int
                           -> IO (Either (ParseErrorBundle String Void) [QValueMatrixTarget])
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


