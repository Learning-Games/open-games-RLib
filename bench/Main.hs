{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE BangPatterns, OverloadedStrings #-}
module Main where

import           Control.DeepSeq
import           Control.Exception
import           Criterion
import           Criterion.Main
import           Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as S8
import           Data.Foldable
import           Data.Functor.Identity
import           Data.Maybe
import           Engine.QLearning
import           Engine.TLL
import qualified Examples.QLearning.BestReply as BestReply
import qualified Examples.QLearning.CalvanoReplication as CalvanoReplication
import qualified Examples.QLearning.PDDeterministicPlayer as PDDeterministicPlayer
import           System.Environment
import           Text.Read

main = do
  iters <- fmap (fromMaybe [1 :: Int] . (>>=readMaybe)) (lookupEnv "ITERS")
  defaultMain
    [ bgroup
        "BestReply"
        [ bench
          ("iters/" ++ show i)
          (nf
             (\i ->
                BestReply.evalStageLS
                  (BestReply.initiateStrat BestReply.actionSpace 6)
                  i)
             i)
        | i <- iters
        ]
    , bgroup
        "PDDeterministicPlayer"
        [ Criterion.Main.env
          (fmap
             SkipNF
             (do putStrLn "Allocating ... "
                 initial <- PDDeterministicPlayer.initiateStrat
                 putStrLn "Sequencing ... "
                 xs <- PDDeterministicPlayer.sequenceL initial
                 putStrLn "Pure'ing"
                 pure xs))
          (\(SkipNF st) ->
             bench
               ("iters/" ++ show i)
               (nfIO (PDDeterministicPlayer.evalStageM st i)))
        | i <- iters
        ]
    , bgroup
        "CalvanoReplication"
        [ Criterion.Main.env
          (fmap
             SkipNF
             (do initial <- CalvanoReplication.initialStrat
                 CalvanoReplication.sequenceL initial))
          (\(SkipNF st) ->
             bench
               ("iters/" ++ show i)
               (nfIO (CalvanoReplication.evalStageM st i)))
        | i <- iters
        ]
    ]

newtype SkipNF a = SkipNF a
instance NFData (SkipNF a) where
  rnf _ = ()

{-
main = do
  let results = evalStageLS initiateStrat 100
  let pairLS = fmap toPair results
  print $ head pairLS
--  print $ drop 299900 $ pairLS
  print $ take 100 $ fmap  extractSnd pairLS
  print $ drop 99900 $ fmap extractSnd pairLS
--}
