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
import           Engine.QLearning
import           Engine.TLL
import qualified Examples.QLearning.BestReply as BestReply
import qualified Examples.QLearning.CalvanoReplication as CalvanoReplication
import qualified Examples.QLearning.CalvanoReplicationHash as CalvanoReplicationHash
import qualified Examples.QLearning.CalvanoReplicationMutable as CalvanoReplicationMutable
import qualified Examples.QLearning.PDDeterministicPlayer as PDDeterministicPlayer

main = do
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
        [ bench
          ("iters/" ++ show i)
          (nf
             (\i ->
                let st = PDDeterministicPlayer.initiateStrat
                    results = PDDeterministicPlayer.evalStageLS st i
                 in results)
             i)
        | i <- iters
        ]
    , bgroup
        "CalvanoReplication"
        [ bgroup
            "evalStageM"
            [ Criterion.Main.env
              (fmap
                 SkipNF
                 (pure
                    (runIdentity
                       (do let !initial = CalvanoReplication.initialStrat
                           CalvanoReplication.sequenceL initial))))
              (\(SkipNF st) ->
                 bench
                   ("iters/" ++ show i)
                   (nf (CalvanoReplication.evalStageM st) i))
            | i <- iters
            ]
        ]
    , bgroup
        "CalvanoReplicationHash"
        [ bgroup
            "evalStageM"
            [ Criterion.Main.env
              (fmap
                 SkipNF
                 (pure
                    (runIdentity
                       (do let !initial = CalvanoReplicationHash.initialStrat
                           CalvanoReplicationHash.sequenceL initial))))
              (\(SkipNF st) ->
                 bench
                   ("iters/" ++ show i)
                   (nf (CalvanoReplicationHash.evalStageM st) i))
            | i <- iters
            ]
        ]
    , bgroup
        "CalvanoReplicationMutable"
        [ bgroup
            "evalStageLS"
            [ Criterion.Main.env
              (fmap SkipNF CalvanoReplicationMutable.initialStrat)
              (\(SkipNF st) ->
                 bench
                   ("iters/" ++ show i)
                   (nfIO
                      (do let xs = CalvanoReplicationMutable.evalStageLS st i
                          traverse_ (\tll -> sequenceListA tll) xs)))
            | i <- iters
            , False
            ]
        , bgroup
            "evalStageM"
            [ Criterion.Main.env
              (fmap
                 SkipNF
                 (do initial <- CalvanoReplicationMutable.initialStrat
                     CalvanoReplicationMutable.sequenceL initial))
              (\(SkipNF st) ->
                 bench
                   ("iters/" ++ show i)
                   (nfIO (CalvanoReplicationMutable.evalStageM st i)))
            | i <- iters
            ]
        ]
    ]
  where
    iters = [10, 100, 1000]

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
