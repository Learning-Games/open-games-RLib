{-# LANGUAGE BangPatterns #-}
-- |

module GoldenTests where

import           Control.DeepSeq
import           Control.Exception
import           Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as S8
import           Data.Foldable
import           Data.Functor.Identity
import           Engine.QLearning
import           Engine.TLL
import qualified Examples.QLearning.BestReply as BestReply
import qualified Examples.QLearning.CalvanoReplication as CalvanoReplication
import qualified Examples.QLearning.CalvanoReplicationMutable as CalvanoReplicationMutable
import qualified Examples.QLearning.PDDeterministicPlayer as PDDeterministicPlayer
import           Test.Hspec

main =
  hspec
    (describe
       "CalvanoReplication"
       (sequence_
          [ it
            ("evalStageM = evalStageLS [" ++ show i ++ " iters]")
            (do st <- CalvanoReplicationMutable.initialStrat
                let xs = CalvanoReplicationMutable.evalStageLS st i
                steps <- traverse (\tll -> sequenceListA tll) xs
                -- Monadic mutable version
                initial <- CalvanoReplicationMutable.initialStrat
                st <- CalvanoReplicationMutable.sequenceL initial
                lastStep <-
                  CalvanoReplicationMutable.evalStageM
                    st
                    -- Below: The original evalStageLS computes one-too-many steps.
                    (i + 1)
                shouldBe lastStep steps)
          | i <- [1, 3, 5]
          ]))
