{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ScopedTypeVariables, TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}

-- | Memory for the engine.

module Engine.Memory
  ( Vector
  , Memory(..)
  ) where

import           Control.DeepSeq
import           Data.Function
import           Data.Ix
import           Data.Ord
import qualified Data.Vector.Sized as SV
import           GHC.TypeLits

--------------------------------------------------------------------------------
-- Cased vector

-- | A vector where we specialize small sizes.
data family Vector (n :: Nat) a

newtype instance Vector 1 a = V1 {unV1 :: a} deriving (NFData, Ord, Eq, Ix, Functor)

newtype instance Vector 2 a = V2 (a, a) deriving (NFData, Ord, Eq, Ix, Functor)

newtype instance Vector 3 a = VN (SV.Vector 3 a) deriving (NFData, Ord, Eq, Ix, Functor)

--------------------------------------------------------------------------------
-- Memory

-- | Operations you can perform on a memory vector.
class Memory size where
  pushEnd :: Vector size a -> a -> Vector size a
  fromSV :: SV.Vector size a -> Vector size a

--------------------------------------------------------------------------------
-- Memory instances

instance Memory 1 where
  pushEnd _ a = V1 a
  {-# INLINE pushEnd #-}
  fromSV s = V1 (SV.head s)

instance Memory 2 where
  pushEnd (V2 (_,prev)) next = V2 (prev, next)
  {-# INLINE pushEnd #-}
  fromSV s = V2 (SV.head s, SV.head (SV.tail s))

instance Memory 3 where
  pushEnd (VN vec) a =
    VN
      (SV.knownLength
         vec
         (SV.imap
            (\idx _ ->
               if fromIntegral idx == SV.length vec - 1
                 then a
                 else SV.index vec (idx + 1))
            vec))
  {-# INLINE pushEnd #-}
  fromSV = VN
