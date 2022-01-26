{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE BangPatterns #-}


-- Parts of this file were written by Sjoerd Visscher

module Engine.TLL
  ( List(..)
  , Apply(..)
  , Unappend(..)
  , MapL(..)
  , FoldrL(..)
  , ConstMap(..)
  , SequenceList(..)
  , Natural(..)
  , IndexList(..)
  , type (+:+)
  , (+:+)
  , toPair
  , lastL
  ) where

import Control.Applicative
import Control.DeepSeq
import Data.Kind

infixr 6 ::-
data List ts where
  Nil :: List '[]
  (::-) :: t -> List ts -> List (t ': ts)

type family (+:+) (as :: [*]) (bs :: [*]) :: [*] where
  '[] +:+ bs = bs
  (a ': as) +:+ bs = a ': (as +:+ bs)

(+:+) :: List as -> List bs -> List (as +:+ bs)
(+:+) Nil bs = bs
(+:+) (a ::- as) bs = a ::- as +:+ bs


class Unappend as where
  unappend :: List (as +:+ bs) -> (List as, List bs)

instance Unappend '[] where
  unappend bs = (Nil, bs)

instance Unappend as => Unappend (a ': as) where
  unappend (a ::- abs) = case unappend abs of (as, bs) -> (a ::- as, bs)


instance Show (List '[]) where
    show Nil = "Nil"

instance (Show (List as), Show a)
    => Show (List (a ': as)) where
    show (a ::- rest) =
        show a ++ " ::- " ++ show rest

instance NFData (List '[]) where
    rnf _ = ()

instance (NFData (List as), NFData a)
    => NFData (List (a ': as)) where
    rnf (a ::- rest) =
        let !() = rnf a
        in rnf rest

-- Why the Show instance? This is for the test suite. When I tried
-- with the Eq instance, visibly-the-same values were returning
-- False. With Show used for equality, it passes. I'm not sure why
-- that is, but structural equality via Show is enough for our
-- purposes for now.
instance (Eq (List as), Show a) => Eq (List (a ': as)) where
  x ::- xs == y ::- ys = show x == show y && xs == ys

instance Eq (List '[]) where
  _ == _ = True

---------------------------------
-- Operations to transform output
-- Preliminary apply class

class Apply f a b where
  apply :: f -> a -> b


-- Map
class MapL f xs ys where
  mapL :: f -> List xs -> List ys

instance MapL f '[] '[] where
  mapL _ _ = Nil


instance (Apply f x y, MapL f xs ys)
  => MapL f (x ': xs) (y ': ys) where
  mapL f (x ::- xs) = apply f x ::- mapL f xs

-- Foldr
class FoldrL f acc xs where
  foldrL :: f -> acc -> List xs -> acc

instance FoldrL f acc '[] where
  foldrL _ acc _ = acc

instance (Apply f x (acc -> acc), FoldrL f acc xs)
  => FoldrL f acc (x ': xs) where
  foldrL f acc (x ::- xs) = apply f x $ foldrL f acc xs

type family ConstMap (t :: *) (xs :: [*]) :: [*] where
  ConstMap _      '[]  = '[]
  ConstMap t (x ': xs) = t ': (ConstMap t xs)

-- Produce pair of results, useful for two player interactions
toPair :: List '[a,b] -> Maybe (a,b)
toPair (x ::- y ::- Nil) = Just (x,y)

----------------------------------------
-- Features to ease feeding back outputs
--
class Applicative m => SequenceList m a b | a -> b, m b -> a where
    sequenceListA :: List a -> m (List b)

instance Applicative m => SequenceList m '[] '[] where
    sequenceListA _ = pure Nil

instance (Applicative m, SequenceList m as bs) => SequenceList m (m a ': as) (a ': bs) where
    sequenceListA (a ::- b) = liftA2 (::-) a (sequenceListA b)


-- Indexing on the list

data Nat = Z | S Nat

data Natural a where
  Zero :: Natural 'Z
  Succ :: Natural a -> Natural ('S a)


class IndexList (n :: Nat) (xs :: [Type]) (i :: Type) | n xs -> i where
   fromIndex :: Natural n -> List xs -> i

instance IndexList Z (x ': xs) x where
   fromIndex Zero (x ::- _) = x

instance IndexList n xs a => IndexList (S n) (x ': xs) a where
   fromIndex (Succ n) (_ ::- xs) = fromIndex n xs


--------------------------------------
-- List functionality

headL :: List (a ': as) -> a
headL (x ::- _) = x

tailL :: List (a ': as) -> List as
tailL (_ ::- xs) = xs

type family LastL xs where
  LastL '[x] = x
  LastL (x ': xs) = LastL xs

lastL :: List a -> LastL a
lastL (x ::- Nil)          = x
lastL (x ::- xs@(_ ::- _)) = lastL xs


