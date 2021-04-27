{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}

module Engine.OpticClass
  ( Stochastic(..)
  , Vector(..)
  , StochasticStatefulOptic(..)
  , StochasticStatefulContext(..)
  , Optic(..)
  , Precontext(..)
  , Context(..)
  , ContextAdd(..)
  , identity
  ) where


import           Control.Monad.State                hiding (state)
import           Numeric.Probability.Distribution   hiding (lift)



class Optic o where
  lens :: (s -> a) -> (s -> b -> t) -> o s t a b
  (>>>>) :: o s t a b -> o a b p q -> o s t p q
  (&&&&) :: o s1 t1 a1 b1 -> o s2 t2 a2 b2 -> o (s1, s2) (t1, t2) (a1, a2) (b1, b2)
  (++++) :: o s1 t a1 b -> o s2 t a2 b -> o (Either s1 s2) t (Either a1 a2) b

identity :: (Optic o) => o s t s t
identity = lens id (flip const)

class Precontext c where
  void :: c () () () ()

-- Precontext is a separate class to Context because otherwise the typechecker throws a wobbly

class (Optic o, Precontext c) => Context c o where
  cmap :: o s1 t1 s2 t2 -> o a1 b1 a2 b2 -> c s1 t1 a2 b2 -> c s2 t2 a1 b1
  (//) :: (Show s1) => o s1 t1 a1 b1 -> c (s1, s2) (t1, t2) (a1, a2) (b1, b2) -> c s2 t2 a2 b2
  (\\) :: (Show s2) => o s2 t2 a2 b2 -> c (s1, s2) (t1, t2) (a1, a2) (b1, b2) -> c s1 t1 a1 b1

-- (\\) is derivable from (//) using
-- l \\ c = l // (cmap (lift swap swap) (lift swap swap) c)
-- (and vice versa) but it doesn't typecheck and I don't understand why

-- ContextAdd is a separate class to Precontext and Context because its implementation is more ad-hoc,
-- eg. it can't be done generically in a monad

class ContextAdd c where
  prl :: c (Either s1 s2) t (Either a1 a2) b -> Maybe (c s1 t a1 b)
  prr :: c (Either s1 s2) t (Either a1 a2) b -> Maybe (c s2 t a2 b)

-------------------------------------------------------------
--- replicate the old implementation of a stochastic context 
type Stochastic = T Double
type Vector = String -> Double


data StochasticStatefulOptic s t a b where
  StochasticStatefulOptic :: (s -> Stochastic (z, a))
                          -> (z -> b -> StateT Vector Stochastic t)
                          -> StochasticStatefulOptic s t a b

instance Optic StochasticStatefulOptic where
  lens v u = StochasticStatefulOptic (\s -> return (s, v s)) (\s b -> return (u s b))
  (>>>>) (StochasticStatefulOptic v1 u1) (StochasticStatefulOptic v2 u2) = StochasticStatefulOptic v u
    where v s = do {(z1, a) <- v1 s; (z2, p) <- v2 a; return ((z1, z2), p)}
          u (z1, z2) q = do {b <- u2 z2 q; u1 z1 b}
  (&&&&) (StochasticStatefulOptic v1 u1) (StochasticStatefulOptic v2 u2) = StochasticStatefulOptic v u
    where v (s1, s2) = do {(z1, a1) <- v1 s1; (z2, a2) <- v2 s2; return ((z1, z2), (a1, a2))}
          u (z1, z2) (b1, b2) = do {t1 <- u1 z1 b1; t2 <- u2 z2 b2; return (t1, t2)}
  (++++) (StochasticStatefulOptic v1 u1) (StochasticStatefulOptic v2 u2) = StochasticStatefulOptic v u
    where v (Left s1)  = do {(z1, a1) <- v1 s1; return (Left z1, Left a1)}
          v (Right s2) = do {(z2, a2) <- v2 s2; return (Right z2, Right a2)}
          u (Left z1) b  = u1 z1 b
          u (Right z2) b = u2 z2 b

data StochasticStatefulContext s t a b where
  StochasticStatefulContext :: (Show z) => Stochastic (z, s) -> (z -> a -> StateT Vector Stochastic b) -> StochasticStatefulContext s t a b

instance Precontext StochasticStatefulContext where
  void = StochasticStatefulContext (return ((), ())) (\() () -> return ())

instance Context StochasticStatefulContext StochasticStatefulOptic where
  cmap (StochasticStatefulOptic v1 u1) (StochasticStatefulOptic v2 u2) (StochasticStatefulContext h k)
            = let h' = do {(z, s) <- h; (_, s') <- v1 s; return (z, s')}
                  k' z a = do {(z', a') <- lift (v2 a); b' <- k z a'; u2 z' b'}
               in StochasticStatefulContext h' k'
  (//) (StochasticStatefulOptic v u) (StochasticStatefulContext h k)
            = let h' = do {(z, (s1, s2)) <- h; return ((z, s1), s2)}
                  k' (z, s1) a2 = do {(_, a1) <- lift (v s1); (_, b2) <- k z (a1, a2); return b2}
               in StochasticStatefulContext h' k'
  (\\) (StochasticStatefulOptic v u) (StochasticStatefulContext h k)
            = let h' = do {(z, (s1, s2)) <- h; return ((z, s2), s1)}
                  k' (z, s2) a1 = do {(_, a2) <- lift (v s2); (b1, _) <- k z (a1, a2); return b1}
               in StochasticStatefulContext h' k'

instance ContextAdd StochasticStatefulContext where
  prl (StochasticStatefulContext h k)
    = let fs = [((z, s1), p) | ((z, Left s1), p) <- decons h]
       in if null fs then Nothing
                     else Just (StochasticStatefulContext (fromFreqs fs) (\z a1 -> k z (Left a1)))
  prr (StochasticStatefulContext h k)
    = let fs = [((z, s2), p) | ((z, Right s2), p) <- decons h]
       in if null fs then Nothing
                     else Just (StochasticStatefulContext (fromFreqs fs) (\z a2 -> k z (Right a2)))

------------------------------------------------------------
-- 2 Alternative implementation of monadic optic
-- NOTE Missing the branching operator


data MonadOptic m s t a b where
  MonadOptic :: Monad m => (s -> m (z, a))
                          -> (z -> b -> m t)
                          -> MonadOptic m s t a b

instance Monad m => Optic (MonadOptic m) where
  lens v u = MonadOptic (\s -> return (s, v s)) (\s b -> return (u s b))
  (>>>>) (MonadOptic v1 u1) (MonadOptic v2 u2) = MonadOptic v u
    where v s = do {(z1, a) <- v1 s; (z2, p) <- v2 a; return ((z1, z2), p)}
          u (z1, z2) q = do {b <- u2 z2 q; u1 z1 b}
  (&&&&) (MonadOptic v1 u1) (MonadOptic v2 u2) = MonadOptic v u
    where v (s1, s2) = do {(z1, a1) <- v1 s1; (z2, a2) <- v2 s2; return ((z1, z2), (a1, a2))}
          u (z1, z2) (b1, b2) = do {t1 <- u1 z1 b1; t2 <- u2 z2 b2; return (t1, t2)}

data MonadContext m s t a b where
  MonadContext :: (Monad m, Show z) => m (z, s) -> (z -> a -> m b) -> MonadContext m s t a b

instance Monad m => Precontext (MonadContext m) where
  void = MonadContext (return ((), ())) (\() () -> return ())

instance Monad m => Context (MonadContext m)  (MonadOptic m) where
  cmap (MonadOptic v1 u1) (MonadOptic v2 u2) (MonadContext h k)
            = let h' = do {(z, s) <- h; (_, s') <- v1 s; return (z, s')}
                  k' z a = do {(z', a') <- (v2 a); b' <- k z a'; u2 z' b'}
               in MonadContext h' k'
  (//) (MonadOptic v u) (MonadContext h k)
            = let h' = do {(z, (s1, s2)) <- h; return ((z, s1), s2)}
                  k' (z, s1) a2 = do {(_, a1) <- (v s1); (_, b2) <- k z (a1, a2); return b2}
               in MonadContext h' k'
  (\\) (MonadOptic v u) (MonadContext h k)
            = let h' = do {(z, (s1, s2)) <- h; return ((z, s2), s1)}
                  k' (z, s2) a1 = do {(_, a2) <- (v s2); (b1, _) <- k z (a1, a2); return b1}
               in MonadContext h' k'


------------------------------------------------------------
-- 3 Alternative, simple implementation of monadic optic


data MonadicLens m s t a b where
     MonadicLens :: (Monad m) => (s -> a) -> (s -> b -> m t) -> MonadicLens m s t a b

instance (Monad m) => Optic (MonadicLens m) where
  lens v u = MonadicLens (\s -> v s) (\s b -> return (u s b))
  (>>>>) (MonadicLens v1 u1) (MonadicLens v2 u2) = MonadicLens v u
    where v s   = let a1 = v1 s
                      in v2 a1
          u s q = let a1 = v1 s
                      in do  {b' <- u2 a1 q; u1 s b'}
  (&&&&) (MonadicLens v1 u1) (MonadicLens v2 u2) = MonadicLens v u
    where v (s1, s2) = let a1 = v1 s1
                           a2 = v2 s2
                           in (a1,a2)
          u (s1, s2) (b1, b2) = do {t1 <- u1 s1 b1; t2 <- u2 s2 b2; return (t1, t2)}
  (++++) (MonadicLens v1 u1) (MonadicLens v2 u2) = MonadicLens v u
    where v (Left s1)  = let a1 = v1 s1
                             in Left a1
          v (Right s2) = let a2 = v2 s2
                             in  Right a2
          u (Left s1) b  =  u1 s1 b
          u (Right s2) b =  u2 s2 b

data MonadicLensContext m s t a b where
  MonadicLensContext :: (Monad m) =>  s -> (a -> m b) -> MonadicLensContext m s t a b

instance Monad m => Precontext (MonadicLensContext m) where
  void = MonadicLensContext () (\() -> return ())


instance Monad m => Context (MonadicLensContext m) (MonadicLens m) where
  cmap (MonadicLens v1 u1) (MonadicLens v2 u2) (MonadicLensContext h k)
            = let h' = v1 h
                  k' a = let a' = v2 a
                             in do {y <- k a'; u2 a y} 
               in MonadicLensContext h' k'
  (//) (MonadicLens v u) (MonadicLensContext h k)
            = let h' = let (_, s2) = h
                           in s2
                  k' a2 = let (s1,_) = h
                              a1     =  v s1
                              in do {(_,b2) <- k (a1,a2); return b2}
               in MonadicLensContext h' k'
  (\\) (MonadicLens v u) (MonadicLensContext h k)
            = let h' = let (s1, _) = h
                            in s1
                  k' a1 = let (_,s2) = h
                              a2     = v s2
                              in do {(b1,_) <- k (a1,a2); return b1}
                  in MonadicLensContext h' k'

instance Monad m => ContextAdd (MonadicLensContext m) where
  prl (MonadicLensContext h k)
    =  case h of
         Left s1 -> do  Just (MonadicLensContext s1 (k . Left))
         _       -> do  Nothing
  prr (MonadicLensContext h k)
    = case h of
         Right s2 -> Just (MonadicLensContext s2 (k . Right))
         _       -> Nothing


-------------------------------------------------------------
-- 4 Alternative implementations of monadic, stateful optics;
-- the difference is in the action of the monad
-- NOTE no implementation (yet for the (++++) operator)

data MonadicLearnLens m s t a b where
     MonadicLearnLens :: (Monad m) => (s ->  m a) -> (s -> m (b -> m t)) -> MonadicLearnLens m s t a b

instance (Monad m) => Optic (MonadicLearnLens m) where
  lens v u = MonadicLearnLens
                (\s -> pure $ v s)
                (\s -> do 
                    pure $ (\b -> do return (u s b)))
  (>>>>) (MonadicLearnLens v1 u1) (MonadicLearnLens v2 u2) = MonadicLearnLens v u
    where v s1  = do
                     a1 <- v1 s1
                     v2 a1
          u s1  = do
                    a1   <- v1 s1
                    u1'  <- u1 s1
                    u2'  <- u2 a1
                    pure $ (\b2 -> do
                                  b1 <- u2' b2
                                  u1' b1)
  (&&&&) (MonadicLearnLens v1 u1) (MonadicLearnLens v2 u2) = MonadicLearnLens v u
    where v (s1, s2) = do
                         a1 <- v1 s1
                         a2 <- v2 s2
                         pure (a1,a2)
          u (s1, s2) = let u1' = u1 s1
                           u2' = u2 s2
                           in do
                              u1'' <- u1'
                              u2'' <- u2'
                              pure $ (\(b1, b2) -> do
                                                   t1 <- u1'' b1
                                                   t2 <- u2'' b2
                                                   pure (t1,t2))


data MonadicLearnLensContext m s t a b where
  MonadicLearnLensContext :: (Monad m) => m s -> m (a -> m b) -> MonadicLearnLensContext m s t a b

instance Monad m => Precontext (MonadicLearnLensContext m) where
  void = MonadicLearnLensContext (pure ()) (pure $ \() -> return ())


instance Monad m => Context (MonadicLearnLensContext m) (MonadicLearnLens m) where
  cmap (MonadicLearnLens v1 u1) (MonadicLearnLens v2 u2) (MonadicLearnLensContext h k)
            = let h'  = do {s1 <- h; v1 s1}
                  u2' = u2
                  k' = do pure $ (\a1 -> do
                                         s2   <- v2 a1
                                         u2'' <- u2' a1
                                         k''  <- k
                                         b2'' <- k'' s2
                                         u2'' b2'')
               in MonadicLearnLensContext h' k'
  (//) (MonadicLearnLens v u) (MonadicLearnLensContext h k)
            = let h' = do {(s1,s2) <- h; pure s2} 
                  k' = do pure $ (\a2 -> do
                                            k''     <- k
                                            h'      <- h
                                            a1      <- v $ fst h'
                                            (b1,b2) <- k'' (a1,a2)
                                            pure b2) 
                  in MonadicLearnLensContext h' k'
  (\\) (MonadicLearnLens v u) (MonadicLearnLensContext h k)
            = let h' = do {(s1,s2) <- h; pure s1} 
                  k' = do pure $ (\a1 -> do
                                            k''     <- k
                                            h'      <- h
                                            a2      <- v $ snd h'
                                            (b1,b2) <- k'' (a1,a2)
                                            pure b1) 
                  in MonadicLearnLensContext h' k'
