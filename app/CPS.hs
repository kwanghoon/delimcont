
{- Copyright (c) 2005, R. Kent Dybvig, Simon L. Peyton Jones, and Amr Sabry -}

-- A CPS monad supporting Felleisen's C operator

module CPS (
  M, K,
  throw, c, 
  runM
) where

----------------------------------------------------------------------
-- CPS monad

import Control.Applicative (Applicative(..))
import Control.Monad (liftM, ap)
  
newtype K ans a = K (a -> ans)
newtype M ans a = M (K ans a -> ans)

instance Functor (M ans) where
  fmap = liftM
  
instance Applicative (M ans) where
  pure = return
  (<*>) = ap
  
instance Monad (M ans) where
  return e = M (\ (K k) -> k e)
  (M e1) >>= e2 = 
    M (\k -> e1 (K (\ v1 -> let M c = e2 v1 in c k)))

runM :: M ans ans -> ans
runM (M e) = e (K id)

-- Control operators

callcc :: (K ans a -> M ans a) -> M ans a
callcc f = M (\k -> let M c = f k in c k)

abort :: ans -> M ans a
abort a = M (\_ -> a)

throw :: K ans a -> M ans a -> M ans b
throw k (M e) = M (\_ -> e k)

c :: (K ans a -> ans) -> M ans a
c f = callcc (\k -> abort (f k))

----------------------------------------------------------------------
