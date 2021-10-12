-- {-# OPTIONS -fglasgow-exts #-}
{-# LANGUAGE RankNTypes, GADTs #-}

{- Copyright (c) 2005, R. Kent Dybvig, Simon L. Peyton Jones, and Amr Sabry -}

-- An implementation of the control operations using the standard
-- representation of continuations as functions
-- (includes improvements suggested by Oleg Kiselyov)

module CC_Function (
  CC, Prompt, SubCont, 
  runCC,
  newPrompt, pushPrompt, -- operations on prompts
  withSubCont, pushSubCont, -- operations on subcontinuations
) where

import Prompt
import Seq

import Control.Applicative (Applicative(..))
import Control.Monad (liftM, ap)

----------------------------------------------------------------------
-- CC monad

newtype Cont ans a b = Cont (a -> MC ans b)
unCont (Cont k) = k

type MetaCont ans a = Seq Cont ans a 
type SubCont ans a b = SubSeq Cont ans a b

newtype CC ans a = CC (forall b. Cont ans a b -> MC ans b)
unCC (CC c) = c

newtype MC ans b = MC (MetaCont ans b -> P ans ans)
unMC (MC m) = m

--

instance Functor (CC ans) where
  fmap = liftM
  
instance Applicative (CC ans) where
  pure = return
  (<*>) = ap

instance Monad (CC ans) where  
  return e = CC (\ (Cont k) -> k e)
  (CC e1) >>= e2 = CC (\k -> e1 (Cont (\v1 -> unCC (e2 v1) k)))

-- Applies a control segment (a first level continuation)
appseg :: Cont ans a b -> a -> MC ans b
appseg (Cont k) a = k a

-- Runs a first level CPS term
runOne :: CC ans a -> MC ans a
runOne c = unCC c initk 
  where initk = Cont (\a -> MC (\mk -> appmk mk a))

-- Applies a second level continuation
appmk :: MetaCont ans a -> a -> P ans ans
appmk EmptyS a = return a
appmk (PushP _ mk') a = appmk mk' a 
appmk (PushSeg k mk') a = unMC (appseg k a) mk'

-- Runs a second level CPS term
runTwo :: MC ans ans -> P ans ans
runTwo c = unMC c EmptyS

runCC :: (forall ans. CC ans a) -> a
runCC ce = runP (runTwo (runOne ce))

----------------------------------------------------------------------
-- Exported operations 

newPrompt :: CC ans (Prompt ans a)
newPrompt = CC (\k -> MC (\mk -> do p <- newPromptName
                                    unMC (appseg k p) mk))

pushPrompt :: Prompt ans a -> CC ans a -> CC ans a
pushPrompt p e = 
    CC (\k -> MC (\mk -> 
      unMC (runOne e) (PushP p (PushSeg k mk))))
    
withSubCont :: Prompt ans b -> (SubCont ans a b -> CC ans b) -> CC ans a
withSubCont p f = 
    CC (\k -> MC (\mk -> 
       let (subk,mk') = splitSeq p mk
           e = f (appendSubSeq (PushSeg k) subk)
       in unMC (runOne e) mk'))

pushSubCont :: SubCont ans a b -> CC ans a -> CC ans b
pushSubCont subk e = 
    CC (\k -> MC (\mk -> 
       unMC (runOne e) (pushSeq subk (PushSeg k mk))))

----------------------------------------------------------------------
