-- {-# OPTIONS -fglasgow-exts #-}
{-# LANGUAGE RankNTypes, GADTs #-}

{- Copyright (c) 2005, R. Kent Dybvig, Simon L. Peyton Jones, and Amr Sabry -}

-- An implementation of the control operators using an unknown representation
-- of continuations; continuations are reified using Felleisen's C operator
-- as supplied by the module CPS

module CC_ReifiedK (
  CC, Prompt, SubCont,
  runCC,
  newPrompt, pushPrompt, -- operations on prompts
  withSubCont, pushSubCont, -- operations on subcontinuations
) where

import qualified CPS
import Prompt
import Seq

import Control.Applicative (Applicative(..))
import Control.Monad (liftM, ap)

----------------------------------------------------------------------
-- Types

newtype Cont ans a b = Cont (CPS.K (MC ans b) a)
unCont (Cont k) = k

type MetaCont ans a = Seq Cont ans a
type SubCont ans a b = SubSeq Cont ans a b

newtype CC ans a = CC (forall b. CPS.M (MC ans b) a)
unCC (CC c) = c

newtype MC ans b = MC (MetaCont ans b -> P ans ans)
unMC (MC m) = m

---------------------------------------------------------------------
-- CC monad

instance Functor (CC ans) where
  fmap = liftM
  
instance Applicative (CC ans) where
  pure = return
  (<*>) = ap

instance Monad (CC ans) where
  return e = CC (return e)
  (CC e1) >>= e2 = CC (do v1 <- e1; unCC (e2 v1))

-- Applies a control segment (a first level continuation)
appseg :: Cont ans a b -> a -> MC ans b
appseg (Cont k) a = CPS.runM (CPS.throw k (return a))

-- Runs a first level CPS term
runOne :: CC ans a -> MC ans a 
runOne m = CPS.runM (do a <- unCC m; initkF a)
  where initkF a = return (MC (\mk -> appmk mk a))

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
newPrompt =
    CC (CPS.c (\k -> MC (\mk -> 
      do p <- newPromptName
         unMC (appseg (Cont k) p) mk)))

pushPrompt :: Prompt ans a -> CC ans a -> CC ans a
pushPrompt p e = 
    CC (CPS.c (\k -> MC (\mk -> 
        unMC (runOne e) (PushP p (PushSeg (Cont k) mk)))))

withSubCont :: Prompt ans b -> (SubCont ans a b -> CC ans b) -> CC ans a
withSubCont p f = 
    CC (CPS.c (\k -> MC (\mk -> 
        let (subk,mk') = splitSeq p mk
            e = f (appendSubSeq (PushSeg (Cont k)) subk)
        in unMC (runOne e) mk')))

pushSubCont :: SubCont ans a b -> CC ans a -> CC ans b
pushSubCont subk e = 
    CC (CPS.c (\k -> MC (\mk -> 
        unMC (runOne e) (pushSeq subk (PushSeg (Cont k) mk)))))

---------------------------------------------------------------------
