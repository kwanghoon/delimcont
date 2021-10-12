-- {-# OPTIONS -fglasgow-exts #-}
{-# LANGUAGE RankNTypes, GADTs #-}

{- Copyright (c) 2005, R. Kent Dybvig, Simon L. Peyton Jones, and Amr Sabry -}

-- An implementation of the control operators using a representation of the
-- continuation as a sequence of frames 

module CC_Frame (
  CC, Prompt, SubCont, -- abstract types
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

newtype Frame ans a b = Frame (a -> CC ans b)

type Cont ans a = Seq Frame ans a
type SubCont ans a b = SubSeq Frame ans a b

newtype CC ans a = CC (Cont ans a -> P ans ans)
unCC (CC c) = c

--
instance Functor (CC ans) where
  fmap = liftM
  
instance Applicative (CC ans) where
  pure = return
  (<*>) = ap

instance Monad (CC ans) where
  return v = CC (\k -> appk k v)
  (CC e1) >>= e2 = CC (\k -> e1 (PushSeg (Frame e2) k))

-- Applies a control segment
appseg :: Frame ans a b -> a -> CC ans b
appseg (Frame fr) a = fr a 

-- Applies a continuation
appk :: Cont ans a -> a -> P ans ans
appk EmptyS a           = return a
appk (PushP _ k') a        = appk k' a
appk (PushSeg seg k') a = unCC (appseg seg a) k'

-- Runs a CPS term
runTerm :: CC ans ans -> P ans ans
runTerm c = unCC c EmptyS

runCC :: (forall ans. CC ans a) -> a
runCC ce = runP (runTerm ce)

----------------------------------------------------------------------
-- Exported operations

newPrompt :: CC ans (Prompt ans a)
newPrompt = CC (\k -> do p <- newPromptName; appk k p)

-- Make the operation strict in k to avoid accumulation of
-- continuation segments
pushPrompt :: Prompt ans a -> CC ans a -> CC ans a
--pushPrompt p (CC e) = CC (\k -> e (PushP p k))
pushPrompt p (CC e) = CC (\k -> k `seq` e (PushP p k))

withSubCont :: Prompt ans b -> (SubCont ans a b -> CC ans b) -> CC ans a
withSubCont p f = 
    CC (\k -> let (subk, k') = splitSeq p k
              in unCC (f subk) k')

pushSubCont :: SubCont ans a b -> CC ans a -> CC ans b
pushSubCont subk (CC e) = CC (\k -> e (pushSeq subk k))

----------------------------------------------------------------------
