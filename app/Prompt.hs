-- {-# OPTIONS -fglasgow-exts #-}
{-# LANGUAGE GADTs #-}

{- Copyright (c) 2005, R. Kent Dybvig, Simon L. Peyton Jones, and Amr Sabry -}

-- A module for generating new prompt names using a name supply.

-- A _control region_ is identified with the type of its final answer

-- A prompt (Prompt ans a) accepts values of type a; it was generated
-- in a region identified by a final answer of type ans; it can only
-- be used in a region with that final answer. The internal
-- representation of the prompt is just an integer

-- A computation (P ans a) produces values of type a and might
-- generate new prompts in the process; it can only be performed in a
-- region identified by a final answer of type ans. The computation
-- keeps the next of the next prompt to generate as a state variable.

-- P         the type of computations that might generate new prompts
-- Prompt    the abstract type of prompts
-- runP      performs the computation giving a pure answer
-- newPrompt creates a new prompt and updates the index of the next prompt
-- eqPrompt  uses an unsafe coercion 

module Prompt (
  P, Prompt,
  runP, newPromptName, eqPrompt, Equal(..)
) where

-- import GHC.Prim( unsafeCoerce# )  -- GHC.Prim
import Unsafe.Coerce -- ( unsafeCoerce# )
-- The GHC-specific unsafe coerce function

import Control.Applicative (Applicative(..))
import Control.Monad (liftM, ap)
  
------------------------------------------------------------------------

newtype Prompt ans a = Prompt Int

newtype P ans a = P (Int -> (Int,a)) 
unP (P f) = f

instance Functor (P ans) where
  fmap = liftM
  
instance Applicative (P ans) where
  pure = return
  (<*>) = ap


instance Monad (P ans) where
  return e = P (\s -> (s,e))
  (P e1) >>= e2 = P (\s1 -> case e1 s1 of
                              (s2,v1) -> unP (e2 v1) s2)

runP :: P ans ans -> ans
runP pe = snd (unP pe 0)

newPromptName :: P ans (Prompt ans a)
newPromptName = P (\np -> (np+1, Prompt np))

-- The Equal type is a GADT that gives evidence for
-- the equality of two types: note the type of EQUAL
data Equal a b where
  EQUAL     :: Equal a a
  NOT_EQUAL :: Equal a b

eqPrompt :: Prompt ans a -> Prompt ans b -> Equal a b
-- eqPrompt contains the single use of 'unsafeCoerce#'.
-- It embodies the fact (not checkable by the type system)
-- that if two prompts are equal then their types are equal,
-- and returns evidence of that fact.
eqPrompt (Prompt p1) (Prompt p2)  
    | p1 == p2  = unsafeCoerce EQUAL
    | otherwise = NOT_EQUAL
