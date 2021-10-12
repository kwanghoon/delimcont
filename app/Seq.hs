-- {-# OPTIONS -fglasgow-exts #-}
{-# LANGUAGE GADTs #-}

{- Copyright (c) 2005, R. Kent Dybvig, Simon L. Peyton Jones, and Amr Sabry -}

-- A module defining sequences of control segments and prompts
-- (includes improvements suggested by Oleg Kiselyov 
--  using a type of SubSeq)

module Seq (
  Seq (..), SubSeq, 
  appendSubSeq, pushSeq, splitSeq
) where

import Prompt

------------------------------------------------------------------------

-- A sequence (Seq contseg ans a) consists of a sequence of control
-- segments built using the type constructor contseg. The sequence
-- receives a value of type a and returns a final answer of type ans.
--
-- Seq is defined as a GADT.  Note in particular the type of EmptyS!

data Seq contseg ans a where 
    EmptyS  :: Seq contseg ans ans
    PushP   :: Prompt ans a -> Seq contseg ans a -> Seq contseg ans a
    PushSeg :: contseg ans a b -> Seq contseg ans b -> Seq contseg ans a

-- A subsequence (SubSeq contseg ans a b) is a sequence that receives values
-- of type a and returns an intermediate value of type b; it can only be
-- pushed on sequences which return a final answer of type ans. The
-- subsequence is represented as a function which when given a b-receiving
-- sequence it builds an a-receiving sequence.

type SubSeq contseg ans a b = Seq contseg ans b -> Seq contseg ans a 

emptySubSeq :: SubSeq contseg ans a a 
emptySubSeq = id

appendSubSeq :: SubSeq contseg ans a b
             -> SubSeq contseg ans b c
             -> SubSeq contseg ans a c
appendSubSeq = (.)

pushSeq :: SubSeq contseg ans a b -> Seq contseg ans b -> Seq contseg ans a
-- Push a SubSeq onto the front of a Seq, returning a Seq
pushSeq = ($)

splitSeq :: Prompt ans b -> Seq contseg ans a -> 
            (SubSeq contseg ans a b, Seq contseg ans b)
-- Split a sequence at a prompt into
-- a subsequence and the remaining sequence.
splitSeq p EmptyS
  = error ("Prompt was not found on the stack")
splitSeq p (PushP p' sk)
  = case eqPrompt p' p of 
      EQUAL     -> (emptySubSeq, sk)
      NOT_EQUAL -> case splitSeq p sk of
                     (subk,sk') -> (appendSubSeq (PushP p') subk, sk')
splitSeq p (PushSeg seg sk)
  = case splitSeq p sk of
      (subk,sk') -> (appendSubSeq (PushSeg seg) subk, sk')

----------------------------------------------------------------------
