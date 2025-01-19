{-# LANGUAGE GADTs #-}

-------------------------------------------------------------------------------
-- |
-- Module      : Control.Monad.CC.Seq
-- Copyright   : (c) R. Kent Dybvig, Simon L. Peyton Jones and Amr Sabry
-- License     : MIT
--
-- Maintainer  : Dan Doel
-- Stability   : Experimental
-- Portability : Non-portable (generalized algebraic datatypes)
--
-- A monadic treatment of delimited continuations.
--
--    Adapted from the paper
--      /A Monadic Framework for Delimited Continuations/,
--    by R. Kent Dybvig, Simon Peyton Jones and Amr Sabry
--      (<http://www.cs.indiana.edu/~sabry/papers/monadicDC.pdf>)
--
-- This module implements the generalized sequence type used as a stack of
-- frames representation of the delimited continuations.
module Control.Monad.CC.Seq (
        -- * Sequence datatype
        Seq(..),
        -- * Sub-sequences
        SubSeq,
        appendSubSeq,
        pushSeq,
        splitSeq,
    ) where

import Control.Monad.CC.Prompt

-- | This is a generalized sequence datatype, parameterized by three types:
-- seg : A constructor for segments of the sequence. 
--
-- ans : the type resulting from applying all the segments of the sequence.
-- Also used as a region parameter.
--
-- a   : The type expected as input to the sequence of segments.
data Seq seg ans a where
    EmptyS  :: Seq seg ans ans
    PushP   :: Prompt ans a -> Seq seg ans a -> Seq seg ans a
    PushSeg :: seg ans a b -> Seq seg ans b -> Seq seg ans a

-- | A type representing a sub-sequence, which may be appended to a sequence
-- of appropriate type. It represents a sequence that takes values of type
-- a to values of type b, and may be pushed onto a sequence that takes values
-- of type b to values of type ans.
-- 適切な型のシーケンスに追加できる部分シーケンスを表す型です。
-- この型は、型 a の値を型 b の値に変換するシーケンスを表し、型 b の値を型 ans の値に変換するシーケンスに追加することができます。
type SubSeq seg ans a b = Seq seg ans b -> Seq seg ans a

-- | The null sub-sequence
emptySubSeq :: SubSeq seg ans a a
emptySubSeq = id

-- | Concatenate two subsequences
appendSubSeq :: SubSeq seg ans a b -> SubSeq seg ans b c -> SubSeq seg ans a c
appendSubSeq = (.) -- SubSeqとSubSeqを関数合成

-- | Push a sub-sequence onto the front of a sequence
pushSeq :: SubSeq seg ans a b -> Seq seg ans b -> Seq seg ans a
pushSeq = ($) -- SubSeqはSeqを引数とする関数なので関数適用

-- | Splits a sequence at the given prompt into a sub-sequence, and the rest of the sequence
-- 与えられたプロンプトとシーケンスをサブシーケンスと残りのシーケンスに分割する。
splitSeq :: Prompt ans b -> Seq seg ans a -> (SubSeq seg ans a b, Seq seg ans b)
splitSeq _ EmptyS = error "Prompt was not found on the stack." -- Seqがない場合はエラー
splitSeq p (PushP p' sk) =
    -- プロンプトが一致するかどうか
    case eqPrompt p' p of
         -- 同じならサブシーケンスは空、残りのシーケンスはsk
         EQU -> (emptySubSeq, sk)
         -- 違うなら再帰して得たサブシーケンスにPushP p'を追加して、残りのシーケンスはsk'をそのまま使う
         NEQ -> case splitSeq p sk of
                     (subk, sk') -> (appendSubSeq (PushP p') subk, sk')
splitSeq p (PushSeg seg sk) =
    case splitSeq p sk of
         (subk, sk') -> (appendSubSeq (PushSeg seg) subk, sk')

