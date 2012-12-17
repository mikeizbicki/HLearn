{-|
Module      :  GHC.dup
Copyright   :  (c) 2012 Joachim Breitner
License     :  BSD3
Maintainer  :  Joachim Breitner <mail@joachim-breitner.de>
Stability   :  experimental
Portability :  not at all

This module provides two new operations, 'GHC.Dup.dup' and 'GHC.Dup.deepDup',
that allow you to prevent the result of two evaluations of the same
expression to be shared.
-}

{-# LANGUAGE GHCForeignImportPrim, MagicHash, UnboxedTuples, UnliftedFFITypes #-}

{-# OPTIONS_HADDOCK prune #-}

module GHC.Dup (Box(Box), dup, deepDup, deepDupFun) where

import GHC.Exts

-- This is a datatype that has the same layout as Ptr, so that by
-- unsafeCoerce'ing, we obtain the Addr of the wrapped value

-- | The Box datatype allows you to control the time of evaluations of 'dup' or
-- 'deepDup', by pattern-matching on the result.
data Box a = Box a

-- This is for turning an a to something we can pass to a primitive operation

aToWord# :: Any -> Word#
aToWord# a = case Box a of mb@(Box _) -> case unsafeCoerce# mb :: Word of W# addr -> addr

wordToA# :: Word# -> Box Any
wordToA# a = unsafeCoerce# (W# a) :: Box Any

-- This is for actually calling the primitive operation

foreign import prim "dupClosure" dupClosure :: Word# -> Word#

{-# NOINLINE dup #-}
-- | Dup copies a the parameter and returns it. The copy is shallow, i.e.
-- referenced thunks are still shared between the parameter and its copy.
dup :: a -> Box a
dup a =
    case dupClosure (aToWord# (unsafeCoerce# a)) of { x ->
    case wordToA# x of { Box x' ->
        Box (unsafeCoerce# x')
    }}

foreign import prim "deepDupClosure" deepDupClosure :: Word# -> Word#

-- This is like 'deepDup', but with a different type, and should not be used by
-- the programmer.
deepDupFun :: a -> a
deepDupFun a =
    case deepDupClosure (aToWord# (unsafeCoerce# a)) of { x ->
    case wordToA# x of { Box x' ->
        unsafeCoerce# x'
    }}
{-# NOINLINE deepDupFun #-}

-- | This copies the parameter and changes all references therein so that when
-- they are evaluated, they are copied again. This ensures that everything put on the heap by a function that wraps all is parameters in 'deepDup' can be freed after the evaluation.
deepDup :: a -> Box a
deepDup a =
    case deepDupClosure (aToWord# (unsafeCoerce# a)) of { x ->
    case wordToA# x of { Box x' ->
        Box (unsafeCoerce# x')
    }}
{-# NOINLINE deepDup #-}

