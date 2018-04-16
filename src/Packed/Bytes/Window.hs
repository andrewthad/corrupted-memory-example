{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE BinaryLiterals #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeInType #-}
{-# LANGUAGE UnboxedSums #-}
{-# LANGUAGE UnboxedTuples #-}

{-# OPTIONS_GHC
 -Weverything
 -fno-warn-unsafe
 -fno-warn-implicit-prelude
 -O2
#-}

module Packed.Bytes.Window
  ( foldr
  ) where

import Prelude hiding (foldr)

import Data.Primitive (ByteArray)
import Data.Word (Word8)
import qualified Data.Primitive as PM

unsafeIndex :: ByteArray -> Int -> Word8
unsafeIndex = PM.indexByteArray

foldr ::
     Int
  -> Int
  -> (Word8 -> a -> a)
  -> a
  -> ByteArray
  -> a
foldr off0 len f a0 arr = go off0 where
  !end = off0 + len
  go !ix = if ix < end
    then f (unsafeIndex arr ix) (go (ix + 1))
    else a0

