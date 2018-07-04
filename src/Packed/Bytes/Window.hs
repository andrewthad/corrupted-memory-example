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
  ( findByte
  , findAnyByte2
  ) where

import Prelude hiding (foldr)

import Data.Primitive (ByteArray(ByteArray))
import Data.Word (Word8)
import GHC.Types (RuntimeRep,TYPE)
import GHC.Int (Int(I#))
import GHC.Word (Word8(W8#),Word(W#))
import GHC.Exts (Int#,Word#,ByteArray#,byteSwap#,word2Int#,(+#),int2Word#)
import Data.Bits (xor,(.|.),(.&.),complement,unsafeShiftL,finiteBitSize,
  unsafeShiftR,countLeadingZeros)
import Control.Monad.ST (ST,runST)
import qualified Data.Char
import qualified Data.Primitive as PM

type Maybe# (a :: TYPE (r :: RuntimeRep)) = (# (# #) | a #)

boxMaybeInt :: Maybe# Int# -> Maybe Int
boxMaybeInt = \case
  (# | a #) -> Just (I# a)
  (# (# #) | #) -> Nothing

boxMaybeIntWord8 :: Maybe# (# Int#, Word# #) -> Maybe (Int,Word8)
boxMaybeIntWord8 = \case
  (# | (# i,w #) #) -> Just (I# i, W8# w)
  (# (# #) | #) -> Nothing

unboxInt :: Int -> Int#
unboxInt (I# i) = i

-- | Finds the first occurrence of the given byte.
{-# INLINE findByte #-}
findByte :: Int -> Int -> Word8 -> ByteArray -> Maybe Int
findByte (I# off) (I# len) (W8# w) (ByteArray arr) =
  boxMaybeInt (findByte' off len w arr)

{-# NOINLINE findByte' #-}
findByte' :: Int# -> Int# -> Word# -> ByteArray# -> Maybe# Int#
findByte' !off# !len0# !w0# !arr0# = 
  let !off = I# off#
      !len0 = I# len0#
      !end0 = off + len0
      !beginMachWord = alignUp off
      !endMachWord = alignDown end0
   in if len0 < PM.sizeOf (undefined :: Word)
        then go off end0
        else case go off (beginMachWord * PM.sizeOf (undefined :: Word)) of
          (# | ix #) -> (# | ix #)
          (# (# #) | #) -> case goMachWord beginMachWord endMachWord (broadcastWord8 w) of
            (# | ix #) -> (# | ix #)
            (# (# #) | #) -> case go (endMachWord * PM.sizeOf (undefined :: Word)) end0 of
              (# | ix #) -> (# | ix #)
              (# (# #) | #) -> (# (# #) | #)
  where
  !w = W8# w0#
  !arr = ByteArray arr0#
  go :: Int -> Int -> Maybe# Int#
  go !ix !end = if ix < end
    then if PM.indexByteArray arr ix == w
      then (# | unboxInt ix #)
      else go (ix + 1) end
    else (# (# #) | #)
  -- The start and end index here are given in machine Word elements,
  -- not Word8 elements.
  goMachWord :: Int -> Int -> Word -> Maybe# Int#
  goMachWord !ix !end !artifact = if ix < end
    then case detectArtifact (unsafeIndexWord arr ix) artifact of
      0 -> goMachWord (ix + 1) end artifact
      _ -> go -- this call to go should always return Just
        (ix * PM.sizeOf (undefined :: Word)) 
        ((ix + 1) * PM.sizeOf (undefined :: Word))
    else (# (# #) | #)

-- | Finds the first occurrence of either of the two given bytes.
--   Returns the index of the matching byte and the byte itself.
{-# INLINE findAnyByte2 #-}
findAnyByte2 :: Int -> Int -> Word8 -> Word8 -> ByteArray -> Maybe (Int,Word8)
findAnyByte2 (I# off) (I# len) (W8# w1) (W8# w2) (ByteArray arr) =
  boxMaybeIntWord8 (findAnyByte2' off len w1 w2 arr)

{-# NOINLINE findAnyByte2' #-}
findAnyByte2' :: Int# -> Int# -> Word# -> Word# -> ByteArray# -> Maybe# (# Int#,Word# #)
findAnyByte2' !off# !len0# !wa0# !wb0# !arr0# = 
  let !off = I# off#
      !len0 = I# len0#
      !end0 = off + len0
      !beginMachWord = alignUp off
      !endMachWord = alignDown end0
   in if len0 < PM.sizeOf (undefined :: Word)
        then go off end0
        else case go off (beginMachWord * PM.sizeOf (undefined :: Word)) of
          (# | ix #) -> (# | ix #)
          (# (# #) | #) -> case goMachWord beginMachWord endMachWord (broadcastWord8 wa) (broadcastWord8 wb) of
            (# | ix #) -> (# | ix #)
            (# (# #) | #) -> case go (endMachWord * PM.sizeOf (undefined :: Word)) end0 of
              (# | ix #) -> (# | ix #)
              (# (# #) | #) -> (# (# #) | #)
  where
  !wa = W8# wa0#
  !wb = W8# wb0#
  !arr = ByteArray arr0#
  go :: Int -> Int -> Maybe# (# Int#, Word# #)
  go !ix !end = if ix < end
    then if PM.indexByteArray arr ix == wa
      then (# | (# unboxInt ix, wa0# #) #)
      else if PM.indexByteArray arr ix == wb
        then (# | (# unboxInt ix, wb0# #) #)
        else go (ix + 1) end
    else (# (# #) | #)
  -- The start and end index here are given in machine Word elements,
  -- not Word8 elements.
  goMachWord :: Int -> Int -> Word -> Word -> Maybe# (# Int#, Word# #)
  goMachWord !ix !end !artifactA !artifactB = if ix < end
    then
      let !theByte = unsafeIndexWord arr ix
      in case detectArtifact theByte artifactA .|. detectArtifact theByte artifactB of
        0 -> goMachWord (ix + 1) end artifactA artifactB
        _ -> go -- this call to go should always return Just
          (ix * PM.sizeOf (undefined :: Word)) 
          ((ix + 1) * PM.sizeOf (undefined :: Word))
    else (# (# #) | #)


-- cast a Word8 index to a machine Word index, rounding up
alignUp :: Int -> Int
alignUp i =
  let !(!quotient,!remainder) = quotRem i (PM.sizeOf (undefined :: Word))
   in case remainder of
        0 -> quotient
        _ -> quotient + 1

-- cast a Word8 index to a machine Word index, rounding down
alignDown :: Int -> Int
alignDown i = quot i (PM.sizeOf (undefined :: Word))

broadcastWord8 :: Word8 -> Word
broadcastWord8 !w0 = go 8 (fromIntegral w0) where
  go :: Int -> Word -> Word
  go !n !w = if n < 8 * PM.sizeOf (undefined :: Word)
    then go (twice n) (unsafeShiftL w n .|. w)
    else w

twice :: Int -> Int
twice n = n * 2

-- returns non-zero if a null byte is present in the machine word
detectNull :: Word -> Word
detectNull x = (x - repeatHexZeroOne) .&. complement x .&. repeatHexEightZero

detectArtifact :: Word -> Word -> Word
detectArtifact x artifact = detectNull (applyArtifact x artifact)

applyArtifact :: Word -> Word -> Word
applyArtifact = xor

repeatHexZeroOne :: Word
repeatHexZeroOne = div maxBound 255

repeatHexEightZero :: Word
repeatHexEightZero = 128 * (div maxBound 255 :: Word)

-- this is only used internally
unsafeIndexWord :: ByteArray -> Int -> Word
unsafeIndexWord = PM.indexByteArray

