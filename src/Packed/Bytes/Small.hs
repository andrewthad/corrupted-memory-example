{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-# OPTIONS_GHC
 -Weverything
 -fno-warn-unsafe
 -fno-warn-implicit-prelude
 -fno-warn-missing-import-lists
 -O2
#-}

module Packed.Bytes.Small
  ( ByteArray(..)
  , empty
  , pack
  , unpack
  , singleton
  , append
  , replicate
  , length
  , take
  , cons
  , uncons
    -- * Unsafe
  , unsafeIndex
  ) where

import Prelude hiding (replicate,length,take,reverse)

import Control.Monad.Primitive (primitive_)
import Control.Monad.ST (runST,ST)
import Data.Primitive.ByteArray (ByteArray(..),MutableByteArray(..))
import GHC.Exts (setByteArray#,word2Int#,byteSwap#)
import GHC.Int (Int(I#))
import GHC.Word (Word8(W8#),Word(W#))
import qualified GHC.OldList as L
import qualified Packed.Bytes.Window as BAW
import qualified Data.Primitive as PM

singleton :: Word8 -> ByteArray
singleton w = runST $ do
  marr <- PM.newByteArray 1
  PM.writeByteArray marr 0 w
  PM.unsafeFreezeByteArray marr

pack :: [Word8] -> ByteArray
pack ws0 = runST $ do
  marr <- PM.newByteArray (L.length ws0)
  let go [] !_ = return ()
      go (w : ws) !ix = writeByteArrayWord8 marr ix w >> go ws (ix + 1)
  go ws0 0
  PM.unsafeFreezeByteArray marr

unpack :: ByteArray -> [Word8]
unpack arr = go 0 where
  go :: Int -> [Word8]
  go !ix = if ix < length arr
    then unsafeIndex arr ix : go (ix + 1)
    else []

append :: ByteArray -> ByteArray -> ByteArray
append !a !b = do
  let !lenA = length a
      !lenB = length b
      !len = lenA + lenB
   in runST $ do
        !marr <- PM.newByteArray len
        PM.copyByteArray marr 0 a 0 lenA
        PM.copyByteArray marr lenA b 0 lenB
        PM.unsafeFreezeByteArray marr

cons :: Word8 -> ByteArray -> ByteArray
cons w arr = runST $ do
  let !lenArr = length arr
  marr <- PM.newByteArray (lenArr + 1)
  PM.writeByteArray marr 0 w
  PM.copyByteArray marr 1 arr 0 lenArr
  PM.unsafeFreezeByteArray marr

uncons :: ByteArray -> Maybe (Word8, ByteArray)
uncons arr0 = if lenArr > 0
  then
    let !arr1 = runST $ do
          marr <- PM.newByteArray (lenArr - 1)
          PM.copyByteArray marr 0 arr0 1 (lenArr - 1)
          PM.unsafeFreezeByteArray marr
        w :: Word8
        !w = PM.indexByteArray arr0 0
     in Just (w,arr1)
  else Nothing
  where
  !lenArr = length arr0

-- internal function
replicate :: Int -> Word8 -> ByteArray
replicate len@(I# len#) (W8# w#) = runST $ do
  marr@(MutableByteArray marr#) <- PM.newByteArray len
  primitive_ (setByteArray# marr# 0# len# (word2Int# w#))
  PM.unsafeFreezeByteArray marr

length :: ByteArray -> Int
length = PM.sizeofByteArray

take :: Int -> ByteArray -> ByteArray
take !n !arr = if n < length arr
  then runST $ do
    marr <- PM.newByteArray n
    PM.copyByteArray marr 0 arr 0 n
    PM.unsafeFreezeByteArray marr
  else empty

empty :: ByteArray
empty = runST (PM.newByteArray 0 >>= PM.unsafeFreezeByteArray)

writeByteArrayWord8 :: MutableByteArray s -> Int -> Word8 -> ST s ()
writeByteArrayWord8 = PM.writeByteArray

-- | Does not check to see if the index is in bounds.
unsafeIndex :: ByteArray -> Int -> Word8
unsafeIndex = PM.indexByteArray
