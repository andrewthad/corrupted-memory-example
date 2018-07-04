{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}

{-# OPTIONS_GHC
 -Weverything
 -fno-warn-unsafe
 -fno-warn-implicit-prelude
 -fno-warn-missing-import-lists
 -O2
#-}

module Packed.Bytes
  ( Bytes(..)
  , pack
  , unpack
  , length
  , singleton
  , cons
  , append
    -- * Folds
  , foldr
    -- * Unsliced Byte Arrays
  , fromByteArray
  ) where

import Prelude hiding (take,length,replicate,drop,null,concat,foldr)

import Data.Primitive (ByteArray(..))
import Data.Word (Word8)
import Control.Monad.ST (runST)
import qualified Data.Primitive as PM
import qualified GHC.OldList as L

data Bytes = Bytes
  {-# UNPACK #-} !ByteArray -- payload
  {-# UNPACK #-} !Int -- offset
  {-# UNPACK #-} !Int -- length

instance Show Bytes where
  show x = "pack " ++ show (unpack x)

pack :: [Word8] -> Bytes
pack bs = let arr = packByteArray bs in Bytes arr 0 (lengthByteArray arr)

unpack :: Bytes -> [Word8]
unpack (Bytes arr off len) = go off
  where
  go :: Int -> [Word8]
  go !ix = if ix < len + off
    then PM.indexByteArray arr ix : go (ix + 1)
    else []

fromByteArray :: ByteArray -> Bytes
fromByteArray ba = Bytes ba 0 (lengthByteArray ba)

length :: Bytes -> Int
length (Bytes _ _ len) = len

foldr :: (Word8 -> a -> a) -> a -> Bytes -> a
foldr f a0 (Bytes arr off0 len) = go off0 where
  !end = off0 + len
  go !ix = if ix < end
    then f (PM.indexByteArray arr ix) (go (ix + 1))
    else a0

packByteArray :: [Word8] -> ByteArray
packByteArray ws0 = runST $ do
  marr <- PM.newByteArray (L.length ws0)
  let go [] !_ = return ()
      go (w : ws) !ix = PM.writeByteArray marr ix w >> go ws (ix + 1)
  go ws0 0
  PM.unsafeFreezeByteArray marr

unpackByteArray :: ByteArray -> [Word8]
unpackByteArray arr = go 0 where
  go :: Int -> [Word8]
  go !ix = if ix < lengthByteArray arr
    then PM.indexByteArray arr ix : go (ix + 1)
    else []

lengthByteArray :: ByteArray -> Int
lengthByteArray = PM.sizeofByteArray

cons :: Word8 -> Bytes -> Bytes
cons w (Bytes arr off len) = runST $ do
  marr <- PM.newByteArray (len + 1)
  PM.writeByteArray marr 0 w
  PM.copyByteArray marr 1 arr off len
  newArr <- PM.unsafeFreezeByteArray marr
  return (Bytes newArr 0 (len + 1))

singleton :: Word8 -> Bytes
singleton w = Bytes (singletonByteArray w) 0 1

singletonByteArray :: Word8 -> ByteArray
singletonByteArray w = runST $ do
  marr <- PM.newByteArray 1
  PM.writeByteArray marr 0 w
  PM.unsafeFreezeByteArray marr

append :: Bytes -> Bytes -> Bytes
append (Bytes arr1 off1 len1) (Bytes arr2 off2 len2) = runST $ do
  marr <- PM.newByteArray (len1 + len2)
  PM.copyByteArray marr 0 arr1 off1 len1
  PM.copyByteArray marr len1 arr2 off2 len2
  arr <- PM.unsafeFreezeByteArray marr
  return (Bytes arr 0 (len1 + len2))

