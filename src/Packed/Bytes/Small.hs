{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-# OPTIONS_GHC
 -Weverything
 -fno-warn-unsafe
 -fno-warn-implicit-prelude
 -fno-warn-missing-import-lists
 -fno-warn-missing-local-signatures
 -O2
#-}

module Packed.Bytes.Small
  ( ByteArray(..)
  , empty
  , pack
  , unpack
  , length
  ) where

import Prelude hiding (replicate,length,take,reverse)

import Control.Monad.ST (runST)
import Data.Primitive.ByteArray (ByteArray(..))
import Data.Word (Word8)
import qualified GHC.OldList as L
import qualified Data.Primitive as PM

pack :: [Word8] -> ByteArray
pack ws0 = runST $ do
  marr <- PM.newByteArray (L.length ws0)
  let go [] !_ = return ()
      go (w : ws) !ix = PM.writeByteArray marr ix w >> go ws (ix + 1)
  go ws0 0
  PM.unsafeFreezeByteArray marr

unpack :: ByteArray -> [Word8]
unpack arr = go 0 where
  go :: Int -> [Word8]
  go !ix = if ix < length arr
    then PM.indexByteArray arr ix : go (ix + 1)
    else []

length :: ByteArray -> Int
length = PM.sizeofByteArray

empty :: ByteArray
empty = runST (PM.newByteArray 0 >>= PM.unsafeFreezeByteArray)
