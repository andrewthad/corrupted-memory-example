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
    -- * Folds
  , foldr
    -- * Unsliced Byte Arrays
  , fromByteArray
  ) where

import Prelude hiding (take,length,replicate,drop,null,concat,foldr)

import Packed.Bytes.Small (ByteArray(..))
import Data.Word (Word8)
import qualified Packed.Bytes.Window as BAW
import qualified Packed.Bytes.Small as BA
import qualified Data.Primitive as PM

data Bytes = Bytes
  {-# UNPACK #-} !ByteArray -- payload
  {-# UNPACK #-} !Int -- offset
  {-# UNPACK #-} !Int -- length

instance Show Bytes where
  show x = "pack " ++ show (unpack x)

pack :: [Word8] -> Bytes
pack bs = let arr = BA.pack bs in Bytes arr 0 (BA.length arr)

unpack :: Bytes -> [Word8]
unpack (Bytes arr off len) = go off
  where
  go :: Int -> [Word8]
  go !ix = if ix < len + off
    then PM.indexByteArray arr ix : go (ix + 1)
    else []

fromByteArray :: ByteArray -> Bytes
fromByteArray ba = Bytes ba 0 (BA.length ba)

length :: Bytes -> Int
length (Bytes _ _ len) = len

foldr :: (Word8 -> a -> a) -> a -> Bytes -> a
foldr f a (Bytes arr off len) = BAW.foldr off len f a arr

