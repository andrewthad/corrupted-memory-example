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
  , empty
  , singleton
  , uncons
  , append
  , pack
  , unpack
  , null
  , drop
  , dropEnd
  , replicate
  , length
  , take
    -- * Folds
  , foldr
    -- * Unsliced Byte Arrays
  , fromByteArray
    -- * Unsafe
  , unsafeTake
  , unsafeDrop
  ) where

import Prelude hiding (take,length,replicate,drop,null,concat,foldr)

import Packed.Bytes.Small (ByteArray(..))
import Data.Monoid (Monoid(..))
import Data.Semigroup (Semigroup)
import Data.Word (Word8)
import Control.Monad.ST (runST)
import qualified Packed.Bytes.Window as BAW
import qualified Packed.Bytes.Small as BA
import qualified Data.Primitive as PM
import qualified Data.Semigroup as SG

data Bytes = Bytes
  {-# UNPACK #-} !ByteArray -- payload
  {-# UNPACK #-} !Int -- offset
  {-# UNPACK #-} !Int -- length

instance Show Bytes where
  show x = "pack " ++ show (unpack x)

instance Semigroup Bytes where
  (<>) = append

instance Monoid Bytes where
  mempty = empty
  mappend = (SG.<>)

uncons :: Bytes -> Maybe (Word8, Bytes)
uncons (Bytes arr off len) = if len > 0
  then Just (PM.indexByteArray arr off, Bytes arr (off + 1) (len - 1))
  else Nothing

append :: Bytes -> Bytes -> Bytes
append (Bytes arr1 off1 len1) (Bytes arr2 off2 len2) = runST $ do
  marr <- PM.newByteArray (len1 + len2)
  PM.copyByteArray marr 0 arr1 off1 len1
  PM.copyByteArray marr len1 arr2 off2 len2
  arr <- PM.unsafeFreezeByteArray marr
  return (Bytes arr 0 (len1 + len2))

null :: Bytes -> Bool
null (Bytes _ _ len) = len < 1

pack :: [Word8] -> Bytes
pack bs = let arr = BA.pack bs in Bytes arr 0 (BA.length arr)

unpack :: Bytes -> [Word8]
unpack (Bytes arr off len) = go off
  where
  go :: Int -> [Word8]
  go !ix = if ix < len + off
    then BA.unsafeIndex arr ix : go (ix + 1)
    else []

drop :: Int -> Bytes -> Bytes
drop n (Bytes arr off len) = if len > n
  then Bytes arr (off + n) (len - n)
  else empty

dropEnd :: Int -> Bytes -> Bytes
dropEnd n (Bytes arr off len) = if len > n
  then Bytes arr off (len - n)
  else empty

replicate :: Int -> Word8 -> Bytes
replicate len w = fromByteArray (BA.replicate len w)

fromByteArray :: ByteArray -> Bytes
fromByteArray ba = Bytes ba 0 (BA.length ba)

length :: Bytes -> Int
length (Bytes _ _ len) = len

take :: Int -> Bytes -> Bytes
take !n b@(Bytes arr off len) = if n < len
  then Bytes arr off n
  else b

unsafeTake :: Int -> Bytes -> Bytes
unsafeTake !n (Bytes arr off _) = Bytes arr off n

unsafeDrop :: Int -> Bytes -> Bytes
unsafeDrop n (Bytes arr off len) = Bytes arr (off + n) (len - n)

empty :: Bytes
empty = Bytes BA.empty 0 0

singleton :: Word8 -> Bytes
singleton w = Bytes (BA.singleton w) 0 1

foldr :: (Word8 -> a -> a) -> a -> Bytes -> a
foldr f a (Bytes arr off len) = BAW.foldr off len f a arr

