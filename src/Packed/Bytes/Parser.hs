{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeInType #-}
{-# LANGUAGE UnboxedSums #-}
{-# LANGUAGE UnboxedTuples #-}

{-# OPTIONS_GHC
 -Weverything
 -fno-warn-unsafe
 -fno-warn-implicit-prelude
 -fno-warn-missing-import-lists
 -fno-warn-noncanonical-monoid-instances
 -O2
#-}

module Packed.Bytes.Parser
  ( Parser(..)
  , Result(..)
  , Leftovers(..)
  , parseStreamST
  , takeBytesUntilEndOfLineConsume
  , failure
  ) where

import Data.Primitive (ByteArray(..))
import GHC.Int (Int(I#))
import GHC.ST (ST(..),runST)
import GHC.Types (TYPE,RuntimeRep(..))
import GHC.Word (Word8(W8#))
import Packed.Bytes (Bytes(..))
import Packed.Bytes.Stream.ST (ByteStream(..))
import Prelude hiding (any,replicate)

import qualified Data.Primitive as PM
import qualified Packed.Bytes as B
import qualified Packed.Bytes.Stream.ST as Stream
import qualified Packed.Bytes.Window as BAW

import GHC.Exts (Int#,ByteArray#,State#,(+#),(-#),(<#),indexWord8Array#)

type Bytes# = (# ByteArray#, Int#, Int# #)
type Maybe# (a :: TYPE r) = (# (# #) | a #)
type Leftovers# s = (# Bytes# , ByteStream s #)
type Either# a (b :: TYPE r) = (# a | b #)
type Result# e c s (r :: RuntimeRep) (a :: TYPE r) =
  (# Maybe# (Leftovers# s), Either# (Maybe e) a, c #)
type BytesRep = 'TupleRep '[ 'UnliftedRep, 'IntRep, 'IntRep ]

data Result e c s a = Result
  { resultLeftovers :: !(Maybe (Leftovers s))
  , resultValue :: !(Either (Maybe e) a)
  , resultContext :: c
  }


data Leftovers s = Leftovers
  { leftoversChunk :: {-# UNPACK #-} !Bytes
    -- ^ The last chunk pulled from the stream
  , leftoversStream :: ByteStream s
    -- ^ The remaining stream
  }

data PureResult a = PureResult
  { pureResultLeftovers :: {-# UNPACK #-} !Bytes
  , pureResultValue :: !(Maybe a)
  } deriving (Show)

emptyByteArray :: ByteArray
emptyByteArray = runST (PM.newByteArray 0 >>= PM.unsafeFreezeByteArray)

parseStreamST :: ByteStream s -> c -> Parser e c a -> ST s (Result e c s a)
parseStreamST stream c0 (Parser (ParserLevity f)) = ST $ \s0 ->
  case f c0 (# | (# (# unboxByteArray emptyByteArray, 0#, 0# #), stream #) #) s0 of
    (# s1, r #) -> (# s1, boxResult r #)

boxResult :: Result# e c s 'LiftedRep a -> Result e c s a
boxResult (# leftovers, val, c #) = case val of
  (# err | #) -> Result (boxLeftovers leftovers) (Left err) c
  (# | a #) -> Result (boxLeftovers leftovers) (Right a) c

boxLeftovers :: Maybe# (Leftovers# s) -> Maybe (Leftovers s)
boxLeftovers (# (# #) | #) = Nothing
boxLeftovers (# | (# theBytes, stream #) #) = Just (Leftovers (boxBytes theBytes) stream)

newtype Parser e c a = Parser (ParserLevity e c 'LiftedRep a)

newtype ParserLevity e c (r :: RuntimeRep) (a :: TYPE r) = ParserLevity
  { getParserLevity :: forall s.
       c
    -> Maybe# (Leftovers# s)
    -> State# s
    -> (# State# s, Result# e c s r a #)
  }

nextNonEmpty :: ByteStream s -> State# s -> (# State# s, Maybe# (Leftovers# s) #)
nextNonEmpty (ByteStream f) s0 = case f s0 of
  (# s1, r #) -> case r of
    (# (# #) | #) -> (# s1, (# (# #) | #) #)
    (# | (# theBytes@(# _,_,len #), stream #) #) -> case len of
      0# -> nextNonEmpty stream s1
      _ -> (# s1, (# | (# theBytes, stream #) #) #)

-- This assumes that the Bytes is longer than the index. It also does
-- not eliminate zero-length references to byte arrays.
unsafeDrop# :: Int# -> Bytes# -> Bytes#
unsafeDrop# i (# arr, off, len #) = (# arr, off +# i, len -# i #)

unboxByteArray :: ByteArray -> ByteArray#
unboxByteArray (ByteArray arr) = arr

boxBytes :: Bytes# -> Bytes
boxBytes (# a, b, c #) = Bytes (ByteArray a) (I# b) (I# c)

failure :: Parser e c a
failure = Parser (ParserLevity (\c m s -> (# s, (# m, (# Nothing | #), c #) #)))

{-# INLINE takeBytesUntilEndOfLineConsume #-}
takeBytesUntilEndOfLineConsume :: Parser e c Bytes
takeBytesUntilEndOfLineConsume = Parser (boxBytesParser takeBytesUntilEndOfLineConsumeUnboxed)

{-# NOINLINE takeBytesUntilEndOfLineConsumeUnboxed #-}
takeBytesUntilEndOfLineConsumeUnboxed :: ParserLevity e c BytesRep Bytes#
takeBytesUntilEndOfLineConsumeUnboxed = ParserLevity (go (# (# #) | #)) where
  go :: Maybe# Bytes# -> c -> Maybe# (Leftovers# s) -> State# s -> (# State# s, Result# e c s BytesRep Bytes# #)
  go !_ c0 (# (# #) | #) s0 = (# s0, (# (# (# #) | #), (# Nothing | #), c0 #) #)
  go !mbytes c0 (# | (# bytes0@(# arr0, off0, len0 #), !stream0@(ByteStream streamFunc) #) #) s0 = case BAW.findAnyByte2 (I# off0) (I# len0) 10 13 (ByteArray arr0) of
    Nothing -> case streamFunc s0 of
      (# s1, r #) -> go (# | appendMaybeBytes mbytes bytes0 #) c0 r s1
    Just (I# ix, W8# theByte) -> case theByte of
      10## -> (# s0, (# (# | (# unsafeDrop# ((ix -# off0) +# 1# ) bytes0, stream0 #) #), (# | appendMaybeBytes mbytes (# arr0, off0, ix -# off0 #) #), c0 #) #)
      -- second case means it was 13
      _ -> case ix <# (off0 +# len0 -# 1#) of
        1# -> case indexWord8Array# arr0 (ix +# 1# ) of
          10## -> (# s0, (# (# | (# unsafeDrop# ((ix -# off0) +# 2# ) bytes0, stream0 #) #), (# | appendMaybeBytes mbytes (# arr0, off0, ix -# off0 #) #), c0 #) #)
          _ -> (# s0, (# (# | (# unsafeDrop# (ix -# off0) bytes0, stream0 #) #), (# Nothing | #), c0 #) #)
        _ -> case nextNonEmpty stream0 s0 of
          (# s1, m #) -> case m of
            (# (# #) | #) -> (# s1, (# (# | (# unboxBytes (B.singleton 13), Stream.empty #) #), (# Nothing | #), c0 #) #)
            (# | (# bytes1@(# arr1, _, _ #), stream1 #) #) -> case indexWord8Array# arr1 0# of
              10## -> (# s1, (# (# | (# unsafeDrop# 1# bytes1, stream1 #) #), (# | appendMaybeBytes mbytes (# arr0, off0, ix -# off0 #) #), c0 #) #)
              _ -> (# s1, (# (# | (# unboxBytes (B.cons 13 (boxBytes bytes1)), stream1 #) #), (# Nothing | #), c0 #) #)

unboxBytes :: Bytes -> Bytes#
unboxBytes (Bytes (ByteArray a) (I# b) (I# c)) = (# a,b,c #)

appendMaybeBytes :: Maybe# Bytes# -> Bytes# -> Bytes#
appendMaybeBytes (# (# #) | #) theBytes = theBytes
appendMaybeBytes (# | b #) theBytes = unboxBytes (B.append (boxBytes b) (boxBytes theBytes))

boxBytesParser ::
     ParserLevity e c BytesRep Bytes#
  -> ParserLevity e c 'LiftedRep Bytes
boxBytesParser p = ParserLevity $ \c0 leftovers0 s0 ->
  case getParserLevity p c0 leftovers0 s0 of
    (# s1, (# leftovers1, val, c1 #) #) -> case val of
      (# err | #) -> (# s1, (# leftovers1, (# err | #), c1 #) #)
      (# | theBytes #) -> (# s1, (# leftovers1, (# | boxBytes theBytes #), c1 #) #)

