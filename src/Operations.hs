{-# LANGUAGE MagicHash, UnliftedFFITypes #-}

-- |
-- Module      : Data.Primitive.Internal.Operations
-- Copyright   : (c) Roman Leshchinskiy 2011-2012
-- License     : BSD-style
--
-- Maintainer  : Roman Leshchinskiy <rl@cse.unsw.edu.au>
-- Portability : non-portable
--
-- Internal operations
--


module Operations (
  setWord8Array#, setWord16Array#, setWord32Array#,
  setWord64Array#, setWordArray#,
  setInt8Array#, setInt16Array#, setInt32Array#,
  setInt64Array#, setIntArray#,
  setAddrArray#, setStablePtrArray#, setFloatArray#, setDoubleArray#,
  setWideCharArray#,

  setWord8OffP#, setWord16OffP#, setWord32OffP#,
  setWord64OffP#, setWordOffP#,
  setInt8OffP#, setInt16OffP#, setInt32OffP#,
  setInt64OffP#, setIntOffP#,
  setAddrOffP#, setFloatOffP#, setDoubleOffP#, setWideCharOffP#,
  setStablePtrOffP#
) where

import GHC.Types (IO)

foreign import ccall unsafe "primitive-memops.h hsprimitive_memset_Word8"
  setWord8Array# :: MutableByteArray# s -> I {- ^ diff -} -> I {- ^ size -} -> U -> IO ()
foreign import ccall unsafe "primitive-memops.h hsprimitive_memset_Word16"
  setWord16Array# :: MutableByteArray# s -> I {- ^ diff -} -> I {- ^ size -} -> U -> IO ()
foreign import ccall unsafe "primitive-memops.h hsprimitive_memset_Word32"
  setWord32Array# :: MutableByteArray# s -> I {- ^ diff -} -> I {- ^ size -} -> U -> IO ()
foreign import ccall unsafe "primitive-memops.h hsprimitive_memset_Word64"
  setWord64Array# :: MutableByteArray# s -> I {- ^ diff -} -> I {- ^ size -} -> U -> IO ()
foreign import ccall unsafe "primitive-memops.h hsprimitive_memset_Word"
  setWordArray# :: MutableByteArray# s -> I {- ^ diff -} -> I {- ^ size -} -> U -> IO ()
foreign import ccall unsafe "primitive-memops.h hsprimitive_memset_Word8"
  setInt8Array# :: MutableByteArray# s -> I {- ^ diff -} -> I {- ^ size -} -> I -> IO ()
foreign import ccall unsafe "primitive-memops.h hsprimitive_memset_Word16"
  setInt16Array# :: MutableByteArray# s -> I {- ^ diff -} -> I {- ^ size -} -> I -> IO ()
foreign import ccall unsafe "primitive-memops.h hsprimitive_memset_Word32"
  setInt32Array# :: MutableByteArray# s -> I {- ^ diff -} -> I {- ^ size -} -> I -> IO ()
foreign import ccall unsafe "primitive-memops.h hsprimitive_memset_Word64"
  setInt64Array# :: MutableByteArray# s -> I {- ^ diff -} -> I {- ^ size -} -> I -> IO ()
foreign import ccall unsafe "primitive-memops.h hsprimitive_memset_Word"
  setIntArray# :: MutableByteArray# s -> I {- ^ diff -} -> I {- ^ size -} -> I -> IO ()
foreign import ccall unsafe "primitive-memops.h hsprimitive_memset_Ptr"
  setAddrArray# :: MutableByteArray# s -> I {- ^ diff -} -> I {- ^ size -} -> P# -> IO ()
foreign import ccall unsafe "primitive-memops.h hsprimitive_memset_Ptr"
  setStablePtrArray# :: MutableByteArray# s -> I {- ^ diff -} -> I {- ^ size -} -> StablePtr# a -> IO ()
foreign import ccall unsafe "primitive-memops.h hsprimitive_memset_Float"
  setFloatArray# :: MutableByteArray# s -> I {- ^ diff -} -> I {- ^ size -} -> Float# -> IO ()
foreign import ccall unsafe "primitive-memops.h hsprimitive_memset_Double"
  setDoubleArray# :: MutableByteArray# s -> I {- ^ diff -} -> I {- ^ size -} -> Double# -> IO ()
foreign import ccall unsafe "primitive-memops.h hsprimitive_memset_Char"
  setWideCharArray# :: MutableByteArray# s -> I {- ^ diff -} -> I {- ^ size -} -> Char# -> IO ()

foreign import ccall unsafe "primitive-memops.h hsprimitive_memset_Word8"
  setWord8OffP# :: P# -> I {- ^ diff -} -> I {- ^ size -} -> U -> IO ()
foreign import ccall unsafe "primitive-memops.h hsprimitive_memset_Word16"
  setWord16OffP# :: P# -> I {- ^ diff -} -> I {- ^ size -} -> U -> IO ()
foreign import ccall unsafe "primitive-memops.h hsprimitive_memset_Word32"
  setWord32OffP# :: P# -> I {- ^ diff -} -> I {- ^ size -} -> U -> IO ()
foreign import ccall unsafe "primitive-memops.h hsprimitive_memset_Word64"
  setWord64OffP# :: P# -> I {- ^ diff -} -> I {- ^ size -} -> U -> IO ()
foreign import ccall unsafe "primitive-memops.h hsprimitive_memset_Word"
  setWordOffP# :: P# -> I {- ^ diff -} -> I {- ^ size -} -> U -> IO ()
foreign import ccall unsafe "primitive-memops.h hsprimitive_memset_Word8"
  setInt8OffP# :: P# -> I {- ^ diff -} -> I {- ^ size -} -> I -> IO ()
foreign import ccall unsafe "primitive-memops.h hsprimitive_memset_Word16"
  setInt16OffP# :: P# -> I {- ^ diff -} -> I {- ^ size -} -> I -> IO ()
foreign import ccall unsafe "primitive-memops.h hsprimitive_memset_Word32"
  setInt32OffP# :: P# -> I {- ^ diff -} -> I {- ^ size -} -> I -> IO ()
foreign import ccall unsafe "primitive-memops.h hsprimitive_memset_Word64"
  setInt64OffP# :: P# -> I {- ^ diff -} -> I {- ^ size -} -> I -> IO ()
foreign import ccall unsafe "primitive-memops.h hsprimitive_memset_Word"
  setIntOffP# :: P# -> I {- ^ diff -} -> I {- ^ size -} -> I -> IO ()
foreign import ccall unsafe "primitive-memops.h hsprimitive_memset_Ptr"
  setAddrOffP# :: P# -> I {- ^ diff -} -> I {- ^ size -} -> P# -> IO ()
foreign import ccall unsafe "primitive-memops.h hsprimitive_memset_Ptr"
  setStablePtrOffP# :: P# -> I {- ^ diff -} -> I {- ^ size -} -> StablePtr# a -> IO ()
foreign import ccall unsafe "primitive-memops.h hsprimitive_memset_Float"
  setFloatOffP# :: P# -> I {- ^ diff -} -> I {- ^ size -} -> Float# -> IO ()
foreign import ccall unsafe "primitive-memops.h hsprimitive_memset_Double"
  setDoubleOffP# :: P# -> I {- ^ diff -} -> I {- ^ size -} -> Double# -> IO ()
foreign import ccall unsafe "primitive-memops.h hsprimitive_memset_Char"
  setWideCharOffP# :: P# -> I {- ^ diff -} -> I {- ^ size -} -> Char# -> IO ()

