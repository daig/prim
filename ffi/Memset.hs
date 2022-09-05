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


module Memset (
  setWord8Array#, setWord16Array#, setWord32Array#,
  setWord64Array#, setWordArray#,
  setInt8Array#, setInt16Array#, setInt32Array#,
  setInt64Array#, setIntArray#,
  setAddrArray#, setStablePtrArray#, setFloatArray#, setDoubleArray#,
  setWideCharArray#, setCharArray#,

  setWord8OffAddr#, setWord16OffAddr#, setWord32OffAddr#,
  setWord64OffAddr#, setWordOffAddr#,
  setInt8OffAddr#, setInt16OffAddr#, setInt32OffAddr#,
  setInt64OffAddr#, setIntOffAddr#,
  setAddrOffAddr#, setFloatOffAddr#, setDoubleOffAddr#,
  setWideCharOffAddr#, setCharOffAddr#,
  setStablePtrOffAddr#
) where

import GHC.Types (IO)
import GHC.Prim

foreign import ccall unsafe "primitive-memops.h hsprimitive_memset_Word8"
  setWord8Array# ∷ MutableByteArray# s → Int# {- ^ diff -} → Int# {- ^ size -}→ Word8# → IO ()
foreign import ccall unsafe "primitive-memops.h hsprimitive_memset_Word16"
  setWord16Array# ∷ MutableByteArray# s → Int# {- ^ diff -} → Int# {- ^ size -}→ Word16# → IO ()
foreign import ccall unsafe "primitive-memops.h hsprimitive_memset_Word32"
  setWord32Array# ∷ MutableByteArray# s → Int# {- ^ diff -} → Int# {- ^ size -}→ Word32# → IO ()
foreign import ccall unsafe "primitive-memops.h hsprimitive_memset_Word64"
  setWord64Array# ∷ MutableByteArray# s → Int# {- ^ diff -} → Int# {- ^ size -}→ Word64# → IO ()
foreign import ccall unsafe "primitive-memops.h hsprimitive_memset_Word"
  setWordArray# ∷ MutableByteArray# s → Int# {- ^ diff -} → Int# {- ^ size -}→ Word# → IO ()
foreign import ccall unsafe "primitive-memops.h hsprimitive_memset_Word8"
  setInt8Array# ∷ MutableByteArray# s → Int# {- ^ diff -} → Int# {- ^ size -} → Int8# → IO ()
foreign import ccall unsafe "primitive-memops.h hsprimitive_memset_Word16"
  setInt16Array# ∷ MutableByteArray# s → Int# {- ^ diff -} → Int# {- ^ size -} → Int16# → IO ()
foreign import ccall unsafe "primitive-memops.h hsprimitive_memset_Word32"
  setInt32Array# ∷ MutableByteArray# s → Int# {- ^ diff -} → Int# {- ^ size -} → Int32# → IO ()
foreign import ccall unsafe "primitive-memops.h hsprimitive_memset_Word64"
  setInt64Array# ∷ MutableByteArray# s → Int# {- ^ diff -} → Int# {- ^ size -} → Int64# → IO ()
foreign import ccall unsafe "primitive-memops.h hsprimitive_memset_Word"
  setIntArray# ∷ MutableByteArray# s → Int# {- ^ diff -} → Int# {- ^ size -} → Int# → IO ()
foreign import ccall unsafe "primitive-memops.h hsprimitive_memset_Ptr"
  setAddrArray# ∷ MutableByteArray# s → Int# {- ^ diff -} → Int# {- ^ size -} → Addr# → IO ()
foreign import ccall unsafe "primitive-memops.h hsprimitive_memset_Ptr"
  setStablePtrArray# ∷ MutableByteArray# s → Int# {- ^ diff -} → Int# {- ^ size -} → StablePtr# a → IO ()
foreign import ccall unsafe "primitive-memops.h hsprimitive_memset_Float"
  setFloatArray# ∷ MutableByteArray# s → Int# {- ^ diff -} → Int# {- ^ size -} → Float# → IO ()
foreign import ccall unsafe "primitive-memops.h hsprimitive_memset_Double"
  setDoubleArray# ∷ MutableByteArray# s → Int# {- ^ diff -} → Int# {- ^ size -} → Double# → IO ()
foreign import ccall unsafe "primitive-memops.h hsprimitive_memset_Char"
  setWideCharArray# ∷ MutableByteArray# s → Int# {- ^ diff -} → Int# {- ^ size -} → Char# → IO ()
foreign import ccall unsafe "primitive-memops.h hsprimitive_memset_Word8"
  setCharArray# ∷ MutableByteArray# s → Int# {- ^ diff -} → Int# {- ^ size -} → Char# → IO ()

foreign import ccall unsafe "primitive-memops.h hsprimitive_memset_Word8"
  setWord8OffAddr# ∷ Addr# → Int# {- ^ diff -} → Int# {- ^ size -}→ Word8# → IO ()
foreign import ccall unsafe "primitive-memops.h hsprimitive_memset_Word16"
  setWord16OffAddr# ∷ Addr# → Int# {- ^ diff -} → Int# {- ^ size -}→ Word16# → IO ()
foreign import ccall unsafe "primitive-memops.h hsprimitive_memset_Word32"
  setWord32OffAddr# ∷ Addr# → Int# {- ^ diff -} → Int# {- ^ size -}→ Word32# → IO ()
foreign import ccall unsafe "primitive-memops.h hsprimitive_memset_Word64"
  setWord64OffAddr# ∷ Addr# → Int# {- ^ diff -} → Int# {- ^ size -}→ Word64# → IO ()
foreign import ccall unsafe "primitive-memops.h hsprimitive_memset_Word"
  setWordOffAddr# ∷ Addr# → Int# {- ^ diff -} → Int# {- ^ size -}→ Word# → IO ()
foreign import ccall unsafe "primitive-memops.h hsprimitive_memset_Word8"
  setInt8OffAddr# ∷ Addr# → Int# {- ^ diff -} → Int# {- ^ size -} → Int8# → IO ()
foreign import ccall unsafe "primitive-memops.h hsprimitive_memset_Word16"
  setInt16OffAddr# ∷ Addr# → Int# {- ^ diff -} → Int# {- ^ size -} → Int16# → IO ()
foreign import ccall unsafe "primitive-memops.h hsprimitive_memset_Word32"
  setInt32OffAddr# ∷ Addr# → Int# {- ^ diff -} → Int# {- ^ size -} → Int32# → IO ()
foreign import ccall unsafe "primitive-memops.h hsprimitive_memset_Word64"
  setInt64OffAddr# ∷ Addr# → Int# {- ^ diff -} → Int# {- ^ size -} → Int64# → IO ()
foreign import ccall unsafe "primitive-memops.h hsprimitive_memset_Word"
  setIntOffAddr# ∷ Addr# → Int# {- ^ diff -} → Int# {- ^ size -} → Int# → IO ()
foreign import ccall unsafe "primitive-memops.h hsprimitive_memset_Ptr"
  setAddrOffAddr# ∷ Addr# → Int# {- ^ diff -} → Int# {- ^ size -} → Addr# → IO ()
foreign import ccall unsafe "primitive-memops.h hsprimitive_memset_Ptr"
  setStablePtrOffAddr# ∷ Addr# → Int# {- ^ diff -} → Int# {- ^ size -} → StablePtr# a → IO ()
foreign import ccall unsafe "primitive-memops.h hsprimitive_memset_Float"
  setFloatOffAddr# ∷ Addr# → Int# {- ^ diff -} → Int# {- ^ size -} → Float# → IO ()
foreign import ccall unsafe "primitive-memops.h hsprimitive_memset_Double"
  setDoubleOffAddr# ∷ Addr# → Int# {- ^ diff -} → Int# {- ^ size -} → Double# → IO ()
foreign import ccall unsafe "primitive-memops.h hsprimitive_memset_Char"
  setWideCharOffAddr# ∷ Addr# → Int# {- ^ diff -} → Int# {- ^ size -} → Char# → IO ()
foreign import ccall unsafe "primitive-memops.h hsprimitive_memset_Word8"
  setCharOffAddr# ∷ Addr# → Int# {- ^ diff -} → Int# {- ^ size -} → Char# → IO ()
