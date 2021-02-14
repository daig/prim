--------------------------------------------------------------------
-- | Description : Elements that fit in in primitive arrays
--------------------------------------------------------------------
{-# language CPP #-}
{-# language LinearTypes #-}
{-# language FunctionalDependencies,ScopedTypeVariables,ForeignFunctionInterface, UnliftedFFITypes #-}
module Prim where
import I
import IO
import {-# source #-} A
import P
import ST.Do

#include "MachDeps.h"

instance 𝔸 A# where
  type M A# = A##
  alloc i f = f (runRW# \ s → case newByteArray# i s of
    (# s' , ma #) → A### ma)
class 𝔸 a where
  type M a ∷ T_A
  alloc ∷ ∀ {r} (o ∷ T_ r). I → (M a ⊸ o) ⊸ o

--unio ∷ IO () → ST_# s
--unio (IO io) s = case unsafeCoerce# io s of (# s' , _ #) → s'
--

-- | Primitive unboxed, unlifted types that fit natively into raw memory
class (♭) (x ∷ T_ r) where
  size ∷ I {- ^ # elements -} → I {- ^ size in bytes -}
  align ∷ I → I
  indexA# ∷ A# → I → x
  indexB# ∷ A# → I → x
  indexP# ∷ P# → I → x
  readA# ∷  I → ST A## x
  readB# ∷ I → ST A## x
  readP# ∷ I → ST P# x
  writeA# ∷ I → x → ST_ A##
  writeB# ∷ I → x → ST_ A##
  writeP# ∷ I → x → ST_ P#
  setB# ∷ I {- ^ offset -} → I {- ^ elements -} →  x → ST_ A## 
  setP# ∷ I {- ^ offset -} → I {- ^ elements -} → x → ST_ P#

instance (♭) I where
  size = (SIZEOF_HSINT# ×)
  align i = case i % ALIGNMENT_HSINT# of {0# → i; off → i + (ALIGNMENT_HSINT# - off)}
  indexA# = coerce indexIntArray#
  indexB# = coerce indexWord8ArrayAsInt#
  indexP# = coerce indexIntOffAddr#
  readA# i (A### ma) = run do readIntArray# ma i
  --readP# = coerce RP#
  --readB# = coerce RB#
  --writeA# = coerce WA#
  --writeP# = coerce WP#
  --writeB# = coerce WB#
  --setB# a i j x = unio (SB a i j x) 
  --setP# a i j x = unio (SP a i j x)


{-


undefined# ∷ ∀ a. a
undefined# = let x = x in x

#define INST_PRIM(T,S,A,IA,RA,WA,IP,RP,WP,IB,RB,WB,SB,SP) \
instance (♭) T where \
  size = (S# ×); \
  align i = case i % A# of {0# → i ;off → i + (A# - off)}; \
  indexA# = coerce IA#; \
  readA# = coerce RA#; \
  writeA# = coerce WA#; \
  indexP# = coerce IP#; \
  readP# = coerce RP#; \
  writeP# = coerce WP#; \
  indexB# = coerce IB#; \
  readB# = coerce RB#; \
  writeB# = coerce WB#; \
  setB# a i j x = unio (SB a i j x) ; \
  setP# a i j x = unio (SP a i j x)

foreign import ccall unsafe "primitive-memops.h hsprimitive_memset_Word8"
  setWord8OffP# :: P# -> I {- ^ diff -} -> I {- ^ size -} -> U -> IO ()
foreign import ccall unsafe "primitive-memops.h hsprimitive_memset_Word8"
  setWord8Array# :: MutableByteArray# s -> I {- ^ diff -} -> I {- ^ size -} -> U -> IO ()

INST_PRIM(I, SIZEOF_HSINT, ALIGNMENT_HSINT, indexIntArray, readIntArray, writeIntArray, indexIntOffAddr, readIntOffAddr, writeIntOffAddr, indexWord8ArrayAsInt, readWord8ArrayAsInt, writeWord8ArrayAsInt, undefined#, undefined#)
INST_PRIM(I8, SIZEOF_INT8, ALIGNMENT_INT8, indexInt8Array, readInt8Array, writeInt8Array, indexInt8OffAddr, readInt8OffAddr, writeInt8OffAddr, indexInt8Array, readInt8Array, writeInt8Array, undefined#, undefined#)
INST_PRIM(I16, SIZEOF_INT16, ALIGNMENT_INT16, indexInt16Array, readInt16Array, writeInt16Array, indexInt16OffAddr, readInt16OffAddr, writeInt16OffAddr, indexWord8ArrayAsInt16, readWord8ArrayAsInt16, writeWord8ArrayAsInt16, undefined#, undefined#)
INST_PRIM(I32, SIZEOF_INT32, ALIGNMENT_INT32, indexInt32Array, readInt32Array, writeInt32Array, indexInt32OffAddr, readInt32OffAddr, writeInt32OffAddr, indexWord8ArrayAsInt32, readWord8ArrayAsInt32, writeWord8ArrayAsInt32, undefined#, undefined#)
INST_PRIM(I64, SIZEOF_INT64, ALIGNMENT_INT64, indexInt64Array, readInt64Array, writeInt64Array, indexInt64OffAddr, readInt64OffAddr, writeInt64OffAddr, indexWord8ArrayAsInt64, readWord8ArrayAsInt64, writeWord8ArrayAsInt64, undefined#, undefined#)
INST_PRIM(U, SIZEOF_HSWORD, ALIGNMENT_HSWORD, indexWordArray, readWordArray, writeWordArray, indexWordOffAddr, readWordOffAddr, writeWordOffAddr, indexWord8ArrayAsWord, readWord8ArrayAsWord, writeWord8ArrayAsWord, setWord8Array#, setWord8OffP#)
INST_PRIM(U8, SIZEOF_WORD8, ALIGNMENT_WORD8, indexWord8Array, readWord8Array, writeWord8Array, indexWord8OffAddr, readWord8OffAddr, writeWord8OffAddr, indexWord8Array, readWord8Array, writeWord8Array, undefined#, undefined#)
INST_PRIM(U16, SIZEOF_WORD16, ALIGNMENT_WORD16, indexWord16Array, readWord16Array, writeWord16Array, indexWord16OffAddr, readWord16OffAddr, writeWord16OffAddr, indexWord8ArrayAsWord16, readWord8ArrayAsWord16, writeWord8ArrayAsWord16, undefined#, undefined#)
INST_PRIM(U32, SIZEOF_WORD32, ALIGNMENT_WORD32, indexWord32Array, readWord32Array, writeWord32Array, indexWord32OffAddr, readWord32OffAddr, writeWord32OffAddr, indexWord8ArrayAsWord32, readWord8ArrayAsWord32, writeWord8ArrayAsWord32, undefined#, undefined#)
INST_PRIM(U64, SIZEOF_WORD64, ALIGNMENT_WORD64, indexWord64Array, readWord64Array, writeWord64Array, indexWord64OffAddr, readWord64OffAddr, writeWord64OffAddr, indexWord8ArrayAsWord64, readWord8ArrayAsWord64, writeWord8ArrayAsWord64, undefined#, undefined#)
INST_PRIM(Char, SIZEOF_HSCHAR, ALIGNMENT_HSCHAR, indexWideCharArray, readWideCharArray, writeWideCharArray, indexWideCharOffAddr, readWideCharOffAddr, writeWideCharOffAddr, indexWord8ArrayAsWideChar, readWord8ArrayAsWideChar, writeWord8ArrayAsWideChar, undefined#, undefined#)
INST_PRIM(Char8, SIZEOF_HSCHAR, ALIGNMENT_HSCHAR, indexCharArray, readCharArray, writeCharArray, indexCharOffAddr, readCharOffAddr, writeCharOffAddr, indexWord8ArrayAsChar, readWord8ArrayAsChar, writeWord8ArrayAsChar, undefined#, undefined#)
INST_PRIM(P#, SIZEOF_HSPTR, ALIGNMENT_HSPTR, indexAddrArray, readAddrArray, writeAddrArray, indexAddrOffAddr, readAddrOffAddr, writeAddrOffAddr, indexWord8ArrayAsAddr, readWord8ArrayAsAddr, writeWord8ArrayAsAddr, undefined#, undefined#)
instance (♭) (Stable.P a) where
  size = (SIZEOF_HSSTABLEPTR# ×)
  align i = case i % ALIGNMENT_HSSTABLEPTR# of {0# → i ;off → i + (ALIGNMENT_HSSTABLEPTR# - off)}
  indexA# = indexStablePtrArray#
  readA# = readStablePtrArray#
  writeA# = writeStablePtrArray#
  indexP# = indexStablePtrOffAddr#
  readP# = readStablePtrOffAddr# 
  writeP# = writeStablePtrOffAddr#
  indexB# = indexWord8ArrayAsStablePtr#
  readB# = readWord8ArrayAsStablePtr# 
  writeB# = writeWord8ArrayAsStablePtr#

-}
