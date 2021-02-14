--------------------------------------------------------------------
-- | Description : Elements that fit in in primitive arrays
--------------------------------------------------------------------
{-# language TypeFamilyDependencies, FlexibleInstances,InstanceSigs,MultiParamTypeClasses #-}
{-# language CPP #-}
{-# language QuantifiedConstraints #-}
{-# language ForeignFunctionInterface, UnliftedFFITypes #-}
module Prim.A.Prim.Elts where
import P hiding (Prim)
import Prim.Char
import Prim.I32 (I32(..))
import Prim.I16 (I16(..))
import Prim.I8 (I8(..))
import Prim.I64 (I64(..))
import Prim.I
import qualified P.Stable as Stable
import {-# source #-} qualified Prim.A.Prim as Prim
import {-# source #-} qualified Prim.A.Prim.Pinned as Pinned
import {-# source #-} qualified Prim.A.Array as Ref
import {-# source #-} qualified Prim.A.Boxed.Big as Big
import {-# source #-} qualified Prim.A.Boxed as Boxed
import qualified P.STM as STM
import Stock.Int
#include "MachDeps.h"
#include "HsBaseConfig.h"
import GHC.Types (IO(..))
import {-# source #-} Prim.A
import Prim.A.M

-- | Primitive unboxed, unlifted types that fit natively into raw memory
class (♭) (x ∷ T_ r) where
  size ∷ I {- ^ # elements -} → I {- ^ size in bytes -}
  align ∷ I → I
  indexA# ∷ Bytes → I → x
  readA# ∷ MBytes s → I → ST s x
  writeA# ∷ M Bytes s → I → x → ST_ s
  indexP# ∷ P# → I → x
  readP# ∷ P# → I → ST s x
  writeP# ∷ P# → I → x → ST_ s
  indexB ∷ Bytes → I → x
  readB ∷ MBytes s → I → ST s x
  writeB ∷ MBytes s → I → x → ST_ s
  setB ∷ MBytes s → I {- ^ offset -} → I {- ^ elements -} →  x → ST_ s
  setP# ∷ P# → I {- ^ offset -} → I {- ^ elements -} → x → ST_ s

unio ∷ IO () → ST_ s
unio (IO io) s = case unsafeCoerce# io s of (# s' , _ #) → s'

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
  indexB = coerce IB; \
  readB = coerce RB; \
  writeB = coerce WB; \
  setB a i j x = unio (SB a i j x) ; \
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
