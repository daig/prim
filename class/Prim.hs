{-# language CPP #-}
{-# language UnliftedFFITypes #-}
module Prim where
import Do
import {-# source #-} Num
import qualified GHC.Types as GHC
import Memset

#include "MachDeps.h"
#include "HsBaseConfig.h"

-- | Primitive unboxed, unlifted types that fit natively into raw memory
type (♭) :: forall {r :: RuntimeRep}. T r -> Constraint
class (♭) (x ∷ T r) where
  size ∷ I {- ^ # elements -} → I {- ^ size in bytes -}
  align ∷ I → I
  indexA# ∷ Bytes → I {- ^ index in # elements -} → x
  indexB# ∷ Bytes → I {- ^ index in bytes -} → x
  indexP# ∷ P# → I {- ^ offset in bytes -} → x
  readA# ∷ Bytes_M s -> I {- ^ index in # elements -} → ST s x
  readB# ∷ Bytes_M s -> I {- ^ index in # Bytes -} → ST s x
  readP# ∷ P# -> I {- ^ offset in # elements -} → ST s x
  writeA# ∷ Bytes_M s -> I {- ^ index in # elements -} → x → ST_ s
  writeB# ∷ Bytes_M s -> I → x → ST_ s
  writeP# ∷ M# s -> I → x → ST_ s
  setB# ∷ Bytes_M s -> I {- ^ offset -} → I {- ^ elements -} → x → ST_ s
  setP# ∷ M# s -> I {- ^ offset -} → I {- ^ elements -} → x → ST_ s

#define INST_PRIM(TY,SI,AL,IA,IB,IP,RA,RB,RP,WA,WB,WP,SB,SP) \
instance (♭) (TY) where {\
  size = (SI# ×); \
  align i = case i % AL# of {0# → i; off → i + (AL# - off)}; \
  indexA# x = coerce IA# x; \
  indexB# x = coerce IB# x; \
  indexP# p = coerce IP# p; \
  readA# = coerce RA# ; \
  readB# = coerce RB# ; \
  readP# = coerce RP# ; \
  writeA# = coerce WA# ; \
  writeB# = coerce WB# ; \
  writeP# = coerce WP# ; \
  setB# m i n x = unio (SB# (coerce m) i n (coerce x)) ; \
  setP# p i n x = unio (SP# (coerce p) i n (coerce x))}

INST_PRIM(I,SIZEOF_HSINT,ALIGNMENT_HSINT,indexIntArray,indexWord8ArrayAsInt,indexIntOffAddr,readIntArray,readWord8ArrayAsInt,readIntOffAddr,writeIntArray,writeWord8ArrayAsInt,writeIntOffAddr,setIntArray,setIntOffAddr)
INST_PRIM(I8,SIZEOF_INT8,ALIGNMENT_INT8,indexInt8Array,indexInt8Array,indexInt8OffAddr,readInt8Array,readInt8Array,readInt8OffAddr,writeInt8Array,writeInt8Array,writeInt8OffAddr,setInt8Array,setInt8OffAddr)
INST_PRIM(I16,SIZEOF_INT16,ALIGNMENT_INT16,indexInt16Array,indexInt16Array,indexInt16OffAddr,readInt16Array,readInt16Array,readInt16OffAddr,writeInt16Array,writeInt16Array,writeInt16OffAddr,setInt16Array,setInt16OffAddr)
INST_PRIM(I32,SIZEOF_INT32,ALIGNMENT_INT32,indexInt32Array,indexInt32Array,indexInt32OffAddr,readInt32Array,readInt32Array,readInt32OffAddr,writeInt32Array,writeInt32Array,writeInt32OffAddr,setInt32Array,setInt32OffAddr)
INST_PRIM(I64,SIZEOF_INT64,ALIGNMENT_INT64,indexInt64Array,indexInt64Array,indexInt64OffAddr,readInt64Array,readInt64Array,readInt64OffAddr,writeInt64Array,writeInt64Array,writeInt64OffAddr,setInt64Array,setInt64OffAddr)

INST_PRIM(U,SIZEOF_HSWORD,ALIGNMENT_HSWORD,indexWordArray,indexWord8ArrayAsWord,indexWordOffAddr,readWordArray,readWord8ArrayAsWord,readWordOffAddr,writeWordArray,writeWord8ArrayAsWord,writeWordOffAddr,setWordArray,setWordOffAddr)
INST_PRIM(U8,SIZEOF_WORD8,ALIGNMENT_WORD8,indexWord8Array,indexWord8Array,indexWord8OffAddr,readWord8Array,readWord8Array,readWord8OffAddr,writeWord8Array,writeWord8Array,writeWord8OffAddr,setWord8Array,setWord8OffAddr)
INST_PRIM(U16,SIZEOF_WORD16,ALIGNMENT_WORD16,indexWord16Array,indexWord16Array,indexWord16OffAddr,readWord16Array,readWord16Array,readWord16OffAddr,writeWord16Array,writeWord16Array,writeWord16OffAddr,setWord16Array,setWord16OffAddr)
INST_PRIM(U32,SIZEOF_WORD32,ALIGNMENT_WORD32,indexWord32Array,indexWord32Array,indexWord32OffAddr,readWord32Array,readWord32Array,readWord32OffAddr,writeWord32Array,writeWord32Array,writeWord32OffAddr,setWord32Array,setWord32OffAddr)
INST_PRIM(U64,SIZEOF_WORD64,ALIGNMENT_WORD64,indexWord64Array,indexWord64Array,indexWord64OffAddr,readWord64Array,readWord64Array,readWord64OffAddr,writeWord64Array,writeWord64Array,writeWord64OffAddr,setWord64Array,setWord64OffAddr)

INST_PRIM(Char,SIZEOF_HSCHAR,ALIGNMENT_HSCHAR,indexWideCharArray,indexWord8ArrayAsWideChar,indexWideCharOffAddr,readWideCharArray,readWord8ArrayAsWideChar,readWideCharOffAddr,writeWideCharArray,writeWord8ArrayAsWideChar,writeWideCharOffAddr,setWideCharArray,setWideCharOffAddr)
INST_PRIM(Char8,SIZEOF_HSCHAR,ALIGNMENT_HSCHAR,indexCharArray,indexWord8ArrayAsChar,indexCharOffAddr,readCharArray,readWord8ArrayAsChar,readCharOffAddr,writeCharArray,writeWord8ArrayAsChar,writeCharOffAddr,setCharArray,setCharOffAddr)

INST_PRIM(P#,SIZEOF_HSPTR,ALIGNMENT_HSPTR,indexAddrArray,indexWord8ArrayAsAddr,indexAddrOffAddr,readAddrArray,readWord8ArrayAsAddr,readAddrOffAddr,writeAddrArray,writeWord8ArrayAsAddr,writeAddrOffAddr,setAddrArray,setAddrOffAddr)

-- INST_PRIM((P_Stable s),SIZEOF_HSPTR,ALIGNMENT_HSPTR,indexAddrArray,indexWord8ArrayAsAddr,indexAddrOffAddr,readAddrArray,readWord8ArrayAsAddr,readAddrOffAddr,writeAddrArray,writeWord8ArrayAsAddr,writeAddrOffAddr,setAddrArray,setAddrOffAddr)
 
unio :: GHC.IO () -> ST_ s
unio (GHC.IO io) s = case unsafeCoerce# io s of (# s , _ #) -> s
{-# INLINE unio #-}
