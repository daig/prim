--------------------------------------------------------------------
-- | Description : Primitive Array types
--------------------------------------------------------------------
{-# language TypeFamilyDependencies, FlexibleInstances,InstanceSigs,MultiParamTypeClasses #-}
{-# language CPP #-}
{-# language QuantifiedConstraints #-}
module A (Bytes,MBytes,Refs,MRefs,module A)where
import P hiding (Prim)
import Char
import I32 (I32(..))
import I16 (I16(..))
import I8 (I8(..))
import I64 (I64(..))
import I
import qualified P.Stable as Stable
import qualified B
import {-# source #-} qualified A.Prim as Prim
import {-# source #-} qualified A.Array as Ref
import {-# source #-} qualified A.Boxed.Big as Big
import {-# source #-} qualified A.Boxed as Boxed
import qualified P.STM as STM
import Stock.Int
#include "MachDeps.h"
#include "HsBaseConfig.h"


class 𝔸 (a ∷ T_A) where
  -- | Uninitialized array.
  new# ∷ I {-^ size in elements -} → ST# s (M a s)
  -- | Make a mutable array immutable, without copying.
  freeze## ∷ M a s → ST# s a
  -- | Make an immutable array mutable, without copying.
  thaw## ∷ a → ST# s (M a s)
  -- | Copy an immutable array into a new mutable one.
  thaw# ∷  a
          → I -- ^ Source offset
          → I -- ^ number of elements to copy
          → ST# s (M a s)
  -- | Create a new immutable array from a mutable by copying
  freeze# ∷ M a s
          → I -- ^ Source offset
          → I -- ^ number of elements to copy
          → ST# s a
  -- | Number of elements
  len ∷ a → I
  -- | Like 'len' for mutable arrays. Only safe in the absence of resizes
  lenM# ∷ M a s → I
  -- | Like 'len' for mutable arrays.
  lenM ∷ M a s → ST# s I
  -- | Create a new array with the elements from the source array.
  -- The provided array must fully contain the specified range, but this is not checked.
  --
  -- Warning: this can fail with an unchecked exception.
  clone# ∷ a
         → I -- ^ Source offset
         → I -- ^ number of elements to copy
         → a
  -- | Create a new array with the elements from the source array.
  -- The provided array must fully contain the specified range, but this is not checked.
  --
  -- Warning: this can fail with an unchecked exception.
  cloneM# ∷ M a s
          → I -- ^ Source offset
          → I -- ^ number of elements to copy
          → ST# s (M a s)
class 𝔸 a ⇒ Shrink (a ∷ T_A) where shrink ∷ M a s → I → ST_# s

type family M (a ∷ k) (s ∷ T) = (ma ∷ k) | ma → a where
  M Bytes s = MBytes s
  M Refs  s = MRefs s
  M (P (x ∷ T_ r)) s = P x
  M (Prim.A (x ∷ T_ r_prim)) s = Prim.MA s x
  M (Boxed.A (x ∷ T)) s = Boxed.MA s x
  M (Big.A (x_big ∷ T)) s = Big.MA s x_big
  M (Ref.A (x ∷ T_A)) s = Ref.MA s x

class Copy (src ∷ T_ r) (dst ∷ T_ r') s where
  -- | Copy the elements from the source to the destination.
  -- Both must fully contain the specified ranges and not overlap in memory,
  -- but this is not checked.
  --
  -- Warning: this can fail with an unchecked exception.
  copy ∷ src
       → I -- ^ Source Offset (bytes)
       → dst
       → I -- ^ Destination Offset (bytes)
       → I -- ^ Number of elements to copy
       → ST_# s

class (x ∷ T_ r) ∈ (a ∷ T_ r') where
  index# ∷ a → I {- ^ Offset in elements -} → x
  read# ∷ M a s → I → ST# s x
  write# ∷ M a s → I → x → ST_# s
  -- | Initialize an array
  new ∷ I {-^ size in elements -} → x → ST# s (M a s)

-- | Primitive unboxed, unlifted types that fit natively into raw memory
class (♭) (a ∷ T_ r) where
  size ∷ I {- ^ # elements -} → I {- ^ size in bytes -}
  align ∷ I → I
  indexA# ∷ Bytes → I → a
  readA# ∷ MBytes s → I → ST# s a
  writeA# ∷ M Bytes s → I → a → ST_# s
  indexP# ∷ P# → I → a
  readP# ∷ P# → I → ST# s a
  writeP# ∷ P# → I → a → ST_# s
  indexB# ∷ Bytes → I → a
  readB# ∷ MBytes s → I → ST# s a
  writeB# ∷ MBytes s → I → a → ST_# s

#define INST_PRIM(T,S,A,IA,RA,WA,IP,RP,WP,IB,RB,WB) \
instance (♭) T where \
  size = (S# I.×); \
  align i = case i I.% A# of {0# → i ;off → i I.+ (A# I.- off)}; \
  indexA# = coerce IA#; \
  readA# = coerce RA#; \
  writeA# = coerce WA#; \
  indexP# = coerce IP#; \
  readP# = coerce RP#; \
  writeP# = coerce WP#; \
  indexB# = coerce IB#; \
  readB# = coerce RB#; \
  writeB# = coerce WB#

INST_PRIM(I, SIZEOF_HSINT, ALIGNMENT_HSINT, indexIntArray, readIntArray, writeIntArray, indexIntOffAddr, readIntOffAddr, writeIntOffAddr, indexWord8ArrayAsInt, readWord8ArrayAsInt, writeWord8ArrayAsInt)
INST_PRIM(I8, SIZEOF_INT8, ALIGNMENT_INT8, indexInt8Array, readInt8Array, writeInt8Array, indexInt8OffAddr, readInt8OffAddr, writeInt8OffAddr, indexInt8Array, readInt8Array, writeInt8Array)
INST_PRIM(I16, SIZEOF_INT16, ALIGNMENT_INT16, indexInt16Array, readInt16Array, writeInt16Array, indexInt16OffAddr, readInt16OffAddr, writeInt16OffAddr, indexWord8ArrayAsInt16, readWord8ArrayAsInt16, writeWord8ArrayAsInt16)
INST_PRIM(I32, SIZEOF_INT32, ALIGNMENT_INT32, indexInt32Array, readInt32Array, writeInt32Array, indexInt32OffAddr, readInt32OffAddr, writeInt32OffAddr, indexWord8ArrayAsInt32, readWord8ArrayAsInt32, writeWord8ArrayAsInt32)
INST_PRIM(I64, SIZEOF_INT64, ALIGNMENT_INT64, indexInt64Array, readInt64Array, writeInt64Array, indexInt64OffAddr, readInt64OffAddr, writeInt64OffAddr, indexWord8ArrayAsInt64, readWord8ArrayAsInt64, writeWord8ArrayAsInt64)
INST_PRIM(U, SIZEOF_HSWORD, ALIGNMENT_HSWORD, indexWordArray, readWordArray, writeWordArray, indexWordOffAddr, readWordOffAddr, writeWordOffAddr, indexWord8ArrayAsWord, readWord8ArrayAsWord, writeWord8ArrayAsWord)
INST_PRIM(U8, SIZEOF_WORD8, ALIGNMENT_WORD8, indexWord8Array, readWord8Array, writeWord8Array, indexWord8OffAddr, readWord8OffAddr, writeWord8OffAddr, indexWord8Array, readWord8Array, writeWord8Array)
INST_PRIM(U16, SIZEOF_WORD16, ALIGNMENT_WORD16, indexWord16Array, readWord16Array, writeWord16Array, indexWord16OffAddr, readWord16OffAddr, writeWord16OffAddr, indexWord8ArrayAsWord16, readWord8ArrayAsWord16, writeWord8ArrayAsWord16)
INST_PRIM(U32, SIZEOF_WORD32, ALIGNMENT_WORD32, indexWord32Array, readWord32Array, writeWord32Array, indexWord32OffAddr, readWord32OffAddr, writeWord32OffAddr, indexWord8ArrayAsWord32, readWord8ArrayAsWord32, writeWord8ArrayAsWord32)
INST_PRIM(U64, SIZEOF_WORD64, ALIGNMENT_WORD64, indexWord64Array, readWord64Array, writeWord64Array, indexWord64OffAddr, readWord64OffAddr, writeWord64OffAddr, indexWord8ArrayAsWord64, readWord8ArrayAsWord64, writeWord8ArrayAsWord64)
INST_PRIM(Char, SIZEOF_HSCHAR, ALIGNMENT_HSCHAR, indexWideCharArray, readWideCharArray, writeWideCharArray, indexWideCharOffAddr, readWideCharOffAddr, writeWideCharOffAddr, indexWord8ArrayAsWideChar, readWord8ArrayAsWideChar, writeWord8ArrayAsWideChar)
INST_PRIM(Char8, SIZEOF_HSCHAR, ALIGNMENT_HSCHAR, indexCharArray, readCharArray, writeCharArray, indexCharOffAddr, readCharOffAddr, writeCharOffAddr, indexWord8ArrayAsChar, readWord8ArrayAsChar, writeWord8ArrayAsChar)
INST_PRIM(P#, SIZEOF_HSPTR, ALIGNMENT_HSPTR, indexAddrArray, readAddrArray, writeAddrArray, indexAddrOffAddr, readAddrOffAddr, writeAddrOffAddr, indexWord8ArrayAsAddr, readWord8ArrayAsAddr, writeWord8ArrayAsAddr)
instance (♭) (Stable.P a) where
  size = (SIZEOF_HSSTABLEPTR# I.×)
  align i = case i I.% ALIGNMENT_HSSTABLEPTR# of {0# → i ;off → i I.+ (ALIGNMENT_HSSTABLEPTR# I.- off)}
  indexA# = indexStablePtrArray#
  readA# = readStablePtrArray#
  writeA# = writeStablePtrArray#
  indexP# = indexStablePtrOffAddr#
  readP# = readStablePtrOffAddr# 
  writeP# = writeStablePtrOffAddr#
  indexB# = indexWord8ArrayAsStablePtr#
  readB# = readWord8ArrayAsStablePtr# 
  writeB# = writeWord8ArrayAsStablePtr#

-- | "A.P"
instance (♭) a ⇒ (a ∷ T_ r) ∈ P a where
  index# (P# p) = indexP# p
  read# (P# p) = readP# p
  write# (P# p) = writeP# p
