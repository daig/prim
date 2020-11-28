{-# language TypeApplications, DerivingVia, InstanceSigs #-}
{-# language FlexibleContexts #-}
{-# language CPP #-}
module A.Unboxed where
import Char (Char(..))
import Char8 (Char8(..))
import I8 (I8(..))
import I16 (I16(..))
import I32 (I32(..))
import I64 (I64(..))
import P (P,(∔))
import A.P ()
import qualified P.Stable as Stable
import A
import Ordering
import qualified Bytes
#include "MachDeps.h"
import qualified I

newtype A    (x ∷ T_ r) ∷ T_A where A#  ∷ ∀   r (x ∷ T_ r). ByteArray#          → A    x
newtype MA s (x ∷ T_ r) ∷ T_A where MA# ∷ ∀ s r (x ∷ T_ r). MutableByteArray# s → MA s x
deriving newtype instance (≡) (MA s x)

-- * Pinned Arrays

-- | Create a new uninitialized pinned (not moved by GC)
-- mutable byte array of specified size (in bytes).
pinned ∷ I → ST# s (MA s x)
pinned = coerce newPinnedByteArray#

-- | Create a new uninitialized pinned (not moved by GC)
-- mutable byte array of specified size (in bytes) and alignment.
-- TODO: add docs for which arg is which
aligned ∷ I → I → ST# s (MA s x)
aligned = coerce newAlignedPinnedByteArray#


-- | Address may change between GC cycles so this is only safe for pinned arrays
contents ∷ A x → P
contents = coerce byteArrayContents#

pinned' ∷ A x → B
pinned' = coerce isByteArrayPinned#

pinnedMA' ∷ MA s x → B
pinnedMA' = coerce isMutableByteArrayPinned#

-- * Operations

-- | Only unpinned
resize ∷ MA s x → I → ST# s (MA s x)
resize = coerce resizeMutableByteArray#

-- | New length (bytes) must be ≤ current 'sizeMA'
instance Shrink (A x) where shrink = coerce shrinkMutableByteArray#

-- | 'thaw##' is just a cast
--
-- 'new' Unpinned w/ init size in bytes
instance 𝔸 (A x) where
  freeze## = coerce unsafeFreezeByteArray#
  freeze# a off n s = case new# n s of
    (# s' , ma #) → case copy a off ma 0# n s' of s'' → freeze## ma s''
  thaw## a s = (# s , unsafeCoerce# a #)
  thaw# a off n s = case new# n s of
    (# s' , ma #) → case copy a off ma 0# n s' of s'' → (# s'' , ma #)
  new# = coerce newByteArray#
  len = coerce sizeofByteArray#
  lenM# = coerce sizeofMutableByteArray#
  lenM  = coerce getSizeofMutableByteArray#

-- | Set a slice to the specified byte.
set ∷ MA s x
    → I  -- ^ slice start offset
    → I  -- ^ slice length in bytes
    → U8 -- ^ the byte to set them to
    → ST_# s
set ma i n (U8 b) = coerce setByteArray# ma i n (word2Int# b)

-- | Lexicographic comparison.
-- Warning: Both arrays must fully contain the specified ranges, but this is not checked.
compare# ∷ A x -- ^ source1
         → I   -- ^ source1 offset
         → A x -- ^ source2
         → I   -- ^ source2 offset
         → I   -- ^ number of bytes to compare
         → Ordering 
compare# = coerce compareByteArrays#

instance Copy (A x) (MA s x) s where copy = coerce copyByteArray#
instance Copy (MA s x) (MA s x) s where copy = coerce copyMutableByteArray#
instance Copy (A x) P s where copy src i dst j n = coerce copyByteArrayToAddr# src i (j ∔ dst) n
instance Copy (MA s x) P s where copy src i dst j n = coerce copyMutableByteArrayToAddr# src i (j ∔ dst) n
instance Copy P (MA s x) s where copy src i dst j n = coerce copyAddrToByteArray# (i ∔ src) dst j n

-- | Primitive unboxed, unlifted types that fit natively into raw memory
class (a ∈ Bytes.A, a ∈ A a, a ∈ P) ⇒ (♭) (a ∷ T_ r) where
  size ∷ I {- ^ # elements -} → I {- ^ size in bytes -}
  align ∷ I → I

#define INST_PRIM(T, SIZE, ALIGN) \
instance (♭) T where \
  size = (SIZE# I.×); \
  align i = case i I.% ALIGN# of {0# → i ;off → i I.+ (ALIGN# I.- off)} 

INST_PRIM(I, SIZEOF_HSINT, ALIGNMENT_HSINT)
INST_PRIM(I8, SIZEOF_INT8, ALIGNMENT_INT8)
INST_PRIM(I16, SIZEOF_INT16, ALIGNMENT_INT16)
INST_PRIM(I32, SIZEOF_INT32, ALIGNMENT_INT32)
INST_PRIM(I64, SIZEOF_INT64, ALIGNMENT_INT64)
INST_PRIM(U, SIZEOF_HSWORD, ALIGNMENT_HSWORD)
INST_PRIM(U8, SIZEOF_WORD8, ALIGNMENT_WORD8)
INST_PRIM(U16, SIZEOF_WORD16, ALIGNMENT_WORD16)
INST_PRIM(U32, SIZEOF_WORD32, ALIGNMENT_WORD32)
INST_PRIM(U64, SIZEOF_WORD64, ALIGNMENT_WORD64)
INST_PRIM(P, SIZEOF_HSPTR, ALIGNMENT_HSPTR)
INST_PRIM((Stable.P a), SIZEOF_HSSTABLEPTR, ALIGNMENT_HSSTABLEPTR)
INST_PRIM(Char, SIZEOF_HSCHAR, ALIGNMENT_HSCHAR)
deriving newtype instance (♭) Char8

instance Char8 ∈ (A Char8) where
  index# = coerce indexCharArray#
  read#   = coerce readCharArray#
  write#  = coerce writeCharArray#
instance Char ∈ (A Char) where
  index# = coerce indexWideCharArray#
  read#   = coerce readWideCharArray#
  write#  = coerce writeWideCharArray#
instance F32 ∈ (A F32) where
  index# = coerce indexFloatArray#
  read#   = coerce readFloatArray#
  write#  = coerce writeFloatArray#
instance F64 ∈ (A F64) where
  index# = coerce indexDoubleArray#
  read#   = coerce readDoubleArray#
  write#  = coerce  writeDoubleArray#
instance I8 ∈ (A I8) where
  index# = coerce indexInt8Array#
  read#   = coerce readInt8Array#
  write#  = coerce writeInt8Array#
instance I16 ∈ (A I16) where
  index# = coerce indexInt16Array#
  read#   = coerce readInt16Array#
  write#  = coerce writeInt16Array#
instance I32 ∈ (A I32) where
  index# = coerce indexInt32Array#
  read#   = coerce readInt32Array#
  write#  = coerce writeInt32Array#
instance I64 ∈ (A I64) where
  index# = coerce indexInt64Array#
  read#   = coerce readInt64Array#
  write#  = coerce writeInt64Array#
instance I ∈ (A I) where
  index# = coerce indexIntArray#
  read#   = coerce readIntArray#
  write#  = coerce writeIntArray#
instance U8 ∈ (A U8) where
  index# = coerce indexWord8Array#
  read#   = coerce readWord8Array#
  write#  = coerce writeWord8Array#
instance U16 ∈ (A U16) where
  index# = coerce indexWord16Array#
  read#   = coerce readWord16Array#
  write#  = coerce writeWord16Array#
instance U32 ∈ (A U32) where
  index# = coerce indexWord32Array#
  read#   = coerce readWord32Array#
  write#  = coerce writeWord32Array#
instance U64 ∈ (A U64) where
  index# = coerce indexWord64Array#
  read#   = coerce readWord64Array#
  write#  = coerce writeWord64Array#
instance U ∈ (A U) where
  index# = coerce indexWordArray#
  read#   = coerce readWordArray#
  write#  = coerce writeWordArray#
instance P ∈ (A P) where
  index# = coerce indexAddrArray#
  read#   = coerce readAddrArray#
  write# = coerce writeAddrArray#
instance (Stable.P a) ∈ (A (Stable.P a)) where
  index# (A# a) = indexStablePtrArray# a
  read# (MA# ma) = readStablePtrArray# ma
  write# (MA# ma) = writeStablePtrArray# ma
