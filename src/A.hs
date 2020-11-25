{-# language FlexibleInstances,InstanceSigs,MultiParamTypeClasses #-}
module A where
import P hiding (Prim)
import Char8
import Char
import I32 (I32(..))
import I16 (I16(..))
import I8 (I8(..))
import I64 (I64(..))
import qualified P.Stable as Stable
import qualified B


type A = ByteArray#
type MA = MutableByteArray#

pinned' ∷ A → B#
pinned' = isByteArrayPinned#

pinnedMA' ∷ MA s → B#
pinnedMA' = isMutableByteArrayPinned#

-- | Only Mutable arrays have identity with sensible equality.
--
-- see https://gitlab.haskell.org/ghc/ghc/-/issues/13908
instance (≡) (MA s) where
  (≡) = sameMutableByteArray#
  a ≠ b = B.not# (a ≡ b)

-- | New length must be ≤ current 'sizeMA'
shrink ∷ MA s → I {- ^ # bytes #-} → ST_# s
shrink = shrinkMutableByteArray#

-- | Number of bytes
size ∷ A → I
size = sizeofByteArray#

-- | Number of bytes.
--
-- note: In @ST#@ because of possible resizes.
sizeMA ∷ MA s → ST# s I
sizeMA = getSizeofMutableByteArray#

-- | MAake a mutable array immutable, without copying.
freeze## ∷ MA s → ST# s A
freeze## = unsafeFreezeByteArray#

class Copy (src ∷ TYPE r) (dst ∷ TYPE r') (s ∷ T) where
  -- | Copy the elements from the source to the destination.
  -- Both must fully contain the specified ranges and not overlap in memory,
  -- but this is not checked.
  --
  -- Warning: this can fail with an unchecked exception.
  copy ∷ src → I → dst → I → I → ST_# s
instance Copy A (MA s) s where copy = copyByteArray#
instance Copy (MA s) (MA s) s where copy = copyMutableByteArray#
instance Copy A P s where copy src i dst j n = copyByteArrayToAddr# src i (j ∔ dst) n
instance Copy (MA s) P s where copy src i dst j n = copyMutableByteArrayToAddr# src i (j ∔ dst) n
instance Copy P (MA s) s where copy src i dst j n = copyAddrToByteArray# (i ∔ src) dst j n

-- | Set a slice to the specified byte.
set ∷ MA s
    → I -- ^ slice start offset
    → I -- ^ slice length in bytes
    → I -- ^ the byte to set them to
    → ST_# s
set = setByteArray#

-- | Lexicographic comparison.
-- Warning: Both arrays mus fully contain the specified ranges, but this is not checked.
compare# ∷ A -- ^ source1
         → I -- ^ source1 offset
         → A -- ^ source2
         → I -- ^ source2 offset
         → I -- ^ number of bytes to compare
         → I -- ^ a number less-than, equal-to, or greater-than @0#@
compare# = compareByteArrays#

class Prim (a ∷ TYPE r) where
  index# ∷ A → I {- ^ Offset in elements -} → a
  index## ∷ A → I {- ^ Offset in bytes -} → a
  read# ∷ MA s → I → ST# s a
  write# ∷ MA s → I → a → ST_# s
instance Prim Char8 where
  index# ∷ A → I {- ^ Offset in 4-byte words -} → Char8
  index# = coerce indexCharArray#
  index## = coerce indexWord8ArrayAsChar#
  read# = coerce readCharArray#
  write# = coerce writeCharArray#
instance Prim Char where
  index# = indexWideCharArray#
  index## = indexWord8ArrayAsWideChar#
  read# = readWideCharArray#
  write# = writeWideCharArray#
instance Prim F32 where
  index# = indexFloatArray#
  index## = indexWord8ArrayAsFloat#
  read# = readFloatArray#
  write# = writeFloatArray#
instance Prim F64 where
  index# = indexDoubleArray#
  index## = indexWord8ArrayAsDouble#
  read# = readDoubleArray#
  write# = writeDoubleArray#
instance Prim I8 where
  index# = coerce indexInt8Array#
  index## = coerce indexInt8Array#
  read# = coerce readInt8Array#
  write# = coerce writeInt8Array#
instance Prim I16 where
  index# = coerce indexInt16Array#
  index## = coerce indexWord8ArrayAsInt16#
  read# = coerce readInt16Array#
  write# = coerce writeInt16Array#
instance Prim I32 where
  index# = coerce indexInt32Array#
  index## = coerce indexWord8ArrayAsInt32#
  read# = coerce readInt32Array#
  write# = coerce writeInt32Array#
instance Prim I64 where
  index# = coerce indexInt64Array#
  index## = coerce indexWord8ArrayAsInt64#
  read# = coerce readInt64Array#
  write# = coerce writeInt64Array#
instance Prim I where
  index# = coerce indexIntArray#
  index## = coerce indexWord8ArrayAsInt#
  read# = coerce readIntArray#
  write# = coerce writeIntArray#
instance Prim U8 where
  index# = coerce indexWord8Array#
  index## = coerce indexWord8Array#
  read# = coerce readWord8Array#
  write# = coerce writeWord8Array#
instance Prim U16 where
  index# = coerce indexWord16Array#
  index## = coerce indexWord8ArrayAsWord16#
  read# = coerce readWord16Array#
  write# = coerce writeWord16Array#
instance Prim U32 where
  index# = coerce indexWord32Array#
  index## = coerce indexWord8ArrayAsWord32#
  read# = coerce readWord32Array#
  write# = coerce writeWord32Array#
instance Prim U64 where
  index# = coerce indexWord64Array#
  index## = coerce indexWord8ArrayAsWord64#
  read# = coerce readWord64Array#
  write# = coerce writeWord64Array#
instance Prim U where
  index# = coerce indexWordArray#
  index## = coerce indexWord8ArrayAsWord#
  read# = coerce readWordArray#
  write# = coerce writeWordArray#
instance Prim P where
  index# = coerce indexAddrArray#
  index## = coerce indexWord8ArrayAsAddr#
  read# = coerce readAddrArray#
  write# = coerce writeAddrArray#
instance Prim (Stable.P a) where
  index# = indexStablePtrArray#
  index## = indexWord8ArrayAsStablePtr#
  read# = readStablePtrArray#
  write# = writeStablePtrArray#
