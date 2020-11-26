{-# language InstanceSigs #-}
module A.Raw where
import Char (Char(..))
import Char8 (Char8(..))
import I8 (I8(..))
import I16 (I16(..))
import I32 (I32(..))
import I64 (I64(..))
import P (P,(∔))
import qualified P.Stable as Stable
import A
import Ordering


type A = ByteArray#
type MA = MutableByteArray#


-- | Unpinned. Size in bytes
instance New# A where new# = newByteArray#
  --new n x s = case new# n s of
    --(# s' , ma #) → case set ma 0# n x s' of
      --s'' → (# s'' , ma #)

-- | Only Mutable arrays have identity with sensible equality.
--
-- see https://gitlab.haskell.org/ghc/ghc/-/issues/13908
instance (≡) (MA s) where
  (≡) = coerce sameMutableByteArray#
  a ≠ b = (¬) (a ≡ b)

-- * Pinned Arrays

-- | Create a new uninitialized pinned (not moved by GC)
-- mutable byte array of specified size (in bytes).
pinned ∷ I → ST# s (MA s)
pinned = newPinnedByteArray#

-- | Create a new uninitialized pinned (not moved by GC)
-- mutable byte array of specified size (in bytes) and alignment.
-- TODO: add docs for which arg is which
aligned ∷ I → I → ST# s (MA s)
aligned = newAlignedPinnedByteArray#


-- | Address may change between GC cycles so this is only safe for pinned arrays
contents ∷ A → P
contents = byteArrayContents#

pinned' ∷ A → B
pinned' = coerce isByteArrayPinned#

pinnedMA' ∷ MA s → B
pinnedMA' = coerce isMutableByteArrayPinned#

-- * Operations

-- | Only unpinned
resize ∷ MA s → I → ST# s (MA s)
resize = resizeMutableByteArray#

-- | New length (bytes) must be ≤ current 'sizeMA'
instance Shrink A where shrink = shrinkMutableByteArray#

-- | Number of bytes.
--
-- note: In @ST#@ because of possible resizes.
sizeM ∷ MA s → ST# s I
sizeM = getSizeofMutableByteArray#

instance Freeze## A where freeze## = unsafeFreezeByteArray#
instance Freeze# A where
  freeze# a off n s = case new# n s of
    (# s' , ma #) → case copy a off ma 0# n s' of s'' → freeze## ma s''
-- | This is just a cast
instance Thaw## A where thaw## a s = (# s , unsafeCoerce# a #)
instance Thaw# A where
  thaw# a off n s = case new# n s of
    (# s' , ma #) → case copy a off ma 0# n s' of s'' → (# s'' , ma #)

instance Size A where size = sizeofByteArray#

-- | Set a slice to the specified byte.
set ∷ MA s
    → I -- ^ slice start offset
    → I -- ^ slice length in bytes
    → I -- ^ the byte to set them to
    → ST_# s
set = setByteArray#

-- | Lexicographic comparison.
-- Warning: Both arrays must fully contain the specified ranges, but this is not checked.
compare# ∷ A -- ^ source1
         → I -- ^ source1 offset
         → A -- ^ source2
         → I -- ^ source2 offset
         → I -- ^ number of bytes to compare
         → Ordering 
compare# = coerce compareByteArrays#

instance Copy A (MA s) s where copy = copyByteArray#
instance Copy (MA s) (MA s) s where copy = copyMutableByteArray#
instance Copy A P s where copy src i dst j n = copyByteArrayToAddr# src i (j ∔ dst) n
instance Copy (MA s) P s where copy src i dst j n = copyMutableByteArrayToAddr# src i (j ∔ dst) n
instance Copy P (MA s) s where copy src i dst j n = copyAddrToByteArray# (i ∔ src) dst j n

-- | Offset in 4-byte words
instance Char8 ∈ A where
  index# = coerce indexCharArray#
  index## = coerce indexWord8ArrayAsChar#
  read#   = coerce readCharArray#
  write#  = coerce writeCharArray#
instance Char ∈ A where
  index# = indexWideCharArray#
  index## = indexWord8ArrayAsWideChar#
  read#   = readWideCharArray#
  write#  = writeWideCharArray#
instance F32 ∈ A where
  index# = indexFloatArray#
  index## = indexWord8ArrayAsFloat#
  read#   = readFloatArray#
  write#  = writeFloatArray#
instance F64 ∈ A where
  index# = indexDoubleArray#
  index## = indexWord8ArrayAsDouble#
  read#   = readDoubleArray#
  write#  = writeDoubleArray#
instance I8 ∈ A where
  index# = coerce indexInt8Array#
  index## = coerce indexInt8Array#
  read#   = coerce readInt8Array#
  write#  = coerce writeInt8Array#
instance I16 ∈ A where
  index# = coerce indexInt16Array#
  index## = coerce indexWord8ArrayAsInt16#
  read#   = coerce readInt16Array#
  write#  = coerce writeInt16Array#
instance I32 ∈ A where
  index# = coerce indexInt32Array#
  index## = coerce indexWord8ArrayAsInt32#
  read#   = coerce readInt32Array#
  write#  = coerce writeInt32Array#
instance I64 ∈ A where
  index# = coerce indexInt64Array#
  index## = coerce indexWord8ArrayAsInt64#
  read#   = coerce readInt64Array#
  write#  = coerce writeInt64Array#
instance I ∈ A where
  index# = indexIntArray#
  index## = indexWord8ArrayAsInt#
  read#   = readIntArray#
  write#  = writeIntArray#
instance U8 ∈ A where
  index# = coerce indexWord8Array#
  index## = coerce indexWord8Array#
  read#   = coerce readWord8Array#
  write#  = coerce writeWord8Array#
instance U16 ∈ A where
  index# = coerce indexWord16Array#
  index## = coerce indexWord8ArrayAsWord16#
  read#   = coerce readWord16Array#
  write#  = coerce writeWord16Array#
instance U32 ∈ A where
  index# = coerce indexWord32Array#
  index## = coerce indexWord8ArrayAsWord32#
  read#   = coerce readWord32Array#
  write#  = coerce writeWord32Array#
instance U64 ∈ A where
  index# = coerce indexWord64Array#
  index## = coerce indexWord8ArrayAsWord64#
  read#   = coerce readWord64Array#
  write#  = coerce writeWord64Array#
instance U ∈ A where
  index# = indexWordArray#
  index## = indexWord8ArrayAsWord#
  read#   = readWordArray#
  write#  = writeWordArray#
instance P ∈ A where
  index# = indexAddrArray#
  index## = indexWord8ArrayAsAddr#
  read#   = readAddrArray#
  write#  = writeAddrArray#
instance (Stable.P a) ∈ A where
  index# = indexStablePtrArray#
  index## = indexWord8ArrayAsStablePtr#
  read#   = readStablePtrArray#
  write#  = writeStablePtrArray#
