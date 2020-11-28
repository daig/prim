{-# language InstanceSigs #-}
module Bytes where
import Char (Char(..))
import Char8 (Char8(..))
import I8 (I8(..))
import I16 (I16(..))
import I32 (I32(..))
import I64 (I64(..))
import U8 (U8(..))
import P (P,(∔))
import qualified P.Stable as Stable
import A
import Ordering


type A = ByteArray#
type MA = MutableByteArray#

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


-- | 'thaw##' is just a cast
-- | 'new' Unpinned w/ init size in bytes
instance 𝔸 A where
  freeze## = unsafeFreezeByteArray#
  freeze# a off n s = case new# n s of
    (# s' , ma #) → case copy a off ma 0# n s' of s'' → freeze## ma s''
  thaw## a s = (# s , unsafeCoerce# a #)
  thaw# a off n s = case new# n s of
    (# s' , ma #) → case copy a off ma 0# n s' of s'' → (# s'' , ma #)
  new# = newByteArray#
  len = sizeofByteArray#
  lenM# = sizeofMutableByteArray#
  lenM = getSizeofMutableByteArray#

-- | Set a slice to the specified byte.
set ∷ MA s
    → I  -- ^ slice start offset
    → I  -- ^ slice length in bytes
    → U8 -- ^ the byte to set them to
    → ST_# s
set ma i n (U8 b) = setByteArray# ma i n (word2Int# b)

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
  index# = coerce indexWord8ArrayAsChar#
  read#   = coerce readWord8ArrayAsChar#
  write#  = coerce writeWord8ArrayAsChar#
instance Char ∈ A where
  index# = indexWord8ArrayAsWideChar#
  read#   = readWord8ArrayAsWideChar#
  write#  = writeWord8ArrayAsWideChar#
instance F32 ∈ A where
  index# = indexWord8ArrayAsFloat#
  read# = readWord8ArrayAsFloat#
  write# = writeWord8ArrayAsFloat#
instance F64 ∈ A where
  index# = indexWord8ArrayAsDouble#
  read# = readWord8ArrayAsDouble#
  write# = writeWord8ArrayAsDouble#
instance I8 ∈ A where
  index# = coerce indexInt8Array#
  read#   = coerce readInt8Array#
  write#  = coerce writeInt8Array#
instance I16 ∈ A where
  index# = coerce indexWord8ArrayAsInt16#
  read# = coerce readWord8ArrayAsInt16#
  write# = coerce writeWord8ArrayAsInt16#
instance I32 ∈ A where
  index# = coerce indexWord8ArrayAsInt32#
  read# = coerce readWord8ArrayAsInt32#
  write# = coerce writeWord8ArrayAsInt32#
instance I64 ∈ A where
  index# = coerce indexWord8ArrayAsInt64#
  read# = coerce readWord8ArrayAsInt64#
  write# = coerce writeWord8ArrayAsInt64#
instance I ∈ A where
  index# = indexWord8ArrayAsInt#
  read# = readWord8ArrayAsInt#
  write# = writeWord8ArrayAsInt#
instance U8 ∈ A where
  index# = coerce indexWord8Array#
  read#   = coerce readWord8Array#
  write#  = coerce writeWord8Array#
instance U16 ∈ A where
  index# = coerce indexWord8ArrayAsWord16#
  read# = coerce readWord8ArrayAsWord16#
  write# = coerce writeWord8ArrayAsWord16#
instance U32 ∈ A where
  index# = coerce indexWord8ArrayAsWord32#
  read# = coerce readWord8ArrayAsWord32#
  write# = coerce writeWord8ArrayAsWord32#
instance U64 ∈ A where
  index# = coerce indexWord8ArrayAsWord64#
  read# = coerce readWord8ArrayAsWord64#
  write# = coerce writeWord8ArrayAsWord64#
instance U ∈ A where
  index# = indexWord8ArrayAsWord#
  read# = readWord8ArrayAsWord#
  write# = writeWord8ArrayAsWord#
instance P ∈ A where
  index# = indexWord8ArrayAsAddr#
  read# = readWord8ArrayAsAddr#
  write# = writeWord8ArrayAsAddr#
instance (Stable.P a) ∈ A where
  index# = indexWord8ArrayAsStablePtr#
  read# = readWord8ArrayAsStablePtr#
  write# = writeWord8ArrayAsStablePtr#
