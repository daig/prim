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


type A = ByteArray#
type MA = MutableByteArray#

-- | Create a new uninitialized unpinned mutable byte array of specified size (in bytes).
new ∷ I {-^ size in bytes -} → ST# s (MA s)
new = newByteArray#

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
sizeMA ∷ MA s → ST# s I
sizeMA = getSizeofMutableByteArray#

-- | Make a mutable array immutable, without copying.
freeze## ∷ MA s → ST# s A
freeze## = unsafeFreezeByteArray#

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

-- | a number less-than, equal-to, or greater-than @0#@
newtype Ordering ∷ T_I where Ordering# ∷ I → Ordering
pattern LT ∷ Ordering
pattern LT ← ((\(Ordering# i) → i < 0# ) → T ) where LT = Ordering# -1#
pattern GT ← ((\(Ordering# i) → i > 0# ) → T ) where GT = Ordering# 1#
pattern EQ ← ((\(Ordering# i) → i ≡ 0# ) → T ) where EQ = Ordering# 1#
{-# complete LT, GT, EQ #-}

instance Copy A (MA s) s where copy = copyByteArray#
instance Copy (MA s) (MA s) s where copy = copyMutableByteArray#
instance Copy A P s where copy src i dst j n = copyByteArrayToAddr# src i (j ∔ dst) n
instance Copy (MA s) P s where copy src i dst j n = copyMutableByteArrayToAddr# src i (j ∔ dst) n
instance Copy P (MA s) s where copy src i dst j n = copyAddrToByteArray# (i ∔ src) dst j n


