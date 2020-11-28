--------------------------------------------------------------------
-- | Description : Raw Byte Arrays
--
-- Untyped region of 'U8' bytes. Useful for packing heterogeneous records.
-- For homogenous arrays prefer "A.Unboxed"
--------------------------------------------------------------------
module A.Byte where
import {-# source #-} A
import qualified P.Stable as Stable
import Ordering
import P
import IO

type A = ByteArray#
type MA = MutableByteArray#
type instance M A s = MA s


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
  clone# a off n = IO.run \ s → case thaw## a s of
    (# s' , ma #) → case freeze# ma off n s' of (# _ , a' #) → a'
  cloneM# ma off n = \s → case new# n s of
    (# s' , ma' #) → case copy ma off ma 0# n s' of s'' → (# s'' , ma' #)

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

-- | Given an array, an offset in machine words, the expected old value, and the
-- new value, perform an atomic compare and swap i.e. write the new value if the
-- current value matches the provided old value. Returns the value of the
-- element before the operation. Implies a full memory barrier.
cas# ∷ MA s → I       -- ^ offset in machine words
            → U       -- ^ expected old value
            → U       -- ^ new value
            → ST# s U -- ^ actual old value
cas# ma o (word2Int# → x0) (word2Int# → x1) s =
  case casIntArray# ma o x0 x1 s of (# s , x #) → (# s , int2Word# x #)

instance Copy A (MA s) s where copy = copyByteArray#
instance Copy (MA s) (MA s) s where copy = copyMutableByteArray#
instance Copy A P s where copy src i dst j n = copyByteArrayToAddr# src i (j ∔ dst) n
instance Copy (MA s) P s where copy src i dst j n = copyMutableByteArrayToAddr# src i (j ∔ dst) n
instance Copy P (MA s) s where copy src i dst j n = copyAddrToByteArray# (i ∔ src) dst j n
