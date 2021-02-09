--------------------------------------------------------------------
-- | Description : Unlifted Pinned Primitive Arrays
--------------------------------------------------------------------
{-# language TypeApplications, DerivingVia, InstanceSigs #-}
{-# language FlexibleContexts #-}
{-# language CPP #-}
{-# language ScopedTypeVariables #-}
module Prim.A.Prim.Pinned where
import Prim.Char (Char(..))
import Prim.Char8 (Char8(..))
import Prim.I8 (I8(..))
import Prim.I16 (I16(..))
import Prim.I32 (I32(..))
import Prim.I64 (I64(..))
import P
import qualified P.Stable as Stable
import Prim.A
import Prim.Ordering
#include "MachDeps.h"
import qualified Prim.I as I
import Prim.A.Prim.Elts as X

-- | Pinned arrays have a fixed addres and aren't moved by GC
newtype A    (x ∷ T_ r) ∷ T_A where A#  ∷ ∀   r (x ∷ T_ r). ByteArray#          → A    x
newtype MA s (x ∷ T_ r) ∷ T_A where MA# ∷ ∀ s r (x ∷ T_ r). MutableByteArray# s → MA s x
instance (≡) (MA s x) where
  (≡) = coerce sameMutableByteArray#
  as ≠ bs = (¬) (as ≡ bs)

-- * Operations

-- | Create a new uninitialized pinned mutable byte array
-- of specified size (in bytes) and alignment.
-- TODO: add docs for which arg is which
aligned ∷ I {- ^ size in bytes -} → I {- ^ alignment in bytes -} → ST# s (MA s x)
aligned = coerce newAlignedPinnedByteArray#

-- | Address may change between GC cycles so this is only safe for pinned arrays
contents ∷ A x → P#
contents = coerce byteArrayContents#

pinned' ∷ A x → (# B , A x #)
pinned' a = (# coerce isByteArrayPinned# a , coerce a #)

pinnedMA' ∷ MA s x → (# B , MA s x #)
pinnedMA' ma = (# coerce isMutableByteArrayPinned# ma , coerce ma #)

-- | New length (bytes) must be ≤ current 'sizeMA'
instance (♭) x ⇒ Shrink (A x) where shrink = coerce shrinkMutableByteArray#

-- | "A.Prim" -
-- 'thaw##' is just a cast.
-- @new#@ unpinned w/ init size in bytes.
instance 𝔸 (A (x ∷ T_ r)) where
  freeze## = coerce unsafeFreezeByteArray#
  freeze# a off n s = case new# n s of
    (# s' , ma #) → case copy a off ma 0# n s' of s'' → freeze## ma s''
  thaw## a s = (# s , unsafeCoerce# a #)
  thaw# a off n s = case new# n s of
    (# s' , ma #) → case copy a off ma 0# n s' of s'' → (# s'' , ma #)
  new# = coerce newPinnedByteArray#
  len = coerce sizeofByteArray#
  lenM# = coerce sizeofMutableByteArray#
  lenM  = coerce getSizeofMutableByteArray#

-- | Given an array, an offset in machine words, the expected old value, and the
-- new value, perform an atomic compare and swap i.e. write the new value if the
-- current value matches the provided old value. Returns the value of the
-- element before the operation. Implies a full memory barrier.
cas# ∷ MA s U → I       -- ^ offset in machine words
            → U       -- ^ expected old value
            → U       -- ^ new value
            → ST# s U -- ^ actual old value
cas# (MA# ma) o (word2Int# → x0) (word2Int# → x1) s =
  case casIntArray# ma o x0 x1 s of (# s , x #) → (# s , int2Word# x #)

-- | Set a slice to the specified byte.
set ∷ MA s U8
    → I  -- ^ slice start offset
    → I  -- ^ slice length in bytes
    → U8 -- ^ the byte to set them to
    → ST_# s
set ma i n (U8 b) = coerce setByteArray# ma i n (word2Int# b)

-- | Lexicographic comparison.
-- Warning: Both arrays must fully contain the specified ranges, but this is not checked.
compare# ∷ A U8 -- ^ source1
         → I   -- ^ source1 offset
         → A U8 -- ^ source2
         → I   -- ^ source2 offset
         → I   -- ^ number of bytes to compare
         → Ordering 
compare# = coerce compareByteArrays#

instance Copy (A (x ∷ T_ r)) (MA s x) s where copy = coerce copyByteArray#
instance Copy (MA s (x ∷ T_ r)) (MA s x) s where copy = coerce copyMutableByteArray#
instance Copy (A x) (P x) s where copy src i (P# dst) j n = coerce copyByteArrayToAddr# src i (j ∔ dst) n
instance Copy (MA s x) (P x) s where copy src i (P# dst) j n = coerce copyMutableByteArrayToAddr# src i (j ∔ dst) n
instance Copy (P x) (MA s x) s where copy (P# src) i dst j n = coerce copyAddrToByteArray# (i ∔ src) dst j n

-- | "A.Prim"
instance (♭) a ⇒ (a ∷ T_ r) ∈ (A a) where
  index# (A# a) = indexA# a
  read# (MA# ma) = readA# ma
  write# (MA# ma) = writeA# ma
-- | "A.Prim"
--
-- Treat the 'A.Prim.A U8' as an untyped region.
-- Offsets in bytes, not elements.
--
-- Useful for packing heterogeneous records.
-- For homogenous arrays prefer a well-typed 'A'
instance (♭) a ⇒ a ∈ A U8 where
--  new = newByteArray#
  index# (A# a) = indexB# a
  read# (MA# ma) = readB# ma
  write# (MA# ma) = writeB# ma
