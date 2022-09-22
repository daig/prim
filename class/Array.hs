module Array where
import Do.ST as ST
import Cast

type Array ∷ ∀ {rx}. (T rx → T_) → C
class Array a where
  -- | Uninitialized array.
  new# ∷ I {-^ size in elements -} → ST s (M a s x)
  -- | Make a mutable array immutable, without copying.
  freeze## ∷ M a s x → ST s (a x)
  -- | Make an immutable array mutable, without copying.
  thaw## ∷ a x → ST s (M a s x)
  -- | Copy an immutable array into a new mutable one.
  thaw# ∷  a x
          → I -- ^ Source offset
          → I -- ^ number of elements to copy
          → ST s (M a s x)
  -- | Create a new immutable array from a mutable by copying
  freeze# ∷ M a s x
          → I -- ^ Source offset
          → I -- ^ number of elements to copy
          → ST s (a x)
  -- | Number of elements
  len ∷ a x → I
  -- | Like 'len' for mutable arrays. Only safe in the absence of resizes
  lenM# ∷ M a s x → I
  -- | Like 'len' for mutable arrays.
  lenM ∷ M a s x → ST s I
  -- | Create a new array with the elements from the source array.
  -- The provided array must fully contain the specified range, but this is not checked.
  --
  -- Warning: this can fail with an unchecked exception.
  clone# ∷ a x
         → I -- ^ Source offset
         → I -- ^ number of elements to copy
         → a x
  -- | Create a new array with the elements from the source array.
  -- The provided array must fully contain the specified range, but this is not checked.
  --
  -- Warning: this can fail with an unchecked exception.
  cloneM# ∷ M a s x
          → I -- ^ Source offset
          → I -- ^ number of elements to copy
          → ST s (M a s x)


-- | "A.Boxed.Big" - @new#@ initializes undefined. @lenM#@ is safe.
instance Array Array# where
  freeze## = unsafeFreezeArray#
  freeze# = freezeArray#
  thaw## = unsafeThawArray#
  thaw# = thawArray#
  new# n = let e = raise# "A.Boxed.new#: unintialized index" in ST.do
                     na <- newArray# n e; return na
  len = sizeofArray#
  lenM#  = sizeofMutableArray#
  lenM ma = return (lenM# ma)
  clone# = cloneArray#
  cloneM# = cloneMutableArray#

-- | "A.Boxed.Small" - @new#@ initializes undefined. @lenM#@ is safe.
instance Array SmallArray# where
  freeze## = unsafeFreezeSmallArray#
  freeze# = freezeSmallArray#
  thaw## = unsafeThawSmallArray#
  thaw# = thawSmallArray#
  new# n = let ~e = raise# "A.Boxed.Small.new#: unintialized index" in newSmallArray# n e
  len = sizeofSmallArray#
  lenM#  = sizeofSmallMutableArray#
  lenM ma = return (lenM# ma)
  clone# = cloneSmallArray#
  cloneM# = cloneSmallMutableArray#

-- | "A.Prim" -
-- 'thaw##' is just a cast.
-- @new#@ unpinned w/ init size in bytes.
instance Array UnboxedArray# where
  freeze## = coerce unsafeFreezeByteArray#
  freeze# a off n = ST.do ma <- cloneM# a off n; freeze## ma
  thaw## a = return (unsafeCoerce# a)
  thaw# a off n = ST.do
    ma <- new# n
    cast (copyByteArray# (coerce a) off (coerce ma) 0# n)
    return ma
  new# = coerce newByteArray#
  len = coerce sizeofByteArray#
  lenM# = coerce sizeofMutableByteArray#
  lenM  = coerce getSizeofMutableByteArray#
  cloneM# a off n = ST.do
    ma <- new# n
    cast (copyMutableByteArray# (coerce a) off (coerce ma) 0# n)
    return ma
  clone# a off n = runST (ST.do ma <- thaw# a off n; freeze## ma)

-- deriving via Bytes instance Array (UnboxedArray# x)

instance Array PinnedArray# where
  freeze## = coerce unsafeFreezeByteArray#
  freeze# a off n = ST.do ma <- cloneM# a off n; freeze## ma
  thaw## a = return (unsafeCoerce# a)
  thaw# a off n = ST.do
    ma <- new# n
    cast (copyByteArray# (coerce a) off (coerce ma) 0# n)
    return ma
  new# = coerce newPinnedByteArray#
  len = coerce sizeofByteArray#
  lenM# = coerce sizeofMutableByteArray#
  lenM  = coerce getSizeofMutableByteArray#
  cloneM# a off n = ST.do
    ma <- new# n
    cast (copyMutableByteArray# (coerce a) off (coerce ma) 0# n)
    return ma
  clone# a off n = runST (ST.do ma <- thaw# a off n; freeze## ma)
