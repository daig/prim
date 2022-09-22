{-# language InstanceSigs #-}
module Array where
import Do.ST as ST
import Cast
import Prim
import Num
  
type New ∷ ∀ {rx}. (T rx → T_) → Constraint
class New a where
  -- | Uninitialized array.
  new# ∷ a ∋ x ⇒ I {-^ size in elements -} → ST s (M a s x)

type Array ∷ ∀ {rx}. (T rx → T_) → Constraint
class New a ⇒ Array a where
  -- | Make a mutable array immutable, without copying.
  freeze## ∷ M a s x → ST s (a x)
  -- | Make an immutable array mutable, without copying.
  thaw## ∷ a x → ST s (M a s x)
  -- | Copy an immutable array into a new mutable one.
  thaw# ∷  a ∋ x ⇒ a x
          → I -- ^ Source offset
          → I -- ^ number of elements to copy
          → ST s (M a s x)
  -- | Create a new immutable array from a mutable by copying
  freeze# ∷ a ∋ x ⇒ M a s x
          → I -- ^ Source offset
          → I -- ^ number of elements to copy
          → ST s (a x)
  -- | Number of elements
  len ∷ a ∋ x ⇒ a x → I
  -- | Like 'len' for mutable arrays. Only safe in the absence of resizes
  lenM# ∷ a ∋ x ⇒ M a s x → I
  -- | Like 'len' for mutable arrays.
  lenM ∷ a ∋ x ⇒ M a s x → ST s I
  -- | Create a new array with the elements from the source array.
  -- The provided array must fully contain the specified range, but this is not checked.
  --
  -- Warning: this can fail with an unchecked exception.
  clone# ∷ a ∋ x ⇒ a x
         → I -- ^ Source offset
         → I -- ^ number of elements to copy
         → a x
  -- | Create a new array with the elements from the source array.
  -- The provided array must fully contain the specified range, but this is not checked.
  --
  -- Warning: this can fail with an unchecked exception.
  cloneM# ∷ a ∋ x ⇒ M a s x
          → I -- ^ Source offset
          → I -- ^ number of elements to copy
          → ST s (M a s x)

instance New Array# where
  new# n = let e = raise# "A.Boxed.new#: unintialized index" in ST.do
                     na <- newArray# n e; return na

-- | "A.Boxed.Big" - @new#@ initializes undefined. @lenM#@ is safe.
instance Array Array# where
  freeze## = unsafeFreezeArray#
  freeze# = freezeArray#
  thaw## = unsafeThawArray#
  thaw# = thawArray#
  len = sizeofArray#
  lenM#  = sizeofMutableArray#
  lenM ma = return (lenM# ma)
  clone# = cloneArray#
  cloneM# = cloneMutableArray#

-- | "A.Boxed.Small" - @new#@ initializes undefined. @lenM#@ is safe.
instance New SmallArray# where
  new# n = let ~e = raise# "A.Boxed.Small.new#: unintialized index" in newSmallArray# n e
instance Array SmallArray# where
  freeze## = unsafeFreezeSmallArray#
  freeze# = freezeSmallArray#
  thaw## = unsafeThawSmallArray#
  thaw# = thawSmallArray#
  len = sizeofSmallArray#
  lenM#  = sizeofSmallMutableArray#
  lenM ma = return (lenM# ma)
  clone# = cloneSmallArray#
  cloneM# = cloneSmallMutableArray#

-- | @new#@ unpinned w/ init size in bytes.
instance New UnboxedArray# where
  new# ∷ ∀ x s. UnboxedArray# ∋ x ⇒ I → ST s (M UnboxedArray# s x)
  new# (size @x → n) = coerce newByteArray# n

-- | "A.Prim" -
-- 'thaw##' is just a cast.
instance Array UnboxedArray# where
  freeze## = coerce unsafeFreezeByteArray#
  freeze# ∷ ∀ x s. UnboxedArray# ∋ x ⇒ M UnboxedArray# s x → I → I → ST s (UnboxedArray# x)
  freeze# a (size @x → off) (size @x → n) = ST.do ma <- cloneM# a off n; freeze## ma
  thaw## a = return (unsafeCoerce# a)
  thaw# ∷ ∀ x s. UnboxedArray# ∋ x ⇒ UnboxedArray# x → I → I → ST s (M UnboxedArray# s x)
  thaw# a (size @x → off) (size @x → n) = ST.do
    ma <- new# n
    cast (copyByteArray# (coerce a) off (coerce ma) 0# n)
    return ma
  len ∷ ∀ x. UnboxedArray# ∋ x ⇒ UnboxedArray# x → I
  len a = coerce sizeofByteArray# a / size @x 1#
  lenM# ∷ ∀ x s. UnboxedArray# ∋ x ⇒ M UnboxedArray# s x → I
  lenM# a = coerce sizeofMutableByteArray# a / size @x 1#
  lenM ∷ ∀ x s. UnboxedArray# ∋ x ⇒ M UnboxedArray# s x → ST s I
  lenM a = ST.do i ← coerce getSizeofMutableByteArray# a; return (i / size @x 1#)
  cloneM# ∷ ∀ x s. UnboxedArray# ∋ x ⇒ M UnboxedArray# s x → I → I → ST s (M UnboxedArray# s x)
  cloneM# a (size @x → off) (size @x → n) = ST.do
    ma <- new# n
    cast (copyMutableByteArray# (coerce a) off (coerce ma) 0# n)
    return ma
  clone# ∷ ∀ x. UnboxedArray# ∋ x ⇒ UnboxedArray# x → I → I → UnboxedArray# x
  clone# a (size @x → off) (size @x → n) = runST (ST.do ma <- thaw# a off n; freeze## ma)

instance New PinnedArray# where new# = coerce newPinnedByteArray#
deriving via UnboxedArray# instance Array PinnedArray#
