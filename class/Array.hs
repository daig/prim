module Array where
import Do as Prim

class Array (a ∷ T_) where
  -- | Uninitialized array.
  new# ∷ I {-^ size in elements -} → ST s (M a s)
  -- | Make a mutable array immutable, without copying.
  freeze## ∷ M a s → ST s a
  -- | Make an immutable array mutable, without copying.
  thaw## ∷ a → ST s (M a s)
  -- | Copy an immutable array into a new mutable one.
  thaw# ∷  a
          → I -- ^ Source offset
          → I -- ^ number of elements to copy
          → ST s (M a s)
  -- | Create a new immutable array from a mutable by copying
  freeze# ∷ M a s
          → I -- ^ Source offset
          → I -- ^ number of elements to copy
          → ST s a
  -- | Number of elements
  len ∷ a → I
  -- | Like 'len' for mutable arrays. Only safe in the absence of resizes
  lenM# ∷ M a s → I
  -- | Like 'len' for mutable arrays.
  lenM ∷ M a s → ST s I
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
          → ST s (M a s)


-- | "A.Boxed.Big" - @new#@ initializes undefined. @lenM#@ is safe.
instance Array (A_Box x) where
  freeze## = coerce (unsafeFreezeArray# @_ @x)
  freeze# = coerce (freezeArray# @_ @x)
  thaw## = coerce (unsafeThawArray# @x)
  thaw# = coerce (thawArray# @x)
  new# n = let e = raise# "A.Boxed.new#: unintialized index" in Prim.do
                     na <- newArray# @x n e; return (MutableArray# na)
  len = coerce (sizeofArray# @x)
  lenM#  = coerce (sizeofMutableArray# @_ @x)
  lenM ma = return (lenM# ma)
  clone# = coerce (cloneArray# @x)
  cloneM# = coerce (cloneMutableArray# @_ @x)

-- | "A.Boxed.Small" - @new#@ initializes undefined. @lenM#@ is safe.
instance Array (A_Box_Small x) where
  freeze## = coerce (unsafeFreezeSmallArray# @_ @x)
  freeze# = coerce (freezeSmallArray# @_ @x)
  thaw## = coerce (unsafeThawSmallArray# @x)
  thaw# = coerce (thawSmallArray# @x)
  new# n = let e = raise# "A.Boxed.Small.new#: unintialized index" in Prim.do
                     na <- newSmallArray# @x n e; return (SmallArray_M# na)
  len = coerce (sizeofSmallArray# @x)
  lenM#  = coerce (sizeofSmallMutableArray# @_ @x)
  lenM ma = return (lenM# ma)
  clone# = coerce (cloneSmallArray# @x)
  cloneM# = coerce (cloneSmallMutableArray# @_ @x)

-- | "A.Prim" -
-- 'thaw##' is just a cast.
-- @new#@ unpinned w/ init size in bytes.
instance Array Bytes where
  freeze## = coerce unsafeFreezeByteArray#
  freeze# a off n = Prim.do ma <- cloneM# a off n; freeze## ma
  thaw## a = return (unsafeCoerce# a)
  thaw# a off n = Prim.do
    ma <- new# n
    \s -> (# coerce copyByteArray# a off ma 0# n s, (##) #)
    return ma
  new# = coerce newByteArray#
  len = coerce sizeofByteArray#
  lenM# = coerce sizeofMutableByteArray#
  lenM  = coerce getSizeofMutableByteArray#
  cloneM# a off n = Prim.do
    ma <- new# n
    \s -> (# coerce copyMutableByteArray# a off ma 0# n s, (##) #)
    return ma
  clone# a off n = runST (Prim.do ma <- thaw# a off n; freeze## ma)

deriving via Bytes instance Array (A_Unbox x)

instance Array Bytes_Pinned where
  freeze## = coerce unsafeFreezeByteArray#
  freeze# a off n = Prim.do ma <- cloneM# a off n; freeze## ma
  thaw## a = return (unsafeCoerce# a)
  thaw# a off n = Prim.do
    ma <- new# n
    \s -> (# coerce copyByteArray# a off ma 0# n s, (##) #)
    return ma
  new# = coerce newPinnedByteArray#
  len = coerce sizeofByteArray#
  lenM# = coerce sizeofMutableByteArray#
  lenM  = coerce getSizeofMutableByteArray#
  cloneM# a off n = Prim.do
    ma <- new# n
    \s -> (# coerce copyMutableByteArray# a off ma 0# n s, (##) #)
    return ma
  clone# a off n = runST (Prim.do ma <- thaw# a off n; freeze## ma)





------
-- | Read from the specified index of an immutable array.
-- The result is packaged into an unboxed unary tuple; the result itself is not yet evaluated.
-- Pattern matching on the tuple forces the indexing of the array to happen
-- but does not evaluate the element itself. Evaluating the thunk prevents
-- additional thunks from building up on the heap. Avoiding these thunks, in turn,
-- reduces references to the argument array, allowing it to be garbage collected more promptly.
-- Warning: this can fail with an unchecked exception.
indexLazySmall# :: forall x. A_Box_Small x -> I -> (# x #)
indexLazySmall# = coerce (indexSmallArray# @x)

resize :: A_Unbox_M x s -> I -> ST s (A_Unbox_M x s)
resize = coerce resizeMutableByteArray#
