{-# language InstanceSigs, CPP #-}
module Array where
import Do.ST as ST
import Cast
import Prim
import Num

type New# ∷ ∀ {rx}. (T rx → T_) → Constraint
class New# a where
  -- | Create a new _uninitizlized_ array
  new# ∷ Elt a x ⇒ I {-^ size in elements -} → ST s (M a s x)
  
type Array ∷ ∀ {rx}. (T rx → T_) → Constraint
class New# a ⇒ Array a where
  -- | Make a mutable array immutable, without copying.
  freeze## ∷ M a s x → ST s (a x)
  -- | Make an immutable array mutable, without copying.
  thaw## ∷ a x → ST s (M a s x)
  -- | Copy an immutable array into a new mutable one.
  thaw# ∷  Elt a x ⇒ a x
          → I -- ^ Source offset
          → I -- ^ number of elements to copy
          → ST s (M a s x)
  -- | Create a new immutable array from a mutable by copying
  freeze# ∷ Elt a x ⇒ M a s x
          → I -- ^ Source offset
          → I -- ^ number of elements to copy
          → ST s (a x)
  -- | Number of elements
  len ∷ Elt a x ⇒ a x → I
  -- | Like 'len' for mutable arrays. Only safe in the absence of resizes
  lenM# ∷ Elt a x ⇒ M a s x → I
  -- | Like 'len' for mutable arrays.
  lenM ∷ Elt a x ⇒ M a s x → ST s I
  -- | Create a new array with the elements from the source array.
  -- The provided array must fully contain the specified range, but this is not checked.
  --
  -- Warning: this can fail with an unchecked exception.
  clone# ∷ Elt a x ⇒ a x
         → I -- ^ Source offset
         → I -- ^ number of elements to copy
         → a x
  -- | Create a new array with the elements from the source array.
  -- The provided array must fully contain the specified range, but this is not checked.
  --
  -- Warning: this can fail with an unchecked exception.
  cloneM# ∷ Elt a x ⇒ M a s x
          → I -- ^ Source offset
          → I -- ^ number of elements to copy
          → ST s (M a s x)

instance New# (Array# ∷ T_ → T_) where
  new# = (`newArray#` raise# "Array#.new#: unintialized index")
instance New# (Array# ∷ ★ → T_) where
  new# = (`newArray#` raise# "Array#.new#: unintialized index")

-- | "A.Boxed.Big" - @new#@ initializes undefined. @lenM#@ is safe.
instance Array (Array# ∷ T_ → T_) where
  freeze## = unsafeFreezeArray#
  freeze# = freezeArray#
  thaw## = unsafeThawArray#
  thaw# = thawArray#
  len = sizeofArray#
  lenM#  = sizeofMutableArray#
  lenM ma = return (lenM# ma)
  clone# = cloneArray#
  cloneM# = cloneMutableArray#
-- | "A.Boxed.Big" - @new#@ initializes undefined. @lenM#@ is safe.
instance Array (Array# ∷ ★ → T_) where
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
instance New# (SmallArray# ∷ ★ → T_) where new# = (`newSmallArray#` raise# "SmallArray#.new#: unintialized index")
instance Array (SmallArray# ∷ ★ → T_) where
  freeze## = unsafeFreezeSmallArray#
  freeze# = freezeSmallArray#
  thaw## = unsafeThawSmallArray#
  thaw# = thawSmallArray#
  len = sizeofSmallArray#
  lenM#  = sizeofSmallMutableArray#
  lenM ma = return (lenM# ma)
  clone# = cloneSmallArray#
  cloneM# = cloneSmallMutableArray#

-- | "A.Boxed.Small" - @new#@ initializes undefined. @lenM#@ is safe.
instance New# (SmallArray# ∷ T_ → T_) where new# = (`newSmallArray#` raise# "SmallArray#.new#: unintialized index")
instance Array (SmallArray# ∷ T_ → T_) where
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
instance New# (UnboxedArray# ∷ T r  → T_) where
  new# ∷ ∀ (x ∷ T r) s. Elt UnboxedArray# x ⇒ I → ST s (M UnboxedArray# s x)
  new# (size @x → n) = coerce newByteArray# n


-- 'thaw##' is just a cast.

#define INST_ARRAY_UB(A)\
instance Array (UnboxedArray# ∷ K A → T_) where { ;\
  freeze## = coerce unsafeFreezeByteArray# ;\
  freeze# ∷ ∀ (x ∷ K A) s. Elt UnboxedArray# x ⇒ M UnboxedArray# s x → I → I → ST s (UnboxedArray# x) ;\
  freeze# a (size @x → off) (size @x → n) = ST.do {ma <- cloneM# a off n; freeze## ma} ;\
  thaw## a = return (unsafeCoerce# a) ;\
  thaw# ∷ ∀ (x ∷ K A) s. Elt UnboxedArray# x ⇒ UnboxedArray# x → I → I → ST s (M UnboxedArray# s x) ;\
  thaw# a (size @x → off) (size @x → n) = ST.do { ;\
    ma <- new# n ;\
    copyByteArray# (coerce a) off (coerce ma) 0# n ;\
    return ma} ;\
  len ∷ ∀ (x ∷ K A). Elt UnboxedArray# x ⇒ UnboxedArray# x → I ;\
  len a = coerce sizeofByteArray# a / size @x 1# ;\
  lenM# ∷ ∀ (x ∷ K A) s. Elt UnboxedArray# x ⇒ M UnboxedArray# s x → I ;\
  lenM# a = coerce sizeofMutableByteArray# a / size @x 1# ;\
  lenM ∷ ∀ (x ∷ K A) s. Elt UnboxedArray# x ⇒ M UnboxedArray# s x → ST s I ;\
  lenM a = ST.do {i ← coerce getSizeofMutableByteArray# a; return (i / size @x 1#)} ;\
  cloneM# ∷ ∀ (x ∷ K A) s. Elt UnboxedArray# x ⇒ M UnboxedArray# s x → I → I → ST s (M UnboxedArray# s x) ;\
  cloneM# a (size @x → off) (size @x → n) = ST.do { ;\
    ma <- new# n ;\
    copyMutableByteArray# (coerce a) off (coerce ma) 0# n ;\
    return ma} ;\
  clone# ∷ ∀ (x ∷ K A). Elt UnboxedArray# x ⇒ UnboxedArray# x → I → I → UnboxedArray# x ;\
  clone# a (size @x → off) (size @x → n) = runST (ST.do {ma <- thaw# a off n; freeze## ma})}  ;\
deriving via (UnboxedArray# ∷ K A → T_) instance Array (PinnedArray# ∷ K A → T_) ;\
instance New# (PinnedArray# ∷ K A → T_) where { ;\
  new# ∷ ∀ {r} (x ∷ T r) s. Elt PinnedArray# x ⇒ I → ST s (M PinnedArray# s x) ;\
  new# (size @x → n) = coerce newPinnedByteArray# n }

INST_ARRAY_UB(I)
INST_ARRAY_UB(I8)
INST_ARRAY_UB(I16)
INST_ARRAY_UB(I32)
INST_ARRAY_UB(I64)
INST_ARRAY_UB(U)
INST_ARRAY_UB(U8)
INST_ARRAY_UB(U16)
INST_ARRAY_UB(U32)
INST_ARRAY_UB(U64)
INST_ARRAY_UB(F32)
INST_ARRAY_UB(F64)
INST_ARRAY_UB(Addr#)
