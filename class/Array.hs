{-# language InstanceSigs, CPP #-}
module Array where
import Do.ST as ST
import Cast
import Prim
import Num

type New# ∷ ∀ {rx}. (T rx → T_A) → TC
class New# a where
  -- | Create a new _uninitizlized_ array
  new# ∷ Elt a x ⇒ I {-^ size in elements -} → ST s (M a s x)
  
type Array ∷ ∀ {rx}. (T rx → T_A) → TC
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

instance New# (Array# ∷ T_A → T_A) where
  new# = (`newArray#` raise# "Array#.new#: unintialized index")
instance New# (Array# ∷ ★ → T_A) where
  new# = (`newArray#` raise# "Array#.new#: unintialized index")

-- | "A.Boxed.Big" - @new#@ initializes undefined. @lenM#@ is safe.
instance Array (Array# ∷ T_A → T_A) where
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
instance Array (Array# ∷ ★ → T_A) where
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
instance New# (SmallArray# ∷ ★ → T_A) where new# = (`newSmallArray#` raise# "SmallArray#.new#: unintialized index")
instance Array (SmallArray# ∷ ★ → T_A) where
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
instance New# (SmallArray# ∷ T_A → T_A) where new# = (`newSmallArray#` raise# "SmallArray#.new#: unintialized index")
instance Array (SmallArray# ∷ T_A → T_A) where
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
instance New# (A_ ∷ T r  → T_A) where
  new# ∷ ∀ (x ∷ T r) s. Elt A_ x ⇒ I → ST s (M A_ s x)
  new# (size @x → n) = coerce newByteArray# n


-- 'thaw##' is just a cast.

#define INST_ARRAY_UB(X)\
instance Array (A_ ∷ X → T_A) where { ;\
  freeze## = coerce unsafeFreezeByteArray# ;\
  freeze# ∷ ∀ (x ∷ X) s. Elt A_ x ⇒ M A_ s x → I → I → ST s (A_ x) ;\
  freeze# a (size @x → off) (size @x → n) = ST.do {ma <- cloneM# a off n; freeze## ma} ;\
  thaw## a = return (unsafeCoerce# a) ;\
  thaw# ∷ ∀ (x ∷ X) s. Elt A_ x ⇒ A_ x → I → I → ST s (M A_ s x) ;\
  thaw# a (size @x → off) (size @x → n) = ST.do { ;\
    ma <- new# n ;\
    copyByteArray# (coerce a) off (coerce ma) 0# n ;\
    return ma} ;\
  len ∷ ∀ (x ∷ X). Elt A_ x ⇒ A_ x → I ;\
  len a = coerce sizeofByteArray# a / size @x 1# ;\
  lenM# ∷ ∀ (x ∷ X) s. Elt A_ x ⇒ M A_ s x → I ;\
  lenM# a = coerce sizeofMutableByteArray# a / size @x 1# ;\
  lenM ∷ ∀ (x ∷ X) s. Elt A_ x ⇒ M A_ s x → ST s I ;\
  lenM a = ST.do {i ← coerce getSizeofMutableByteArray# a; return (i / size @x 1#)} ;\
  cloneM# ∷ ∀ (x ∷ X) s. Elt A_ x ⇒ M A_ s x → I → I → ST s (M A_ s x) ;\
  cloneM# a (size @x → off) (size @x → n) = ST.do { ;\
    ma <- new# n ;\
    copyMutableByteArray# (coerce a) off (coerce ma) 0# n ;\
    return ma} ;\
  clone# ∷ ∀ (x ∷ X). Elt A_ x ⇒ A_ x → I → I → A_ x ;\
  clone# a (size @x → off) (size @x → n) = runST (ST.do {ma <- thaw# a off n; freeze## ma})}  ;\
deriving via (A_ ∷ X → T_A) instance Array (Pinned_ ∷ X → T_A) ;\
instance New# (Pinned_ ∷ X → T_A) where { ;\
  new# ∷ ∀ {r} (x ∷ T r) s. Elt Pinned_ x ⇒ I → ST s (M Pinned_ s x) ;\
  new# (size @x → n) = coerce newPinnedByteArray# n }

INST_ARRAY_UB(T_I)
INST_ARRAY_UB(T_I1)
INST_ARRAY_UB(T_I2)
INST_ARRAY_UB(T_I4)
INST_ARRAY_UB(T_I8)
INST_ARRAY_UB(T_U)
INST_ARRAY_UB(T_U1)
INST_ARRAY_UB(T_U2)
INST_ARRAY_UB(T_U4)
INST_ARRAY_UB(T_U8)
INST_ARRAY_UB(T_F4)
INST_ARRAY_UB(T_F8)
INST_ARRAY_UB(T_P)
