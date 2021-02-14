{-# language LinearTypes #-}
module P where
import {-# source #-} I (I)
import Cmp as X

-- | An arbitrary machine address assumed to point outside the garbage-collected heap
type P# = Addr#

-- | hack to expose nullAddr#
pattern Null ∷ P#
pattern Null ← nullAddr# where Null = nullAddr#

-- |Advances the given address by the given offset in bytes.
(∔) ∷ I → P# ⊸ P#
i ∔ a = unsafeCoerce# plusAddr# a i

-- |Computes the offset required to get from the second to the first argument.
(߸) ∷ P# → P# → I
(߸) = minusAddr#

(.//) ∷ P# → I → I
(.//) = remAddr#

pattern Addr# ∷ I → P#
pattern Addr# i ← (addr2Int# → i) where Addr# = int2Addr#
{-# DEPRECATED Addr# "This pattern is strongly deprecated" #-}

-- | Immutable raw pointer to a valid memory region containing sum number of @x@
newtype Ref# (x ∷ T_ r) ∷ T_P where P# ∷ ∀ r (x ∷ T_ r). P# ⊸ Ref# x

instance (≤) P# where (>) = unsafeCoerce# gtAddr# ; (≥) = unsafeCoerce# geAddr# ; (<) = unsafeCoerce# ltAddr# ; (≤) = unsafeCoerce# leAddr# ;
instance (≡) P# where (≡) = unsafeCoerce# eqAddr# ; (≠) = unsafeCoerce# neAddr#
