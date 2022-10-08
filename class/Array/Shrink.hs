{-# language InstanceSigs #-}
module Array.Shrink where
import Array
import Prim

type Shrink ∷ ∀ {r}. (T r → T_) → TC
class Shrink a where shrink ∷ Elt a x ⇒ M a s x → I → ST_ s

instance Shrink SmallArray# where shrink = shrinkSmallMutableArray#
instance Shrink UnboxedArray# where
  shrink ∷ ∀ x s. Elt UnboxedArray# x ⇒ M UnboxedArray# s x → I → ST_ s
  shrink a (size @x → n) = coerce shrinkMutableByteArray# a n
instance Shrink PinnedArray# where
  shrink ∷ ∀ x s. Elt PinnedArray# x ⇒ M PinnedArray# s x → I → ST_ s
  shrink a (size @x → n) = coerce shrinkMutableByteArray# a n
