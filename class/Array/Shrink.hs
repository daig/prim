{-# language InstanceSigs #-}
module Array.Shrink where
import Array
import Prim

type Shrink ∷ ∀ {r}. (T r → T_A) → TC
class Shrink a where shrink ∷ Elt a x ⇒ M a s x → I → ST_ s

instance Shrink SmallArray# where shrink = shrinkSmallMutableArray#
instance Shrink A_ where
  shrink ∷ ∀ x s. Elt A_ x ⇒ M A_ s x → I → ST_ s
  shrink a (size @x → n) = coerce shrinkMutableByteArray# a n
instance Shrink Pinned_ where
  shrink ∷ ∀ x s. Elt Pinned_ x ⇒ M Pinned_ s x → I → ST_ s
  shrink a (size @x → n) = coerce shrinkMutableByteArray# a n
