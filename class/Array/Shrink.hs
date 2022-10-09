{-# language InstanceSigs #-}
module Array.Shrink where
import Array
import Prim

type Shrink ∷ ∀ {r}. (T r → T_) → TC
class Shrink a where shrink ∷ Elt a x ⇒ M a s x → I → ST_ s

instance Shrink SmallArray# where shrink = shrinkSmallMutableArray#
instance Shrink A' where
  shrink ∷ ∀ x s. Elt A' x ⇒ M A' s x → I → ST_ s
  shrink a (size @x → n) = coerce shrinkMutableByteArray# a n
instance Shrink A'_ where
  shrink ∷ ∀ x s. Elt A'_ x ⇒ M A'_ s x → I → ST_ s
  shrink a (size @x → n) = coerce shrinkMutableByteArray# a n
