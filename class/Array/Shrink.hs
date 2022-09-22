module Array.Shrink where
import Array

type Shrink ∷ ∀ {r}. (T r → T_) → C
class Array a ⇒ Shrink a where shrink ∷ M a s x → I → ST_ s

instance Shrink SmallArray# where shrink = shrinkSmallMutableArray#
instance Shrink UnboxedArray# where shrink = coerce shrinkMutableByteArray#
instance Shrink PinnedArray# where shrink = coerce shrinkMutableByteArray#
