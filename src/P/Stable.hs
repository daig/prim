module P.Stable where
import Prelude hiding (P)
import B ()

type P = StablePtr#

new ∷ a → IO# (P a)
new = makeStablePtr#

deref ∷ P a → IO# a
deref = deRefStablePtr#

instance (≡) (P a) where
  p ≡ q = B# (eqStablePtr# p q)
  p ≠ q = (¬) (p ≡ q)
