module P.Stable where

type P = StablePtr#

new ∷ a → IO# (P a)
new = makeStablePtr#

deref ∷ P a → IO# a
deref = deRefStablePtr#

instance (≡) (P a) where p ≡ q= coerce (eqStablePtr# p q)
