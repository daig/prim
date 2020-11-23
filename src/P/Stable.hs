module P.Stable where

type P = StablePtr#

new ∷ a → IO# (P a)
new = makeStablePtr#

deref ∷ P a → IO# a
deref = deRefStablePtr#

(≡), eq ∷ P a → P a → B#
(≡) = eqStablePtr#; eq = eqStablePtr#
