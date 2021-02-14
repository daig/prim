module P where
type P# = Addr#
newtype Ref# (x ∷ T_ r) ∷ T_P where P# ∷ ∀ r (x ∷ T_ r). P# → Ref# x
