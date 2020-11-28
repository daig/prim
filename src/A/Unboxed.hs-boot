module A.Unboxed where
newtype A    (x ∷ T_ r) ∷ T_A where A#  ∷ ∀   r (x ∷ T_ r). ByteArray#          → A    x
newtype MA s (x ∷ T_ r) ∷ T_A where MA# ∷ ∀ s r (x ∷ T_ r). MutableByteArray# s → MA s x
