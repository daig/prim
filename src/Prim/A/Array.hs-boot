module Prim.A.Array where

newtype A (x ∷ T_A) ∷ T_A where A# ∷ Refs → A x
newtype MA s (x ∷ T_A) ∷ T_A where MA# ∷ MRefs s → MA s x
