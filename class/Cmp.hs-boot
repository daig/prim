{-# language LinearTypes #-}
module Cmp where

infix 4 >, ≥, <, ≤, ≡, ≠, `cmp`
class (≡) (a ∷ T r) where (≡), (≠) ∷ a ⊸ a ⊸ B
class (≡) a ⇒ (≤) (a ∷ T r) where
  (>),(≥),(<),(≤) ∷ a ⊸ a ⊸ B
  cmp ∷ a ⊸ a ⊸ Ordering
newtype Ordering ∷ K I where Ordering# ∷ I ⊸ Ordering
