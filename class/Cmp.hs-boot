module Cmp where

infix 4 >, ≥, <, ≤, =#, ≠, `cmp`
class (≡) (a ∷ T r) where
  (≡), (≠) ∷ a → a → B
  (=#), (≠#) ∷ a → a → B#
class (≡) a ⇒ (≤) (a ∷ T r) where
  (>),(≥),(<),(≤) ∷ a → a → B
  (>#),(≥#),(<#),(≤#) ∷ a → a → B#
  cmp ∷ a → a → Ordering
  min,max ∷ a → a → a
