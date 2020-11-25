module Num where

class (+) (a ∷ T_ r) where (+) ∷ a → a → a
class (×) (a ∷ T_ r) where (×) ∷ a → a → a
class ((+) a, (×) a, (≤) a) ⇒ ℕ (a ∷ T_ r) where
  (/), (%) ∷ a → a → a
  (/%) ∷ a → a → (# a , a #)
class ℕ a ⇒ ℤ (a ∷ T_ r) where
  (//),(%%) ∷ a → a → a
  (//%%) ∷ a → a → (# a , a #)
  (-) ∷ a → a → a
class ℕ a ⇒ ℚ (a ∷ T_ r) where (÷) ∷ a → a → a

