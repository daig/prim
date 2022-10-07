module Num where
import {-# source #-} Cmp


class (≤) a ⇒ Num# (a ∷ T r) where
  (+), (×), (-) ∷ a → a → a
  (-?) ∷ a → a → (# (##) | a #)
  (-??) ∷ a → a → (# a | a #)
  (/), (%) ∷ a → a → a
  (/%) ∷ a → a → (# a , a #)
class Num# a ⇒ Integral# (a ∷ T r) where
  (//),(%%) ∷ a → a → a
  (//%%) ∷ a → a → (# a , a #)
  negate, abs ∷ a → a
  sgn ∷ a → Ordering
class 𝕌 (a ∷ T r) where
  log2 ∷ a → a
  log# ∷ a → a → a
  gcd, lcm ∷ a → a → a
class Integral# a ⇒ Floating# (a ∷ T r) where
  exp,log,sqrt,sin,cos,tan,asin,acos,atan,sinh,cosh,tanh ∷ a → a
  expm1 ∷ a → a
  log1p ∷ a → a
  (**) ∷ a → a → a

instance Num# U where
instance Num# I where
instance Integral# I where
