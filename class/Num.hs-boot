{-# language LinearTypes #-}
module Num where
import {-# source #-} Cmp

-- |Satisfies @((((x / y) × y) + (x % y) ≡ x@. The
class (≤) a ⇒ ℕ (a ∷ T r) where
  (+), (×) ∷ a ⊸ a ⊸ a
  -- | Rounds towards -∞. The behavior is undefined if the first argument is zero.
  (/), (%) ∷ a {- ^ dividend -}  ⊸ a {- ^ divisor -} ⊸ a
  (/%) ∷ a ⊸ a ⊸ (# a , a #)
  -- |Add reporting overflow.
  addC ∷ a ⊸ a ⊸ (# a, B #) -- ^ The truncated sum and whether it overflowed
  -- |Subtract reporting overflow
  subC ∷ a ⊸ a ⊸ (# a, B #) -- ^ The truncated subtraction and whether it underflowed
  min ∷ (##) ⊸ a
  max ∷ (##) ⊸ a
class ℕ a ⇒ ℤ (a ∷ T r) where
  -- |Satisfies @((((x // y) × y) + (x %% y) ≡ x@.
  (//),(%%) ∷ a ⊸ a ⊸ a
  -- | Rounds towards 0. The behavior is undefined if the first argument is zero.
  (//%%) ∷ a ⊸ a ⊸ (# a , a #)
  (-) ∷ a ⊸ a ⊸ a
  negate ∷ a ⊸ a
class ℤ a ⇒ ℝ (a ∷ T r) where
  abs,exp,log,sqrt,sin,cos,tan,asin,acos,atan,sinh,cosh,tanh ∷ a ⊸ a
  (**) ∷ a ⊸ a ⊸ a

instance ℕ U where
instance ℕ I where
instance ℤ I where
