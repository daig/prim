module Prim.I32 where
import Prelude hiding (I32)
import Prim.B

type I32 = Int32#

pattern I32 ∷ I → I32
pattern I32 i ← (int32ToInt# → i) where I32 = intToInt32#

instance (≤) I32 where
  (>) = coerce gtInt32#
  (≥) = coerce geInt32#
  (<) = coerce ltInt32#
  (≤) = coerce leInt32#
instance (≡) I32 where
  (≡) = coerce eqInt32#
  (≠) = coerce neInt32#
instance ℕ I32 where
  (+) = plusInt32#; (×) = timesInt32#
  x / y = case I32 0# < x ∧ I32 0# > y of
    T → (x - I32 1# ) // y - I32 1#
    F → case I32 0# > x ∧ I32 0# < y of
      T → (x + I32 1# ) // y - I32 1#
      F → x // y
  x % y = case I32 0# < x ∧ I32 0# > y of
    T → (x - I32 1# ) %% y + y + I32 1#
    F → case I32 0# > x ∧ I32 0# < y of
      T → (x + I32 1# ) %% y + y + I32 1#
      F → x %% y
  x /% y = case I32 0# < x ∧ I32 0# > y of
    T → case (x - I32 1# ) //%% y of (# q, r #) → (# q - I32 1#, r + y + I32 1# #)
    F → case I32 0# > x ∧ I32 0# < y of
      T → case (x + I32 1# ) //%% y of (# q, r #) → (# q - I32 1#, r + y + I32 1# #)
      F → x //%% y
  addC a b = let c = a + b in (# c , c < a ∧ c < b #)
  subC a b = let c = a - b in (# c , c > a ∧ c > b #)
instance ℤ I32 where
  negate = negateInt32#
  (-) = subInt32#
  (//) = quotInt32#
  (%%) = remInt32#
  (//%%) = quotRemInt32#
