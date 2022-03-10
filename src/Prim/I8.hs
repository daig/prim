module Prim.I8 where
import Prelude hiding (I8)
import Prim.B

type I8 = Int8#

pattern I8 ∷ I → I8
pattern I8 i ← (int8ToInt# → i) where I8 = intToInt8#

instance (≤) I8 where
  (>) = coerce gtInt8#
  (≥) = coerce geInt8#
  (<) = coerce ltInt8#
  (≤) = coerce leInt8#
instance (≡) I8 where
  (≡) = coerce eqInt8#
  (≠) = coerce neInt8#
instance ℕ I8 where
  (+) = plusInt8#; (×) = timesInt8#
  x / y = case I8 0# < x ∧ I8 0# > y of
    T → (x - I8 1# ) // y - I8 1#
    F → case I8 0# > x ∧ I8 0# < y of
      T → (x + I8 1# ) // y - I8 1#
      F → x // y
  x % y = case I8 0# < x ∧ I8 0# > y of
    T → (x - I8 1# ) %% y + y + I8 1#
    F → case I8 0# > x ∧ I8 0# < y of
      T → (x + I8 1# ) %% y + y + I8 1#
      F → x %% y
  x /% y = case I8 0# < x ∧ I8 0# > y of
    T → case (x - I8 1# ) //%% y of (# q, r #) → (# q - I8 1#, r + y + I8 1# #)
    F → case I8 0# > x ∧ I8 0# < y of
      T → case (x + I8 1# ) //%% y of (# q, r #) → (# q - I8 1#, r + y + I8 1# #)
      F → x //%% y
  addC a b = let c = a + b in (# c , c < a ∧ c < b #)
  subC a b = let c = a - b in (# c , c > a ∧ c > b #)
instance ℤ I8 where
  negate = negateInt8#
  (-) = subInt8#
  (//) = quotInt8#
  (%%) = remInt8#
  (//%%) = quotRemInt8#
