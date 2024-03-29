module Prim.I16 where
import Prelude hiding (I16)
import Prim.B

type I16 = Int16#

pattern I16 ∷ I → I16
pattern I16 i ← (int16ToInt# → i) where I16 = intToInt16#

instance (≤) I16 where
  (>) = coerce gtInt16#
  (≥) = coerce geInt16#
  (<) = coerce ltInt16#
  (≤) = coerce leInt16#
instance (≡) I16 where
  (≡) = coerce eqInt16#
  (≠) = coerce neInt16#
instance ℕ I16 where
  (+) = plusInt16#; (×) = timesInt16#
  x / y = case I16 0# < x ∧ I16 0# > y of
    T → (x - I16 1# ) // y - I16 1#
    F → case I16 0# > x ∧ I16 0# < y of
      T → (x + I16 1# ) // y - I16 1#
      F → x // y
  x % y = case I16 0# < x ∧ I16 0# > y of
    T → (x - I16 1# ) %% y + y + I16 1#
    F → case I16 0# > x ∧ I16 0# < y of
      T → (x + I16 1# ) %% y + y + I16 1#
      F → x %% y
  x /% y = case I16 0# < x ∧ I16 0# > y of
    T → case (x - I16 1# ) //%% y of (# q, r #) → (# q - I16 1#, r + y + I16 1# #)
    F → case I16 0# > x ∧ I16 0# < y of
      T → case (x + I16 1# ) //%% y of (# q, r #) → (# q - I16 1#, r + y + I16 1# #)
      F → x //%% y
  addC a b = let c = a + b in (# c , c < a ∧ c < b #)
  subC a b = let c = a - b in (# c , c > a ∧ c > b #)
instance ℤ I16 where
  negate = negateInt16#
  (-) = subInt16#
  (//) = quotInt16#
  (%%) = remInt16#
  (//%%) = quotRemInt16#
