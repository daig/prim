module I16.Packed where
import Prelude hiding (I16)
import B

type I16 = Int16#

pattern I16 ∷ I → I16
pattern I16 i ← (extendInt16# → i) where I16 = narrowInt16#

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
  x /% y = case I16 0# < x ∧ I16 0# > y of
    T → case (x - I16 1# ) //%% y of (# q, r #) → (# q - I16 1#, r + y + I16 1# #)
    F → case I16 0# > x ∧ I16 0# < y of
      T → case (x + I16 1# ) //%% y of (# q, r #) → (# q - I16 1#, r + y + I16 1# #)
      F → x //%% y
instance ℤ I16 where
  negate = negateInt16#
  (-) = subInt16#
  (//) = quotInt16#
  (%%) = remInt16#
  (//%%) = quotRemInt16#
