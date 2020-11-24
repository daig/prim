module I16.Packed where
import Prelude hiding (I16)

type I16 = Int16#

pattern I16 ∷ I → I16
pattern I16 i ← (extendInt16# → i) where I16 = narrowInt16#

instance (≤) I16 where
  (>) = gtInt16#
  (≥) = geInt16#
  (<) = ltInt16#
  (≤) = leInt16#
instance (≡) I16 where
  (≡) = eqInt16#
  (≠) = neInt16#

(+), (-), (×), add, sub, mul ∷ I16 → I16 → I16
(+) = plusInt16#; add = plusInt16#
(-) = subInt16#; sub y x = x - y
(×) = timesInt16#; mul = timesInt16#
(//), (%%), quot, rem ∷ I16 → I16 → I16
(//) = quotInt16#; quot y x = x // y
(%%) = remInt16#; rem y x = x %% y

quotRem ∷ I16 → I16 → (# I16, I16 #)
quotRem y x = quotRemInt16# x y
