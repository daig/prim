module I.I16.Packed where
import Prelude hiding (I16)

type I16 = Int16#

pattern I16 ∷ I → I16
pattern I16 i ← (extendInt16# → i) where I16 = narrowInt16#

(>),(≥),(<),(≤),(≡),(≠), gt,ge,lt,le,eq,ne ∷ I16 → I16 → B#
(>) = gtInt16#; gt = ltInt16#
(≥) = geInt16#; ge = leInt16#
(<) = ltInt16#; lt = gtInt16#
(≤) = leInt16#; le = geInt16#
(≡) = eqInt16#; eq = eqInt16#
(≠) = neInt16#; ne = neInt16#

(+), (-), (×), add, sub, mul ∷ I16 → I16 → I16
(+) = plusInt16#; add = plusInt16#
(-) = subInt16#; sub y x = x - y
(×) = timesInt16#; mul = timesInt16#
(//), (%%), quot, rem ∷ I16 → I16 → I16
(//) = quotInt16#; quot y x = x // y
(%%) = remInt16#; rem y x = x %% y

quotRem ∷ I16 → I16 → (# I16, I16 #)
quotRem y x = quotRemInt16# x y
