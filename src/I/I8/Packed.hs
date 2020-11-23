module I.I8.Packed where
import Prelude hiding (I8)

type I8 = Int8#

pattern I8 ∷ I → I8
pattern I8 i ← (extendInt8# → i) where I8 = narrowInt8#

(>),(≥),(<),(≤),(≡),(≠), gt,ge,lt,le,eq,ne ∷ I8 → I8 → B#
(>) = gtInt8#; gt = ltInt8#
(≥) = geInt8#; ge = leInt8#
(<) = ltInt8#; lt = gtInt8#
(≤) = leInt8#; le = geInt8#
(≡) = eqInt8#; eq = eqInt8#
(≠) = neInt8#; ne = neInt8#

(+), (-), (×), add, sub, mul ∷ I8 → I8 → I8
(+) = plusInt8#; add = plusInt8#
(-) = subInt8#; sub y x = x - y
(×) = timesInt8#; mul = timesInt8#
(//), (%%), quot, rem ∷ I8 → I8 → I8
(//) = quotInt8#; quot y x = x // y
(%%) = remInt8#; rem y x = x %% y

quotRem ∷ I8 → I8 → (# I8, I8 #)
quotRem y x = quotRemInt8# x y