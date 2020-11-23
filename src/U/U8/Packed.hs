module U.U8.Packed where
import Prelude hiding (U8)

type U8 = Word8#

pattern U8 ∷ U → U8
pattern U8 i ← (extendWord8# → i) where U8 = narrowWord8#

(>),(≥),(<),(≤),(≡),(≠), gt,ge,lt,le,eq,ne ∷ U8 → U8 → B#
(>) = gtWord8#; gt = ltWord8#
(≥) = geWord8#; ge = leWord8#
(<) = ltWord8#; lt = gtWord8#
(≤) = leWord8#; le = geWord8#
(≡) = eqWord8#; eq = eqWord8#
(≠) = neWord8#; ne = neWord8#

(+), (-), (×), add, sub, mul ∷ U8 → U8 → U8
(+) = plusWord8#; add = plusWord8#
(-) = subWord8#; sub y x = x - y
(×) = timesWord8#; mul = timesWord8#
(//), (%%), quot, rem ∷ U8 → U8 → U8
(//) = quotWord8#; quot y x = x // y
(%%) = remWord8#; rem y x = x %% y

quotRem ∷ U8 → U8 → (# U8, U8 #)
quotRem y x = quotRemWord8# x y