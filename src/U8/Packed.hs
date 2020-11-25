module U8.Packed where
import Prelude hiding (U8)

type U8 = Word8#

pattern U8 ∷ U → U8
pattern U8 i ← (extendWord8# → i) where U8 = narrowWord8#

instance (≤) U8 where (>) = coerce gtWord8#; (≥) = coerce geWord8#
                      (<) = coerce ltWord8#; (≤) = coerce leWord8#
instance (≡) U8 where (≡) = coerce eqWord8#; (≠) = coerce neWord8#

(+), (-), (×), add, sub, mul ∷ U8 → U8 → U8
(+) = plusWord8#; add = plusWord8#
(-) = subWord8#; sub y x = x - y
(×) = timesWord8#; mul = timesWord8#
(//), (%%), quot, rem ∷ U8 → U8 → U8
(//) = quotWord8#; quot y x = x // y
(%%) = remWord8#; rem y x = x %% y

quotRem ∷ U8 → U8 → (# U8, U8 #)
quotRem y x = quotRemWord8# x y
