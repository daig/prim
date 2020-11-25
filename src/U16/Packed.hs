module U16.Packed where
import Prelude hiding (U16)

type U16 = Word16#

pattern U16 ∷ U → U16
pattern U16 i ← (extendWord16# → i) where U16 = narrowWord16#

instance (≤) U16 where (>) = coerce gtWord16#; (≥) = coerce geWord16#
                       (<) = coerce ltWord16#; (≤) = coerce leWord16#
instance (≡) U16 where (≡) = coerce eqWord16#; (≠) = coerce neWord16#

(+), (-), (×), add, sub, mul ∷ U16 → U16 → U16
(+) = plusWord16#; add = plusWord16#
(-) = subWord16#; sub y x = x - y
(×) = timesWord16#; mul = timesWord16#
(//), (%%), quot, rem ∷ U16 → U16 → U16
(//) = quotWord16#; quot y x = x // y
(%%) = remWord16#; rem y x = x %% y

quotRem ∷ U16 → U16 → (# U16, U16 #)
quotRem y x = quotRemWord16# x y
