module U16.Packed where
import Prelude hiding (U16)
import B ()

type U16 = Word16#

pattern U16 ∷ U → U16
pattern U16 i ← (extendWord16# → i) where U16 = narrowWord16#

instance (≤) U16 where (>) = coerce gtWord16#; (≥) = coerce geWord16#
                       (<) = coerce ltWord16#; (≤) = coerce leWord16#
instance (≡) U16 where (≡) = coerce eqWord16#; (≠) = coerce neWord16#
instance ℕ U16 where
  (+) = plusWord16#; (×) = timesWord16#
  (/) = quotWord16#
  (%) = remWord16#
  (/%) = quotRemWord16#
  addC a b = let c = a + b in (# c , c < a ∧ c < b #)
  subC a b = let c = a U16.Packed.- b in (# c , c > a ∧ c > b #)

-- | Unsigned modular subtraction.
(-) ∷ U16 → U16 → U16
(-) = subWord16#
