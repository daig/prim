module U8.Packed where
import Prelude hiding (U8)

type U8 = Word8#

pattern U8 ∷ U → U8
pattern U8 i ← (extendWord8# → i) where U8 = narrowWord8#

instance (≤) U8 where (>) = coerce gtWord8#; (≥) = coerce geWord8#
                      (<) = coerce ltWord8#; (≤) = coerce leWord8#
instance (≡) U8 where (≡) = coerce eqWord8#; (≠) = coerce neWord8#
instance ℕ U8 where
  (+) = plusWord8#; (×) = timesWord8#
  (/) = quotWord8#
  (%) = remWord8#
  (/%) = quotRemWord8#
