module P.Stable.Name where
import Prelude hiding (P)

type P = StableName#

new ∷ a → IO (P a)
new = makeStableName#
instance (≡) (P a) where (≡) = coerce eqStableName#
toI ∷ P a → I
toI = stableNameToInt#
