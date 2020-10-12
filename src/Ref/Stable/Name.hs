module Ref.Stable.Name where

type Ref = StableName#

new ∷ a → IO (Ref a)
new = makeStableName#
eq ∷ Ref a → Ref a → I1
eq = eqStableName#
toInt ∷ Ref a → I64
toInt = stableNameToInt#
