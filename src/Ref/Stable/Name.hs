module Ref.Stable.Name where

type Ref = StableName#

new ∷ a → IO (Ref a)
new = makeStableName#
(≡), eq ∷ Ref a → Ref a → B#
(≡) = eqStableName#; eq = eqStableName#
toI ∷ Ref a → I
toI = stableNameToInt#
