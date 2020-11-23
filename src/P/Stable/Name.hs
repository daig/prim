module P.Stable.Name where

type P = StableName#

new ∷ a → IO# (P a)
new = makeStableName#
(≡), eq ∷ P a → P a → B#
(≡) = eqStableName#; eq = eqStableName#
toI ∷ P a → I
toI = stableNameToInt#
