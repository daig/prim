--------------------------------------------------------------------
-- | Description : The Empty Type
--------------------------------------------------------------------
{-# language NoImplicitPrelude #-}
module Prim.Void where

-- | The Actually Uninhabited Type (unlike lifted @X@, which contains bottom).
-- GHC cannot currently recognize it as empty, so it must be handled in case
-- matches by 'absurd', rather than the empty pattern.
newtype X ∷ T ('SumRep '[]) where X ∷ X ⊸ X


-- | The form quality of Emptiness
absurd ∷ ∀ r (a ∷ T_ r). X → a
absurd (X v) = absurd v
