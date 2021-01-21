--------------------------------------------------------------------
-- | Description : The Empty Type
--------------------------------------------------------------------
{-# language NoImplicitPrelude #-}
module Void where
import T

-- | The Actually Uninhabited Type (unlike lifted @Void@, which contains bottom).
-- GHC cannot currently recognize it as empty, so it must be handled in case
-- matches by 'absurd', rather than the empty pattern.
newtype Void ∷ T_ ('SumRep '[]) where Void ∷ Void → Void

-- | The form quality of Emptiness
absurd ∷ ∀ r (a ∷ T_ r). Void → a
absurd (Void v) = absurd v
