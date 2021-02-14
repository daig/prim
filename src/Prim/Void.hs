--------------------------------------------------------------------
-- | Description : The Empty Type
--------------------------------------------------------------------
{-# language NoImplicitPrelude #-}
module Prim.Void where
import Type


-- | The form quality of Emptiness
absurd ∷ ∀ r (a ∷ T_ r). Void → a
absurd (Void v) = absurd v
