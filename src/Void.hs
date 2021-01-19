{-# language NoImplicitPrelude #-}
module Void where
import T

newtype Void ∷ T_ ('SumRep '[]) where
  Void ∷ Void → Void

absurd ∷ ∀ r (a ∷ T_ r). Void → a
absurd (Void v) = absurd v
