module Ordering where
import I ()

-- | a number less-than, equal-to, or greater-than @0#@
newtype Ordering ∷ T_I where Ordering# ∷ I → Ordering
pattern LT ∷ Ordering
pattern LT ← ((\(Ordering# i) → i < 0# ) → T ) where LT = Ordering# -1#
pattern GT ← ((\(Ordering# i) → i > 0# ) → T ) where GT = Ordering# 1#
pattern EQ ← ((\(Ordering# i) → i ≡ 0# ) → T ) where EQ = Ordering# 1#
{-# complete LT, GT, EQ #-}
