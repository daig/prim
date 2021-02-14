--------------------------------------------------------------------
-- | Description : Primitive boolean type
--------------------------------------------------------------------
{-# language NoImplicitPrelude #-}
module Prim.B (B(B,F,T), module X) where
import {-# source #-} Prim.I as X
import Class.Prim.Cmp
import Class.Prim.Bits

newtype B ∷ T_I where B ∷ I → B
pattern F ∷ B
pattern F = B 0#
pattern T ∷ B
pattern T = B 1#
{-# complete F, T #-}

deriving newtype instance (≡) B
deriving newtype instance (≤) B
--pattern B ∷ I → B
--pattern B i ← B i where B i = 0# < i
--pattern I1 ∷ I → B
--pattern I1 i ← B i where I1 i = B (andI# 0# i)
