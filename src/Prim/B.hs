--------------------------------------------------------------------
-- | Description : Primitive boolean type
--------------------------------------------------------------------
{-# language NoImplicitPrelude #-}
module Prim.B (B(B#,B,F,T), module X) where
import Types as X (I,B(B#), pattern F,pattern T)
--import Class.Prim.Cmp
--import Class.Prim.Bits

deriving newtype instance (≡) B
deriving newtype instance (≤) B
pattern B ∷ I → B
pattern B i ← B i where B i = 0# < i
--pattern I1 ∷ I → B
--pattern I1 i ← B i where I1 i = B (andI# 0# i)
