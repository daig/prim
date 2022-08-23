--------------------------------------------------------------------
-- | Description : Primitive boolean type
--------------------------------------------------------------------
{-# language NoImplicitPrelude #-}
module Prim.B (B(B#,F,T), module X) where
import Types as X (I,B(B#), pattern F,pattern T)
import Cmp
--import Bits

--pattern I1 ∷ I → B
--pattern I1 i ← B# i where I1 i = B# (andI# 0# i)
