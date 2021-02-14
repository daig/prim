{-# OPTIONS_HADDOCK ignore-exports #-}
--------------------------------------------------------------------
-- | Description : 8-bit Integers
--------------------------------------------------------------------
module I8 (I8(I8#,I8,Min,Max), module X) where
import {-# source #-} I (I)
import Cast as X
import Cmp as X
import Num as X
newtype I8 ∷ T_I where I8#  ∷ I → I8

-- | Narrow a machine 'I' to 8 bits
pattern I8 ∷ I → I8
pattern I8 i ← (coerce → i) where I8 = cast
{-# complete I8 #-}

pattern Max, Min ∷ I8
pattern Max = I8#  0x7FFF#
pattern Min = I8# -0x800#
