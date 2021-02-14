{-# OPTIONS_HADDOCK ignore-exports #-}
--------------------------------------------------------------------
-- | Description : 64-bit Integers 
--------------------------------------------------------------------
module I64 (I64(I64,Min,Max),module X ) where
import {-# source #-} I as X (I)
import {-# source #-} F32
import {-# source #-} F64
import {-# source #-} B
import {-# source #-} U (U)
import Cmp as X
import Num as X
import Cast as X

newtype I64 ∷ T_I where I64 ∷ I → I64

pattern Max, Min ∷ I64
pattern Max =  I64 0x7FFFFFFFFFFFFFFF#
pattern Min = I64 -0x8000000000000000#

deriving newtype instance (≡) I64
deriving newtype instance (≤) I64
deriving newtype instance ℕ I64
deriving newtype instance ℤ I64
deriving newtype instance Cast U I64
deriving newtype instance Cast F32# I64
deriving newtype instance Cast F64# I64
instance Cast I I64 where cast = coerce
