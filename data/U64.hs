--------------------------------------------------------------------
-- | Description : 64-bit Unsigned Integers
--------------------------------------------------------------------
module U64 (module U64,module X) where
import {-# source #-} U as X hiding (Max,Min)
import {-# source #-} F32
import {-# source #-} F64
import {-# source #-} B
import {-# source #-} I
import Cmp as X
import Num as X
import Cast as X

newtype U64 ∷ T_U where U64 ∷ U → U64

pattern Max, Min ∷ U64
pattern Max = U64 0xFFFFFFFFFFFFFFFF##
pattern Min = U64 0##

deriving newtype instance (≡) U64
deriving newtype instance (≤) U64
deriving newtype instance ℕ U64
deriving newtype instance Cast I U64
deriving newtype instance Cast F32# U64
deriving newtype instance Cast F64# U64
instance Cast U U64 where cast = coerce
