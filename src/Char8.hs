--------------------------------------------------------------------
-- | Description : 8-bit Latin-1 code points
--------------------------------------------------------------------
module Char8 where
import Char

-- | 8-bit Latin-1 code points
newtype Char8 ∷ T_U where Char8# ∷ Char → Char8
