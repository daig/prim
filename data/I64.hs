--------------------------------------------------------------------
-- | Description : 64-Bit Signed Integers
--------------------------------------------------------------------
module I64 (I64,Int64#
          -- * Instance reexports
          , module X
          ) where
import Cmp as X
import Num as X
import Cast as X
import Prim as X

-- | Logical right shift. Prefer 'U' for this behavior.
-- Result undefined if shift amount is not in the range [0, word @size - 1@].
shiftRL# ∷ I64 → U → I64
shiftRL# w i = uncheckedIShiftRL64# w (cast @I i); {-# inline shiftRL# #-}
-- | Logical right shift. Prefer 'U' for this behavior.
-- Result 0 if shift amount is not in the range [0, word @size - 1@].
shiftRL ∷ I64 → U → I64
shiftRL w i = case i ≥ 64## of {B# 1# → cast 0#; B# 0# → shiftRL# w i}
{-# inline shiftRL #-}
