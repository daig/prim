--------------------------------------------------------------------
-- | Description : 8-Bit Signed Integers
--------------------------------------------------------------------
module I8 (I8,Int8#
          -- * misc utilities
          ,module I8
          -- * Instance reexports
          , module X
          ) where
import Cmp as X
import Num as X
import Cast as X
import Prim as X

-- | Logical right shift. Prefer 'U' for this behavior.
-- Result undefined if shift amount is not in the range [0, word @size - 1@].
shiftRL# ∷ I8 → U → I8
shiftRL# w i = uncheckedShiftRLInt8# w (cast @I i); {-# inline shiftRL# #-}
-- | Logical right shift. Prefer 'U' for this behavior.
-- Result 0 if shift amount is not in the range [0, word @size - 1@].
shiftRL ∷ I8 → U → I8
shiftRL w i = case i ≥ 8## of {B# 1# → cast 0#; B# 0# → shiftRL# w i}
{-# inline shiftRL #-}
