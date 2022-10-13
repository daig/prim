--------------------------------------------------------------------
-- | Description : 8-Bit Signed Integers
--------------------------------------------------------------------
module I.I1 (I1,Int8#
          -- * misc utilities
          ,module I.I1
          -- * Instance reexports
          , module X
          ) where
import Cmp as X
import Num as X
import Cast as X
import Prim as X

-- | Logical right shift. Prefer 'U' for this behavior.
-- Result undefined if shift amount is not in the range [0, word @size - 1@].
shiftRL# ∷ I1 → U → I1
shiftRL# w i = uncheckedShiftRLInt8# w (cast @I i); {-# inline shiftRL# #-}
-- | Logical right shift. Prefer 'U' for this behavior.
-- Result 0 if shift amount is not in the range [0, word @size - 1@].
shiftRL ∷ I1 → U → I1
shiftRL w i = if i >= 1## then cast 0# else shiftRL# w i
{-# inline shiftRL #-}
