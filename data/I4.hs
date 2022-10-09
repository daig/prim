--------------------------------------------------------------------
-- | Description : 32-Bit Signed Integers
--------------------------------------------------------------------
module I4 (I4,Int32#
          -- * Instance reexports
          , module X
          ) where
import Cmp as X
import Num as X
import Cast as X
import Prim as X

-- | Logical right shift. Prefer 'U' for this behavior.
-- Result undefined if shift amount is not in the range [0, word @size - 1@].
shiftRL# ∷ I4 → U → I4
shiftRL# w i = uncheckedShiftRLInt32# w (cast @I i); {-# inline shiftRL# #-}
-- | Logical right shift. Prefer 'U' for this behavior.
-- Result 0 if shift amount is not in the range [0, word @size - 1@].
shiftRL ∷ I4 → U → I4
shiftRL w i = if i >= 4## then cast 0# else shiftRL# w i
{-# inline shiftRL #-}
