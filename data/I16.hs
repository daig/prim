--------------------------------------------------------------------
-- | Description : 16-Bit Signed Integers
--------------------------------------------------------------------
module I16 (I16,Int16#
          -- * Instance reexports
          , module X
          ) where
import Cmp as X
import Num as X
import Cast as X
import Prim as X

-- | Logical right shift. Prefer 'U' for this behavior.
-- Result undefined if shift amount is not in the range [0, word @size - 1@].
shiftRL# ∷ I16 → U → I16
shiftRL# w i = uncheckedShiftRLInt16# w (cast @I i); {-# inline shiftRL# #-}
-- | Logical right shift. Prefer 'U' for this behavior.
-- Result 0 if shift amount is not in the range [0, word @size - 1@].
shiftRL ∷ I16 → U → I16
shiftRL w i = if i ≥ 16## then cast 0# else shiftRL# w i
{-# inline shiftRL #-}
