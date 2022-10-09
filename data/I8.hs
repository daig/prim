--------------------------------------------------------------------
-- | Description : 64-Bit Signed Integers
--------------------------------------------------------------------
module I8 (I8,Int64#
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
shiftRL# w i = uncheckedIShiftRL64# w (cast @I i); {-# inline shiftRL# #-}
-- | Logical right shift. Prefer 'U' for this behavior.
-- Result 0 if shift amount is not in the range [0, word @size - 1@].
shiftRL ∷ I8 → U → I8
shiftRL w i = if i >= 8## then cast 0# else shiftRL# w i
{-# inline shiftRL #-}
