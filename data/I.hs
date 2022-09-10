{-# language CPP #-}
--------------------------------------------------------------------
-- | Description : Machine-sized Signed Integers
--------------------------------------------------------------------
module I (I,Int#
          -- * Misc Utilities
          , module I
          -- * Instances reexports
          , module X
          ) where
import Cmp as X
import Num as X
import Cast as X
import Prim as X
import B as X
#include "MachDeps.h"


-- |Add reporting overflow.
addC ∷ I → I → (# I, B #) -- ^ The truncated sum and whether it overflowed
addC = coerce addIntC#
-- |Subtract reporting overflow
subC ∷ I → I → (# I, B #) -- ^ The truncated subtraction and whether it underflowed
subC = coerce subIntC#

-- | Logical right shift. Prefer 'U' for this behavior.
-- Result undefined if shift amount is not in the range [0, word @size - 1@].
shiftRL# ∷ I → U → I
shiftRL# w i = uncheckedIShiftRL# w (cast @I i); {-# inline shiftRL# #-}
-- | Logical right shift. Prefer 'U' for this behavior.
-- Result 0 if shift amount is not in the range [0, word @size - 1@].
shiftRL ∷ I → U → I
shiftRL w i = case i ≥ WORD_SIZE_IN_BITS## of {B# 1# → 0#; B# 0# → shiftRL# w i}
{-# inline shiftRL #-}
