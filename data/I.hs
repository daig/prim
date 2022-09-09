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


-- |Add reporting overflow.
addC ∷ I → I → (# I, B #) -- ^ The truncated sum and whether it overflowed
addC = coerce addIntC#
-- |Subtract reporting overflow
subC ∷ I → I → (# I, B #) -- ^ The truncated subtraction and whether it underflowed
subC = coerce subIntC#
