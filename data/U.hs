--------------------------------------------------------------------
-- | Description : Machine-sized Unsigned Integers
--------------------------------------------------------------------
module U (U,Word#
          -- * misc utilities
          , module U
          -- * Instance reexports
          , module X
          ) where
import Cast as X
import Cmp as X
import Num as X
import Bits as X (Logic(..),Bits(..))
import Prim as X (type (♭)(..))


-- | Modular subtraction.
(-#) ∷ U → U → U
(-#) = minusWord#

-- | Add with the high part (carry) in the first
--           component of the returned pair and the low part in the second
--           component of the pair. See also 'addC'.
add2 ∷ U → U → (# U, U #)
add2 y x = plusWord2# x y

-- |Add reporting overflow.
addC ∷ U → U → (# U, B #) -- ^ The truncated sum and whether it overflowed
addC = coerce addWordC#
-- |Subtract reporting overflow
subC ∷ U → U → (# U, B #) -- ^ The truncated subtraction and whether it underflowed
subC = coerce subWordC#
