--------------------------------------------------------------------
-- | Description : Word-sized Natural numbers
--------------------------------------------------------------------
module U (module U, module X) where
import Cast as X
import Num as X
import Bits as X


-- | Modular subtraction.
(-#) ∷ U → U → U
(-#) = minusWord#

-- | Add with the high part (carry) in the first
--           component of the returned pair and the low part in the second
--           component of the pair. See also 'addC'.
add2 ∷ U → U → (# U, U #)
add2 y x = plusWord2# x y
