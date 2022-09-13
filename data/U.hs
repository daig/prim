{-# language BangPatterns #-}
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
import Prim as X (Prim(..))


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

-- | (h,l) <- a + (hb,lb)
add12 ∷ U → (# U, U #) → (# U, U #)
{-# INLINABLE add12 #-}
add12 a0 (# b1,b0 #) = (# m1, m0 #)
   where
      !(# t, m0 #) = plusWord2# a0 b0
      !m1          = plusWord# t b1


-- | Add 3 values together
add3# :: U -> U -> U -> (# U, U #)
{-# INLINABLE add3# #-}
add3# a b c = (# r1, r0 #)
   where
      !(# t1, t0 #) = plusWord2# a b
      !(# t2, r0 #) = plusWord2# t0 c
      !r1           = plusWord# t1 t2

-- | Takes high word of dividend, then low word of dividend, then divisor. Requires that high word < divisor.
--
-- Warning: this can fail with an unchecked exception.
divMod2# ∷ U → U → U → (# U, U #)
divMod2# = quotRemWord2#

-- | 2-by-1 large division
--
-- Requires:
--    b0 /= 0
--    a1 >= b0 (not required, but if not q1=0)
divMod3# :: (# U,U #) -> U -> (# (# U,U #),U #)
divMod3# (# a1,a0 #) b0 = (# (# q1, q0 #), r0 #)
   where
      !(# q1, r' #) = a1 /% b0
      !(# q0, r0 #) = divMod2# r' a0 b0
