--------------------------------------------------------------------
-- | Description : 64-bit Signed Integer operations
--------------------------------------------------------------------
{-# language CPP #-}
{-# language BangPatterns #-}
module I64 (I64(..), module I64) where
import qualified GHC.Classes as GHC (divInt#,modInt#)
import qualified I

#include "MachDeps.h"


pattern Max, Min ∷ I64
pattern Max =  I64 0x7FFFFFFFFFFFFFFF#
pattern Min = I64 -0x8000000000000000#

deriving newtype instance (≡) I64
deriving newtype instance (≤) I64
deriving newtype instance ℕ I64
deriving newtype instance ℤ I64
-- | /Warning/: Bitwise operations rarely make sense on signed ints,
-- Consider using 'U' instead.
deriving newtype instance 𝔹 I64

-- |Return non-zero if there is any possibility that the upper word of a
--     signed integer multiply might contain useful information.  Return
--     zero only if you are completely sure that no overflow can occur.
--     On a 32-bit platform, the recommended implementation is to do a
--     32 x 32 → 64 signed multiply, and subtract result[63:32] from
--     (result[31] >>signed 31).  I64f this is zero, meaning that the
--     upper word is merely a sign extension of the lower one, no
--     overflow can occur.
--
--     On a 64-bit platform it is not always possible to
--     acquire the top 64 bits of the result.  Therefore, a recommended
--     implementation is to take the absolute value of both operands, and
--     return 0 iff bits[63:31] of them are zero, since that means that their
--     magnitudes fit within 31 bits, so the magnitude of the product must fit
--     into 62 bits.
--
--     I64f in doubt, return non-zero, but do make an effort to create the
--     correct answer for small args, since otherwise the performance of
--     @(×) ∷ I64 → I64 → I64@ will be poor.
mulMayOflo ∷ I64 → I64 → B
mulMayOflo x y = coerce mulIntMayOflo# x y

addC, subC ∷ I64 → I64 → (# I64, B #)
-- |Add signed integers reporting overflow.
--           First member of result is the sum truncated to an @I64@;
--           second member is zero if the true sum fits in an @I64@,
--           nonzero if overflow occurred (the sum is either too large
--           or too small to fit in an @I64@).
addC = coerce addIntC#
-- |Subtract signed integers reporting overflow.
--           First member of result is the difference truncated to an @I64@;
--           second member is zero if the true difference fits in an @I64@,
--           nonzero if overflow occurred (the difference is either too large
--           or too small to fit in an @I64@).
subC = coerce subIntC#

-- * Conversions

toU ∷ I64 → U
toU = coerce int2Word#
fromU ∷ U → I64
fromU = coerce word2Int#

toF32 ∷ I64 → F32
toF32 = coerce int2Float#
toF64 ∷ I64 → F64
toF64 = coerce int2Double#

--toI648# ∷ I64 → I648#
--toI648# = narrow8Int#
--toI6416# ∷ I64 → I6416#
--toI6416# = narrow16Int#
--toI6432# ∷ I64 → I6432#
--toI6432# = narrow32Int#
--toI648 ∷ I64 → I648
--toI648 = narrowInt8#
--toI6416 ∷ I64 → I6416
--toI6416 = narrowInt16#


-- |Shift right arithmetic.  Result undefined if shift amount is not
--           in the range 0 to word size - 1 inclusive.
shiftR# ∷ I64 → I → I64
shiftR# = coerce uncheckedIShiftRA#

-- |Shift right arithmetic.  Result 0 or -1 (depending on sign)
-- if shift amount is not in the range 0 to word size - 1 inclusive.
shiftR ∷ I → I64 → I64
shiftR = coerce I.shiftR

-- | Bitwise negation. @not n = -n - 1@
not ∷ I64 → I64
not = coerce notI#

{-# DEPRECATED shiftL#, shiftRL# "Signed logical bitwise operations are rarely sensible, prefer U instead" #-}

shiftL#, shiftRL# ∷ I64 → I → I64
shiftL, shiftRL ∷ I → I64 → I
-- | Shift left.  Result undefined if shift amount is not
--           in the range 0 to word size - 1 inclusive.
shiftL# = coerce uncheckedIShiftL#

-- | Shift left.  Result 0 if shift amount is not
--           in the range 0 to word size - 1 inclusive.
shiftL = coerce I.shiftL


-- | Shift right logical.  Result undefined if shift amount is not
--           in the range 0 to word size - 1 inclusive.
shiftRL# = coerce uncheckedIShiftRL#
-- | Shift right logical.  Result 0 if shift amount is not
--           in the range 0 to word size - 1 inclusive.
shiftRL = coerce I.shiftRL
