{-# language CPP #-}
{-# language BangPatterns #-}
module I (I, module I) where
import B (pattern B#,(∧), pattern T)
import qualified GHC.Classes as GHC (divInt#,modInt#)

import Coerce

#include "MachDeps.h"


pattern Max, Min ∷ I
pattern Max =  0x7FFFFFFFFFFFFFFF#
pattern Min = -0x8000000000000000#

-- * Arithmetic Operations

(+), (-), (×), add, sub, mul ∷ I → I → I
(+) = (+#); (-) = (-#)
-- | Low word of signed integer multiply
(×) = (*#)
add y x = x +# y; sub y x = x -# y
-- | Low word of signed integer multiply
mul y x = x *# y
-- |Return non-zero if there is any possibility that the upper word of a
--     signed integer multiply might contain useful information.  Return
--     zero only if you are completely sure that no overflow can occur.
--     On a 32-bit platform, the recommended implementation is to do a
--     32 x 32 → 64 signed multiply, and subtract result[63:32] from
--     (result[31] >>signed 31).  If this is zero, meaning that the
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
--     If in doubt, return non-zero, but do make an effort to create the
--     correct answer for small args, since otherwise the performance of
--     @(×) ∷ I → I → I@ will be poor.
mulMayOflo ∷ I → I → B#
mulMayOflo x y = mulIntMayOflo# x y
negate ∷ I → I
negate = negateInt#
-- | Rounds towards 0. The behavior is undefined if the first argument is zero.
quot, rem ∷ I {- ^ divisor -}  → I {- ^ dividend -} → I
(%%), (//) ∷ I {- ^ dividend -}  → I {- ^ divisor -} → I
quot y x = quotInt# x y
(//) = quotInt#
-- |Satisfies @(add (rem y x) (mul y (quot y x)) == x@. The
--     behavior is undefined if the first argument is zero.
rem y x = remInt# x y
(%%) = remInt#
-- | Rounds towards 0. The behavior is undefined if the first argument is zero.
quotRem ∷ I → I → (# I, I #)
quotRem y x = quotRemInt# x y

-- These functions have built-in rules.
-- | Rounds towards -∞. The behavior is undefined if the first argument is zero.
div,mod ∷ I {- ^ divisor -} → I {- ^ dividend -} → I
(%), (/) ∷ I {- ^ dividend -}  → I {- ^ divisor -} → I
div y x = GHC.divInt# x y; {-# inline div #-}
mod y x = GHC.modInt# x y; {-# inline mod #-}
(%) = GHC.modInt#; {-# inline (%) #-}
(/) = GHC.divInt#; {-# inline (/) #-}
-- | Rounds towards -∞. The behavior is undefined if the first argument is zero.
divMod ∷ I {- ^ divisor -} → I {- ^ dividend -} → (# I, I #) {- ^ (div, mod) -}
divMod y x | B# (0# < x) ∧ B# (0# > y) = case quotRem y (x -# 1# ) of
                                    (# q, r #) → (# q -# 1#, r +# y +# 1# #)
           | B# (0# > x) ∧ B# (0# < y) = case quotRem y (x +# 1# ) of
                                    (# q, r #) → (# q -# 1#, r +# y +# 1# #)
           | T = quotRem y x


addC, subC ∷ I → I → (# I, B# #)
-- |Add signed integers reporting overflow.
--           First member of result is the sum truncated to an @I@;
--           second member is zero if the true sum fits in an @I@,
--           nonzero if overflow occurred (the sum is either too large
--           or too small to fit in an @I@).
addC y x = addIntC# x y
-- |Subtract signed integers reporting overflow.
--           First member of result is the difference truncated to an @I@;
--           second member is zero if the true difference fits in an @I@,
--           nonzero if overflow occurred (the difference is either too large
--           or too small to fit in an @I@).
subC y x = subIntC# x y

-- * Comparison Operators

instance (≤) I where
  (>) = (># )
  (≥) = (>=# )
  (<) = (<# )
  (≤) = (<=# )
instance (≡) I where
  (≡) = (==# )
  (≠) = (/=# )

-- * Conversions

toU ∷ I → U
toU = int2Word#
fromU ∷ U → I
fromU = word2Int#

toF32 ∷ I → F32
toF32 = int2Float#
toF64 ∷ I → F64
toF64 = int2Double#

--toI8# ∷ I → I8#
--toI8# = narrow8Int#
--toI16# ∷ I → I16#
--toI16# = narrow16Int#
--toI32# ∷ I → I32#
--toI32# = narrow32Int#
--toI8 ∷ I → I8
--toI8 = narrowInt8#
--toI16 ∷ I → I16
--toI16 = narrowInt16#


-- |Shift right arithmetic.  Result undefined if shift amount is not
--           in the range 0 to word size - 1 inclusive.
shiftR# ∷ I → I → I
shiftR# = uncheckedIShiftRA#

-- |Shift right arithmetic.  Result 0 or -1 (depending on sign)
-- if shift amount is not in the range 0 to word size - 1 inclusive.
shiftR ∷ I → I → I
shiftR i x | B# (i ≥ WORD_SIZE_IN_BITS#) = if B# (x < 0#) then (-1#) else 0#
           | T = uncheckedIShiftRA# x i

-- | Bitwise negation. @not n = -n - 1@
not ∷ I → I
not = notI#

{-# DEPRECATED shiftL#, shiftRL#, and, or, xor "Signed logical bitwise operations are rarely sensible, prefer U instead" #-}

shiftL#, shiftL, shiftRL#, shiftRL ∷ I → I → I
-- | Shift left.  Result undefined if shift amount is not
--           in the range 0 to word size - 1 inclusive.
shiftL# i x = uncheckedIShiftL# x i

-- | Shift left.  Result 0 if shift amount is not
--           in the range 0 to word size - 1 inclusive.
shiftL i x | B# (i ≥ WORD_SIZE_IN_BITS#) = 0#
           | T = uncheckedIShiftL# x i


-- | Shift right logical.  Result undefined if shift amount is not
--           in the range 0 to word size - 1 inclusive.
shiftRL# i x = uncheckedIShiftRL# x i
-- | Shift right logical.  Result 0 if shift amount is not
--           in the range 0 to word size - 1 inclusive.
shiftRL i x | B# (i ≥ WORD_SIZE_IN_BITS#) = 0#
            | T = uncheckedIShiftRL# x i
and, or, xor ∷ I → I → I
and = andI#
or = orI#
xor = xorI#
