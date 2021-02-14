--------------------------------------------------------------------
-- | Description : Machine Word sized Signed Integer type
--------------------------------------------------------------------
{-# language CPP #-}
{-# language BangPatterns #-}
module Prim.I (I, module Prim.I) where
import Class.Prim.Num
import Class.Prim.Cmp
import Class.Prim.Bits

#include "MachDeps.h"

-- | A fixed-precision integer type with at least the range @[-2^29 .. 2^29-1]@.
-- The exact range for a given implementation can be determined by using
-- 'I.Min' and 'I.Max'.
type I = Int#


pattern Max, Min ∷ I
pattern Max =  0x7FFFFFFFFFFFFFFF#
pattern Min = -0x8000000000000000#

-- | Low word of signed integer multiply
-- 
-- Modular functions have built-in rules.
instance ℕ I where
  (+) = (+#)
  (×) = (*#)
  (%) = modInt#; {-# inline (%) #-}
  (/) = divInt#; {-# inline (/) #-}
  x /% y = case 0# < x ∧ 0# > y of
    B 1# → case (x - 1# ) //%% y of (# q, r #) → (# q - 1#, r + y + 1# #)
    B 0# → case 0# > x ∧ 0# < y of
      B 1# → case (x + 1# ) //%% y of (# q, r #) → (# q - 1#, r + y + 1# #)
      B 0# → x //%% y
  addC a b = case addIntC# a b of (# x, b #) → (# x , b ≠ 0# #)
  subC a b = case subIntC# a b of (# x, b #) → (# x , b ≠ 0# #)
instance ℤ I where
  negate = negateInt#
  (-) = (-#)
  (//) = quotInt#
  (%%) = remInt#
  (//%%) = quotRemInt#

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
mulMayOflo ∷ I → I → B
mulMayOflo = coerce mulIntMayOflo#
-- |Satisfies @(add (rem y x) (mul y (quot y x)) == x@. The
--     behavior is undefined if the first argument is zero.

{-
divMod y x | B (0# < x) ∧ B (0# > y) = case quotRem y (x -# 1# ) of
                                    (# q, r #) → (# q -# 1#, r +# y +# 1# #)
           | B (0# > x) ∧ B (0# < y) = case quotRem y (x +# 1# ) of
                                    (# q, r #) → (# q -# 1#, r +# y +# 1# #)
           | T = quotRem y x
-}


addC, subC ∷ I → I → (# I, B #)
-- |Add signed integers reporting overflow.
--           First member of result is the sum truncated to an @I@;
--           second member is zero if the true sum fits in an @I@,
--           nonzero if overflow occurred (the sum is either too large
--           or too small to fit in an @I@).
addC = coerce addIntC#
-- |Subtract signed integers reporting overflow.
--           First member of result is the difference truncated to an @I@;
--           second member is zero if the true difference fits in an @I@,
--           nonzero if overflow occurred (the difference is either too large
--           or too small to fit in an @I@).
subC = coerce subIntC#

-- * Comparison Operators

-- * Conversions

--toU ∷ I → U
--toU = int2Word#
--fromU ∷ U → I
--fromU = word2Int#

--toF32 ∷ I → F32
--toF32 = int2Float#
--toF64 ∷ I → F64
--toF64 = int2Double#

--toI8 ∷ I → I8
--toI8 = narrow8Int#
--toI16 ∷ I → I16
--toI16 = narrow16Int#
--toI32 ∷ I → I32
--toI32 = narrow32Int#
--toI8 ∷ I → I8
--toI8 = narrowInt8#
--toI16 ∷ I → I16
--toI16 = narrowInt16#
