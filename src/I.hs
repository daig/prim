{-# language BangPatterns #-}
module I (I, module I) where
import B (pattern B#,(∧), pattern T)
import qualified GHC.Classes as GHC (divInt#,modInt#)

(+), (-), (*) ∷ I → I → I
(+) = (+#); (-) = (-#)
-- | Low word of signed integer multiply
(*) = (*#)
add,sub,mul, quot, rem ∷ I → I → I
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
--     @(*) ∷ Ieger → Ieger → Ieger@ will be poor.
mulMayOflo ∷ I → I → B#
mulMayOflo x y = mulIntMayOflo# x y
negate ∷ I → I
negate = negateInt#
-- | Rounds towards zero. The behavior is undefined if the first argument is zero.
quot y x = quotInt# x y
-- |Satisfies @(add (rem y x) (mul y (quot y x)) == x@. The
--     behavior is undefined if the first argument is zero.
rem y x = remInt# x y
quotRem ∷ I → I → (# I, I #)
-- | Rounds towards zero
quotRem y x = quotRemInt# x y

-- These functions have built-in rules.
div,mod ∷ I {- ^ divisor -} → I {- ^ dividend -} → I {- ^ modulus -}
div y x = GHC.divInt# x y; {-# inline div #-}
mod y x = GHC.modInt# x y; {-# inline mod #-}
-- | Rounds towards negative infinity. The behavior is undefined if the first argument is zero.
divMod ∷ I {- ^ divisor -} → I {- ^ dividend -} → (# I, I #) {- ^ (div, mod) -}
divMod y x | B# (gt 0# x) ∧ B# (lt 0# y) = case quotRem y (x -# 1# ) of
                                    (# q, r #) → (# q -# 1#, r +# y +# 1# #)
           | B# (lt 0# x) ∧ B# (gt 0# y) = case quotRem y (x +# 1# ) of
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

(>),(≥),(<),(≤),(≡),(≠) ∷ I → I → B#
(>) = (>#); (≥) = (>=#); (<) = (<#); (≤) = (<=#)
(≡) = (==#); (≠) = (/=#)
gt,ge,lt,le,eq,ne ∷ I → I → B#
gt = (<#)
ge = (<=#)
lt = (>#)
le = (>=#)
eq = (==#)
ne = (/=#)

toU64 ∷ I → U64
toU64 = int2Word#
fromU64 ∷ U64 → I
fromU64 = word2Int#

toF32 ∷ I → F32
toF32 = int2Float#
toF64 ∷ I → F64
toF64 = int2Double#

toI8# ∷ I → I8#
toI8# = narrow8Int#
toI16# ∷ I → I16#
toI16# = narrow16Int#
toI32# ∷ I → I32#
toI32# = narrow32Int#
toI8 ∷ I → I8
toI8 = narrowInt8#
toI16 ∷ I → I16
toI16 = narrowInt16#


-- |Shift right arithmetic.  Result undefined if shift amount is not
--           in the range 0 to word size - 1 inclusive.

shiftRA# ∷ I → I → I
shiftRA# = uncheckedIShiftRA#


-- * Bitwise operations included for completeness, but signed bit operations should never be used.

{-# DEPRECATED shiftL#, shiftRL#, and, or, xor "Don't use signed bitwise operations, prefer U64 instead" #-}

-- | Shift left.  Result undefined if shift amount is not
--           in the range 0 to word size - 1 inclusive.
shiftL# ∷ I → I → I
shiftL# i x = uncheckedIShiftL# x i

-- |Shift right logical.  Result undefined if shift amount is not
--           in the range 0 to word size - 1 inclusive.

shiftRL# ∷ I → I → I
shiftRL# i x = uncheckedIShiftRL# x i
and, or, xor ∷ I → I → I
and = andI#
or = orI#
xor = xorI#

not ∷ I → I
not = notI#

pattern Max, Min ∷ I
pattern Max =  0x7FFFFFFFFFFFFFFF#
pattern Min = -0x8000000000000000#
