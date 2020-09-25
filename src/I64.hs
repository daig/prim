{-# language BangPatterns #-}
module I64 (I64, (+#), (-#), (*#), module I64) where
import Stock.B (pattern B#,(&&), pattern I)
import qualified GHC.Classes as GHC (divInt#,modInt#)

add,sub,mul, quot, rem :: I64 -> I64 -> I64
add y x = x +# y
sub y x = x -# y
-- | Low word of signed integer multiply
mul y x = x *# y
-- |Return non-zero if there is any possibility that the upper word of a
--     signed integer multiply might contain useful information.  Return
--     zero only if you are completely sure that no overflow can occur.
--     On a 32-bit platform, the recommended implementation is to do a
--     32 x 32 -> 64 signed multiply, and subtract result[63:32] from
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
--     @(*) :: I64eger -> I64eger -> I64eger@ will be poor.
mulMayOflo :: I64 -> I64 -> B
mulMayOflo x y = mulIntMayOflo# x y
negate :: I64 -> I64
negate = negateInt#
-- | Rounds towards zero. The behavior is undefined if the first argument is zero.
quot y x = quotInt# x y
-- |Satisfies @(add (rem y x) (mul y (quot y x)) == x@. The
--     behavior is undefined if the first argument is zero.
rem y x = remInt# x y
quotRem :: I64 -> I64 -> (# I64, I64 #)
-- | Rounds towards zero
quotRem y x = quotRemInt# x y

-- These functions have built-in rules.
div,mod :: I64 {- ^ divisor -} -> I64 {- ^ dividend -} -> I64 {- ^ modulus -}
div y x = GHC.divInt# x y; {-# inline div #-}
mod y x = GHC.modInt# x y; {-# inline mod #-}
-- | Rounds towards negative infinity. The behavior is undefined if the first argument is zero.
divMod :: I64 {- ^ divisor -} -> I64 {- ^ dividend -} -> (# I64, I64 #) {- ^ (div, mod) -}
divMod y x | B# (gt 0# x) && B# (lt 0# y) = case quotRem y (x -# 1# ) of
                                    (# q, r #) -> (# q -# 1#, r +# y +# 1# #)
           | B# (lt 0# x) && B# (gt 0# y) = case quotRem y (x +# 1# ) of
                                    (# q, r #) -> (# q -# 1#, r +# y +# 1# #)
           | I = quotRem y x


addC, subC :: I64 -> I64 -> (# I64, B #)
-- |Add signed integers reporting overflow.
--           First member of result is the sum truncated to an @I64@;
--           second member is zero if the true sum fits in an @I64@,
--           nonzero if overflow occurred (the sum is either too large
--           or too small to fit in an @I64@).
addC y x = addIntC# x y
-- |Subtract signed integers reporting overflow.
--           First member of result is the difference truncated to an @I64@;
--           second member is zero if the true difference fits in an @I64@,
--           nonzero if overflow occurred (the difference is either too large
--           or too small to fit in an @I64@).
subC y x = subIntC# x y

gt,ge,lt,le,eq,ne :: I64 -> I64 -> B
gt y x = x ># y
ge y x = x >=# y
lt y x = x <# y
le y x = x <=# y
eq x y = x ==# y
ne x y = x /=# y

toU64 :: I64 -> U64
toU64 = int2Word#
fromU64 :: U64 -> I64
fromU64 = word2Int#

toF32 :: I64 -> F32
toF32 = int2Float#
toF64 :: I64 -> F64
toF64 = int2Double#

toI8 :: I64 -> I8
toI8 = narrow8Int#
toI16 :: I64 -> I16
toI16 = narrow16Int#
toI32 :: I64 -> I32
toI32 = narrow32Int#

-- |Shift right arithmetic.  Result undefined if shift amount is not
--           in the range 0 to word size - 1 inclusive.

shiftRA# :: I64 -> I64 -> I64
shiftRA# = uncheckedIShiftRA#


-- * Bitwise operations included for completeness, but signed bit operations should never be used.

{-# DEPRECATED shiftL#, shiftRL#, and, or, xor "Don't use signed bitwise operations, prefer U64 instead" #-}

-- | Shift left.  Result undefined if shift amount is not
--           in the range 0 to word size - 1 inclusive.
shiftL# :: I64 -> I64 -> I64
shiftL# i x = uncheckedIShiftL# x i

-- |Shift right logical.  Result undefined if shift amount is not
--           in the range 0 to word size - 1 inclusive.

shiftRL# :: I64 -> I64 -> I64
shiftRL# i x = uncheckedIShiftRL# x i
and, or, xor :: I64 -> I64 -> I64
and = andI#
or = orI#
xor = xorI#

not :: I64 -> I64
not = notI#
