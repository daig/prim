module Int where
import qualified GHC.Types 

add,sub,mul, quot, rem :: Int -> Int -> Int
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
--     @(*) :: Integer -> Integer -> Integer@ will be poor.
mulMayOflo :: Int -> Int -> B
mulMayOflo x y = B# do mulIntMayOflo# x y
negate :: Int -> Int
negate = negateInt#
-- | Rounds towards zero. The behavior is undefined if the first argument is zero.
quot y x = quotInt# x y
-- |Satisfies @(add (rem y x) (mul y (quot y x)) == x@. The
--     behavior is undefined if the first argument is zero.
rem y x = remInt# x y
quotRem :: Int -> Int -> (# Int, Int #)
-- | Rounds towards zero
quotRem y x = quotRemInt# x y

addC, subC :: Int -> Int -> (# Int, B #)
-- |Add signed integers reporting overflow.
--           First member of result is the sum truncated to an @Int@;
--           second member is zero if the true sum fits in an @Int@,
--           nonzero if overflow occurred (the sum is either too large
--           or too small to fit in an @Int@).
addC y x = case addIntC# x y of (# z, o #) -> (# z, B# o #)
-- |Subtract signed integers reporting overflow.
--           First member of result is the difference truncated to an @Int@;
--           second member is zero if the true difference fits in an @Int@,
--           nonzero if overflow occurred (the difference is either too large
--           or too small to fit in an @Int@).
subC y x = case subIntC# x y of (# z, o #) -> (# z, B# o #)

gt y x = B# do x ># y
ge y x = B# do x >=# y
lt y x = B# do x <# y
le y x = B# do x <=# y
eq x y = B# do x ==# y
ne x y = B# do x /=# y

toWord :: Int -> Word
toWord = int2Word#
fromWord :: Word -> Int
fromWord = word2Int#

toF32 :: Int -> F32
toF32 = int2Float#
toF64 :: Int -> F64
toF64 = int2Double#

-- | Shift left.  Result undefined if shift amount is not
--           in the range 0 to word size - 1 inclusive.
shiftL# :: Int -> Int -> Int
shiftL# = uncheckedIShiftL# 


-- |Shift right arithmetic.  Result undefined if shift amount is not
--           in the range 0 to word size - 1 inclusive.

shiftRA# :: Int -> Int -> Int
shiftRA# = uncheckedIShiftRA#

-- |Shift right logical.  Result undefined if shift amount is not
--           in the range 0 to word size - 1 inclusive.

shiftRL# :: Int -> Int -> Int
shiftRL# = uncheckedIShiftRL#

-- | Included for completeness, but signed bit operations should never be used.
and, or, xor :: Int -> Int -> Int
and = andI#
or = orI#
xor = xorI#

not :: Int -> Int
not = notI#
