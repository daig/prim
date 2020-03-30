module Word where
import qualified GHC.Types as GHC

add,sub,mul, quot, rem :: Word -> Word -> Word
add y x = plusWord# x y
sub y x = minusWord# x y
mul y x = timesWord# x y

-- |Add unsigned integers, with the high part (carry) in the first
--           component of the returned pair and the low part in the second
--           component of the pair. See also @addC@.
add2 :: Word -> Word -> (# Word, Word #)
add2 y x = plusWord2# x y

-- | Rounds towards zero. The behavior is undefined if the first argument is zero.
quot y x = quotWord# x y

-- |Satisfies @(add (rem y x) (mul y (quot y x)) == x@. The
--     behavior is undefined if the first argument is zero.
rem y x = remWord# x y

-- | Rounds towards zero
quotRem :: Word -> Word -> (# Word, Word #)
quotRem y x = quotRemWord# x y
-- |Add signed integers reporting overflow.
--           First member of result is the sum truncated to an @Word@;
--           second member is zero if the true sum fits in an @Word@,
--           nonzero if overflow occurred (the sum is either too large
--           or too small to fit in an @Word@).
addC, subC :: Word -> Word -> (# Word, B #)
addC y x = case addWordC# x y of (# z, o #) -> (# z, B# o #)
-- |Subtract signed integers reporting overflow.
--           First member of result is the difference truncated to an @Word@;
--           second member is zero if the true difference fits in an @Word@,
--           nonzero if overflow occurred (the difference is either too large
--           or too small to fit in an @Word@).
subC y x = case subWordC# x y of (# z, o #) -> (# z, B# o #)

gt y x = B# do gtWord# x y
ge y x = B# do geWord# x y
lt y x = B# do ltWord# x y
le y x = B# do leWord# x y
eq x y = B# do eqWord# x y
ne x y = B# do neWord# x y

fromInt :: Int -> Word
fromInt = int2Word#
toInt :: Word -> Int
toInt = word2Int#

toF32 :: Word -> F32
toF32 = word2Float#
toF64 :: Word -> F64
toF64 = word2Double#

toU8 :: Word -> U8
toU8 = narrow8Word#
toU16 :: Word -> U16
toU16 = narrow16Word#
toU32 :: Word -> U32
toU32 = narrow32Word#

toB8 :: Word -> B8
toB8 = narrow8Word#
toB16 :: Word -> B16
toB16 = narrow16Word#
toB32 :: Word -> B32
toB32 = narrow32Word#

-- | Shift left.  Result undefined if shift amount is not
--           in the range 0 to word size - 1 inclusive.
shiftL#, shiftRL# :: Int -> Word -> Word
shiftL# i w = uncheckedShiftL# w i

-- |Shift right logical.  Result undefined if shift amount is not
--           in the range 0 to word size - 1 inclusive.
shiftRL# i w = uncheckedShiftRL# w i

-- | Count the number of set bits
popCnt,clz,ctz :: Word -> U8
popCnt = popCnt#; clz = clz#; ctz = ctz#

byteSwap :: Word -> Word
byteSwap = byteSwap#
pdep, pext :: Word -> Word -> Word
pdep y x = pdep# x y; pext y x = pext# x y

