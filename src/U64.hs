module U64 (U64, module U64) where
import qualified GHC.Types as GHC

add,sub,mul, quot, rem :: U64 -> U64 -> U64
add y x = plusWord# x y
sub y x = minusWord# x y
mul y x = timesWord# x y

-- |Add unsigned integers, with the high part (carry) in the first
--           component of the returned pair and the low part in the second
--           component of the pair. See also @addC@.
add2 :: U64 -> U64 -> (# U64, U64 #)
add2 y x = plusWord2# x y

-- | Rounds towards zero. The behavior is undefined if the first argument is zero.
quot y x = quotWord# x y

-- |Satisfies @(add (rem y x) (mul y (quot y x)) == x@. The
--     behavior is undefined if the first argument is zero.
rem y x = remWord# x y

-- | Rounds towards zero
quotRem :: U64 -> U64 -> (# U64, U64 #)
quotRem y x = quotRemWord# x y
-- |Add signed integers reporting overflow.
--           First member of result is the sum truncated to an @U64@;
--           second member is zero if the true sum fits in an @U64@,
--           nonzero if overflow occurred (the sum is either too large
--           or too small to fit in an @U64@).
addC, subC :: U64 -> U64 -> (# U64, B #)
addC y x = addWordC# x y
-- |Subtract signed integers reporting overflow.
--           First member of result is the difference truncated to an @U64@;
--           second member is zero if the true difference fits in an @U64@,
--           nonzero if overflow occurred (the difference is either too large
--           or too small to fit in an @U64@).
subC y x = subWordC# x y

gt,ge,lt,le,eq,ne :: U64 -> U64 -> B
gt y x = gtWord# x y
ge y x = geWord# x y
lt y x = ltWord# x y
le y x = leWord# x y
eq x y = eqWord# x y
ne x y = neWord# x y

fromInt :: Int -> U64
fromInt = int2Word#
toInt :: U64 -> Int
toInt = word2Int#

toF32 :: U64 -> F32
toF32 = word2Float#
toF64 :: U64 -> F64
toF64 = word2Double#

toU8 :: U64 -> U8
toU8 = narrow8Word#
toU16 :: U64 -> U16
toU16 = narrow16Word#
toU32 :: U64 -> U32
toU32 = narrow32Word#

toB8 :: U64 -> B8
toB8 = narrow8Word#
toB16 :: U64 -> B16
toB16 = narrow16Word#
toB32 :: U64 -> B32
toB32 = narrow32Word#
