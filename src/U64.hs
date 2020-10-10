module U64 (U64, module U64) where
import qualified GHC.Types as GHC

(+),(-),(*) ∷ U64 → U64 → U64
(+) = plusWord#; (-) = minusWord#; (*) = timesWord#
add,sub,mul, quot, rem ∷ U64 → U64 → U64
add y x = plusWord# x y; sub y x = minusWord# x y; mul y x = timesWord# x y

-- |Add unsigned integers, with the high part (carry) in the first
--           component of the returned pair and the low part in the second
--           component of the pair. See also @addC@.
add2 ∷ U64 → U64 → (# U64, U64 #)
add2 y x = plusWord2# x y

-- | Rounds towards zero. The behavior is undefined if the first argument is zero.
quot y x = quotWord# x y

-- |Satisfies @(add (rem y x) (mul y (quot y x)) == x@. The
--     behavior is undefined if the first argument is zero.
rem y x = remWord# x y

-- | Rounds towards zero
quotRem ∷ U64 → U64 → (# U64, U64 #)
quotRem y x = quotRemWord# x y
-- |Add signed integers reporting overflow.
--           First member of result is the sum truncated to an @U64@;
--           second member is zero if the true sum fits in an @U64@,
--           nonzero if overflow occurred (the sum is either too large
--           or too small to fit in an @U64@).
addC, subC ∷ U64 → U64 → (# U64, B #)
addC y x = addWordC# x y
-- |Subtract signed integers reporting overflow.
--           First member of result is the difference truncated to an @U64@;
--           second member is zero if the true difference fits in an @U64@,
--           nonzero if overflow occurred (the difference is either too large
--           or too small to fit in an @U64@).
subC y x = subWordC# x y

(>),(≥),(<),(≤),(≡),(≠) ∷ U64 → U64 → B
(>) = gtWord#; (≥) = geWord#; (<) = ltWord#; (≤) = leWord#
(≡) = eqWord#; (≠) = neWord#
gt,ge,lt,le,eq,ne ∷ U64 → U64 → B
gt = ltWord#; ge = leWord#; lt = gtWord#; le = geWord#
eq = eqWord#; ne = neWord#

fromInt ∷ Int → U64
fromInt = int2Word#
toInt ∷ U64 → Int
toInt = word2Int#

toF32 ∷ U64 → F32
toF32 = word2Float#
toF64 ∷ U64 → F64
toF64 = word2Double#

toU8 ∷ U64 → U8
toU8 = narrow8Word#
toU16 ∷ U64 → U16
toU16 = narrow16Word#
toU32 ∷ U64 → U32
toU32 = narrow32Word#

pattern Max, Min ∷ U64
pattern Max = 0xFFFFFFFFFFFFFFFF##
pattern Min = 0##

-- * Bitwise operations
infixl 7 ∧
infixl 6 ⊕
infixl 5 ∨ 
(∧),(∨),(⊕) ∷ U64 → U64 → U64
(∧) = and#; (∨) = or#; (⊕) = xor#
and,or,xor ∷ U64 → U64 → U64
and = and#; or = or#; xor = xor#
not ∷ U64 → U64
not = not#

-- | Shift left.  Result undefined if shift amount is not
--           in the range 0 to word size - 1 inclusive.
shiftL#, shiftRL# ∷ Int → U64 → U64
shiftL# i w = uncheckedShiftL# w i

-- |Shift right logical.  Result undefined if shift amount is not
--           in the range 0 to word size - 1 inclusive.
shiftRL# i w = uncheckedShiftRL# w i

-- | Count the number of set bits
popCnt,clz,ctz ∷ U64 → U8
popCnt = popCnt#; clz = clz#; ctz = ctz#

byteSwap ∷ U64 → U64
byteSwap = byteSwap#
pdep, pext ∷ U64 → U64 → U64
pdep y x = pdep# x y; pext y x = pext# x y

-- | Reverse the order of the bits.
reverse ∷ U64 → U64
reverse = bitReverse#
