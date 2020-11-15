{-# language CPP #-}
module U (U, module U) where
import qualified GHC.Types as GHC
import B
#include "MachDeps.h"

(+),(-),(*) ∷ U → U → U
(+) = plusWord#; (-) = minusWord#; (*) = timesWord#
add,sub,mul, quot, rem, div, mod ∷ U → U → U
add y x = plusWord# x y; sub y x = minusWord# x y; mul y x = timesWord# x y

-- |Add unsigned integers, with the high part (carry) in the first
--           component of the returned pair and the low part in the second
--           component of the pair. See also @addC@.
add2 ∷ U → U → (# U, U #)
add2 y x = plusWord2# x y

-- | Rounds towards zero. The behavior is undefined if the first argument is zero.
quot y x = quotWord# x y
div y x = quotWord# x y
(//) = quotWord#
(/) = quotWord#

-- |Satisfies @(add (rem y x) (mul y (quot y x)) == x@. The
--     behavior is undefined if the first argument is zero.
rem y x = remWord# x y
mod y x = remWord# x y
(%%) = remWord#
(%) = remWord#

-- | Rounds towards zero
quotRem, divMod ∷ U → U → (# U, U #)
quotRem y x = quotRemWord# x y
divMod y x = quotRemWord# x y
-- |Add signed integers reporting overflow.
--           First member of result is the sum truncated to an @U@;
--           second member is zero if the true sum fits in an @U@,
--           nonzero if overflow occurred (the sum is either too large
--           or too small to fit in an @U@).
addC, subC ∷ U → U → (# U, B# #)
addC y x = addWordC# x y
-- |Subtract signed integers reporting overflow.
--           First member of result is the difference truncated to an @U@;
--           second member is zero if the true difference fits in an @U@,
--           nonzero if overflow occurred (the difference is either too large
--           or too small to fit in an @U@).
subC y x = subWordC# x y

infix 4 >, ≥, <, ≤, ≡, ≠
(>),(≥),(<),(≤),(≡),(≠) ∷ U → U → B#
(>) = gtWord#; (≥) = geWord#; (<) = ltWord#; (≤) = leWord#
(≡) = eqWord#; (≠) = neWord#
gt,ge,lt,le,eq,ne ∷ U → U → B#
gt = ltWord#; ge = leWord#; lt = gtWord#; le = geWord#
eq = eqWord#; ne = neWord#

fromI ∷ I → U
fromI = int2Word#
toI ∷ U → I
toI = word2Int#

toF32 ∷ U → F32
toF32 = word2Float#
toF64 ∷ U → F64
toF64 = word2Double#

toU8# ∷ U → U8#
toU8# = narrow8Word#
toU16# ∷ U → U16#
toU16# = narrow16Word#
toU32# ∷ U → U32#
toU32# = narrow32Word#
toU8 ∷ U → U8
toU8 = narrowWord8#
toU16 ∷ U → U16
toU16 = narrowWord16#

pattern Max, Min ∷ U
pattern Max = 0xFFFFFFFFFFFFFFFF##
pattern Min = 0##

-- * Bitwise operations
infixl 7 ∧
infixl 6 ⊕
infixl 5 ∨ 
(∧),(∨),(⊕) ∷ U → U → U
(∧) = and#; (∨) = or#; (⊕) = xor#
and,or,xor ∷ U → U → U
and = and#; or = or#; xor = xor#
not ∷ U → U
not = not#

-- | Shift left.  Result undefined if shift amount is not
--           in the range 0 to word size - 1 inclusive.
shiftL#, shiftRL#, shiftL ∷ I → U → U
shiftL# i w = uncheckedShiftL# w i
shiftL i w | B# (i >=# WORD_SIZE_IN_BITS#) = 0##
           | T = uncheckedShiftL# w i


-- |Shift right logical.  Result undefined if shift amount is not
--           in the range 0 to word size - 1 inclusive.
shiftRL# i w = uncheckedShiftRL# w i
shiftRL i w | B# (i >=# WORD_SIZE_IN_BITS#) = 0##
            | T = uncheckedShiftRL# w i

-- | Count the number of set bits
popCnt,clz,ctz ∷ U → U8#
popCnt = popCnt#; clz = clz#; ctz = ctz#

byteSwap ∷ U → U
byteSwap = byteSwap#
pdep, pext ∷ U → U → U
pdep y x = pdep# x y; pext y x = pext# x y

-- | Reverse the order of the bits.
reverse ∷ U → U
reverse = bitReverse#
