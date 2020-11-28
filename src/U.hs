--------------------------------------------------------------------
-- | Description : Machine Word sized Unsigned Integer type
--------------------------------------------------------------------
{-# language CPP #-}
module U (U, module U) where
import qualified GHC.Types as GHC
import B hiding (not#)
#include "MachDeps.h"
import I ()

(+),(-),(×) ∷ U → U → U
(+) = plusWord#; (-) = minusWord#; (×) = timesWord#
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
addC, subC ∷ U → U → (# U, B #)
addC = coerce addWordC#
-- |Subtract signed integers reporting overflow.
--           First member of result is the difference truncated to an @U@;
--           second member is zero if the true difference fits in an @U@,
--           nonzero if overflow occurred (the difference is either too large
--           or too small to fit in an @U@).
subC = coerce subWordC#

instance (≤) U where (>) = coerce gtWord#; (≥) = coerce geWord#; (<) = coerce ltWord#; (≤) = coerce leWord#
instance (≡) U where (≡) = coerce eqWord#; (≠) = coerce neWord#

fromI ∷ I → U
fromI = int2Word#
toI ∷ U → I
toI = word2Int#

toF32 ∷ U → F32
toF32 = word2Float#
toF64 ∷ U → F64
toF64 = word2Double#

pattern Max, Min ∷ U
pattern Max = 0xFFFFFFFFFFFFFFFF##
pattern Min = 0##

instance 𝔹 U where (∧) = and#; (∨) = or#; (⊕) = xor#; (¬) = not#

-- | Shift left.  Result undefined if shift amount is not
--           in the range 0 to word size - 1 inclusive.
shiftL#, shiftRL#, shiftL ∷ I → U → U
shiftL# i w = uncheckedShiftL# w i
shiftL i w = case i ≥ WORD_SIZE_IN_BITS# of {T → 0##; F → uncheckedShiftL# w i}


-- |Shift right logical.  Result undefined if shift amount is not
--           in the range 0 to word size - 1 inclusive.
shiftRL# i w = uncheckedShiftRL# w i
shiftRL i w = case i ≥ WORD_SIZE_IN_BITS# of {T → 0##; F → uncheckedShiftRL# w i}

-- | Count the number of set bits
popCnt,clz,ctz ∷ U → U8
popCnt = coerce popCnt#; clz = coerce clz#; ctz = coerce ctz#

byteSwap ∷ U → U
byteSwap = byteSwap#
pdep, pext ∷ U → U → U
pdep y x = pdep# x y; pext y x = pext# x y

-- | Reverse the order of the bits.
reverse ∷ U → U
reverse = bitReverse#
