{-# language CPP #-}
module U64 (U64(..), module U64) where
import qualified GHC.Types as GHC
import B hiding (not#)
import qualified U
#include "MachDeps.h"


(+),(-),(×) ∷ U64 → U64 → U64
(+) = coerce plusWord#; (-) = coerce minusWord#; (×) = coerce timesWord#

-- |Add unsigned integers, with the high part (carry) in the first
--           component of the returned pair and the low part in the second
--           component of the pair. See also @addC@.
add2 ∷ U64 → U64 → (# U64, U64 #)
add2 = coerce plusWord2#

(//), (%%) ∷ U64 → U64 → U64
-- | Rounds towards zero. The behavior is undefined if the first argument is zero.
(//) = coerce quotWord#

-- |satisfies @(add (rem y x) (mul y (quot y x)) == x@. The
--     behavior is undefined if the first argument is zero.
(%%) = coerce remWord#

-- | Rounds towards zero
(//%%), (/%) ∷ U64 → U64 → (# U64, U64 #)
(//%%) = coerce quotRemWord#
(/%) = coerce quotRemWord#
-- |Add signed integers reporting overflow.
--           First member of result is the sum truncated to an @U64@;
--           second member is zero if the true sum fits in an @U64@,
--           nonzero if overflow occurred (the sum is either too large
--           or too small to fit in an @U64@).
addC, subC ∷ U64 → U64 → (# U64, B# #)
addC = coerce addWordC#
-- |Subtract signed integers reporting overflow.
--           First member of result is the difference truncated to an @U64@;
--           second member is zero if the true difference fits in an @U64@,
--           nonzero if overflow occurred (the difference is either too large
--           or too small to fit in an @U64@).
subC = coerce subWordC#

infix 4 >, ≥, <, ≤, ≡, ≠
(>),(≥),(<),(≤),(≡),(≠) ∷ U64 → U64 → B#
(>) = coerce gtWord#; (≥) = coerce geWord#; (<) = coerce ltWord#; (≤) = coerce leWord#
(≡) = coerce eqWord#; (≠) = coerce neWord#

fromI ∷ I → U64
fromI = coerce int2Word#
toI ∷ U64 → I
toI = coerce word2Int#

toF32 ∷ U64 → F32
toF32 = coerce word2Float#
toF64 ∷ U64 → F64
toF64 = coerce word2Double#

pattern Max, Min ∷ U64
pattern Max = U64 0xFFFFFFFFFFFFFFFF##
pattern Min = U64 0##

-- × Bitwise operations
infixl 7 ∧
infixl 6 ⊕
infixl 5 ∨ 
(∧),(∨),(⊕) ∷ U64 → U64 → U64
(∧) = coerce and#; (∨) = coerce or#; (⊕) = coerce xor#
not ∷ U64 → U64
not = coerce not#

shiftL#, shiftRL# ∷ U64 → I → U64
shiftL, shiftRL ∷ I → U64 → U64
-- | Shift left.  Result undefined if shift amount is not
--           in the range 0 to word size - 1 inclusive.
shiftL# = coerce uncheckedShiftL#
-- | Shift left.
shiftL = coerce U.shiftL


-- |Shift right logical.  Result undefined if shift amount is not
--           in the range 0 to word size - 1 inclusive.
shiftRL# = coerce uncheckedShiftRL#
-- |Shift right logical.
shiftRL = coerce U.shiftRL

-- | Count the number of set bits
popCnt,clz,ctz ∷ U64 → U8
popCnt = coerce popCnt#; clz = coerce clz#; ctz = coerce ctz#

byteSwap ∷ U64 → U64
byteSwap = coerce byteSwap#
pdep, pext ∷ U64 → U64 → U64
pdep = coerce pdep#; pext = coerce pext#

-- | Reverse the order of the bits.
reverse ∷ U64 → U64
reverse = coerce bitReverse#
