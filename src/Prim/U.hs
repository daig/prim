--------------------------------------------------------------------
-- | Description : Machine Word sized Unsigned Integer type
--------------------------------------------------------------------
{-# language CPP #-}
module Prim.U where
import qualified GHC.Types as GHC
import Prim.B hiding (not#)
#include "MachDeps.h"
import Prim.I ()
import Class.Prim.Bits
import Class.Prim.Num
import Class.Prim.Cmp

-- | An unsigned integral type, with the same size as 'I'.
type U = Word#

instance ℕ U where
  (+) = plusWord#; (×) = timesWord#
  (/) = quotWord#; (%) = remWord#
  (/%) = quotRemWord#
  addC a b = case addWordC# a b of (# x, b #) → (# x , b ≠ 0# #)
  subC a b = case subWordC# a b of (# x, b #) → (# x , b ≠ 0# #)

-- | Unsigned modular subtraction.
(-) ∷ U → U → U
(-) = minusWord#
-- |Add unsigned integers, with the high part (carry) in the first
--           component of the returned pair and the low part in the second
--           component of the pair. See also @addC@.
add2 ∷ U → U → (# U, U #)
add2 y x = plusWord2# x y

-- |Add signed integers reporting overflow.
--           First member of result is the sum truncated to an @U@;
--           second member is zero if the true sum fits in an @U@,
--           nonzero if overflow occurred (the sum is either too large
--           or too small to fit in an @U@).
-- |Subtract signed integers reporting overflow.
--           First member of result is the difference truncated to an @U@;
--           second member is zero if the true difference fits in an @U@,
--           nonzero if overflow occurred (the difference is either too large
--           or too small to fit in an @U@).

instance (≤) U where
  (>) = coerce gtWord#
  (≥) = coerce geWord#
  (<) = coerce ltWord#
  (≤) = coerce leWord#
  cmp a b = Ordering# (ltWord# a b + geWord# a b -# 1#)
instance (≡) U where
  (≡) = coerce eqWord#
  (≠) = coerce neWord#

--fromI ∷ I → U
--fromI = int2Word#
--toI ∷ U → I
--toI = word2Int#

--toF32 ∷ U → F32
--toF32 = word2Float#
--toF64 ∷ U → F64
--toF64 = word2Double#

pattern Max, Min ∷ U
pattern Max = 0xFFFFFFFFFFFFFFFF##
pattern Min = 0##

instance 𝔹 U where
  (∧) = and#; (∨) = or#; (⊕) = xor#; (¬) = not#
  shiftL# w (word2Int# → i) = uncheckedShiftL# w i
  shiftL w i = case i ≥ WORD_SIZE_IN_BITS## of {T → 0##; F → shiftL# w i}
  shiftR# w (word2Int# → i) = uncheckedShiftRL# w i
  shiftR w i = case i ≥ WORD_SIZE_IN_BITS## of {T → 0##; F → shiftL# w i}
  shift w i = case i ≥ 0# of
    T → case i ≥ WORD_SIZE_IN_BITS# of {T → 0##; F → uncheckedShiftL# w i}
    F → case i ≤ -WORD_SIZE_IN_BITS# of {T → 0##; F → uncheckedShiftRL# w (negateInt# i)}
  popCnt = popCnt#; clz = clz#; ctz = ctz#
  byteSwap = byteSwap#
  bitReverse = bitReverse#
  pdep = pdep#; pext = pext#

