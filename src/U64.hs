--------------------------------------------------------------------
-- | Description : 64-bit Unsigned Integer operations
--------------------------------------------------------------------
{-# language CPP #-}
module U64 (U64(..), module U64) where
import qualified GHC.Types as GHC
import B hiding (not#)
import qualified U
#include "MachDeps.h"

deriving newtype instance (≡) U64
deriving newtype instance (≤) U64
deriving newtype instance ℕ U64
instance 𝔹 U64 where
  (∧) = coerce ((∧) @_ @U)
  (∨) = coerce ((∨) @_ @U)
  (⊕) = coerce ((⊕) @_ @U)
  (¬) (U64 u) = U64 (u ¬)
  shiftL# (U64 w) (word2Int# → i) = U64 (uncheckedShiftL# w i)
  shiftL w i = case i ≥ 64## of {T → U64 0##; F → shiftL# w i}
  shiftR# (U64 w) (word2Int# → i) = U64 (uncheckedShiftRL# w i)
  shiftR w i = case i ≥ 64## of {T → U64 0##; F → shiftL# w i}
  shift (U64 w) i = case i ≥ 0# of
    T → case i ≥  64# of {T → U64 0##; F → U64 (uncheckedShiftL# w i)}
    F → case i ≤ -64# of {T → U64 0##; F → U64 (uncheckedShiftRL# w (negateInt# i))}
  popCnt = coerce popCnt64#; clz = coerce clz64#; ctz = coerce ctz64#
  byteSwap = coerce byteSwap64#
  bitReverse = coerce bitReverse64#
  pdep = coerce pdep64#; pext = coerce pext64#



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
addC, subC ∷ U64 → U64 → (# U64, B #)
addC = coerce addWordC#
-- |Subtract signed integers reporting overflow.
--           First member of result is the difference truncated to an @U64@;
--           second member is zero if the true difference fits in an @U64@,
--           nonzero if overflow occurred (the difference is either too large
--           or too small to fit in an @U64@).
subC = coerce subWordC#

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
