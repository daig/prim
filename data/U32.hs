--------------------------------------------------------------------
-- | Description : 32-bit Unsigned Integer operations
--------------------------------------------------------------------
module U32 (U32(U32#,U32,Min,Max), module X) where
import {-# source #-} U (U)
import Cast as X
import Cmp as X
import Num as X
import Bits as X
import B

newtype U32  ∷ T_U where U32#  ∷ U → U32
-- | Narrow a machine 'U' to 32 bits
pattern U32 ∷ U → U32
pattern U32 u ← (coerce → u) where U32 = cast
{-# complete U32 #-}

deriving newtype instance (≡) U32
deriving newtype instance (≤) U32
instance ℕ U32 where
  (U32 x) + (U32 y) = U32 (plusWord# x y)
  (U32 x) × (U32 y) = U32 (timesWord# x y)
  (U32 x) / (U32 y) = U32 (quotWord# x y)
  (U32 x) % (U32 y) = U32 (remWord# x y)
  (U32 x) /% (U32 y) = case quotRemWord# x y of (# d, m #) → (# U32 d, U32 m #)
  addC (U32 a) (U32 b) = let c = a + b in (# U32 c , c > coerce Max #)
  subC (U32 a) (U32 b) = case subC a b of (# x, b #) → (# U32 x , b #)
instance 𝔹 U32 where
  (∧) = coerce ((∧) @_ @U)
  (∨) = coerce ((∨) @_ @U)
  (⊕) = coerce ((⊕) @_ @U)
  (¬) = (Max ⊕)
  shiftL# (U32 w) (word2Int# → i) = U32 (uncheckedShiftL# w i)
  shiftL w i = case i ≥ 32## of {T → U32# 0##; F → shiftL# w i}
  shiftR# (U32 w) (word2Int# → i) = U32 (uncheckedShiftRL# w i)
  shiftR w i = case i ≥ 32## of {T → U32# 0##; F → shiftL# w i}
  shift (U32 w) i = case i ≥ 0# of
    T → case i ≥  32# of {T → U32# 0##; F → U32 (uncheckedShiftL# w i)}
    F → case i ≤ -32# of {T → U32# 0##; F → U32 (uncheckedShiftRL# w (negateInt# i))}
  popCnt = coerce popCnt32#; clz = coerce clz32#; ctz = coerce ctz32#
  byteSwap x = x
  bitReverse = coerce bitReverse32#
  pdep = coerce pdep32#; pext = coerce pext32#

instance Cast U32 U where cast = coerce narrow32Word#

pattern Max, Min ∷ U32
pattern Max = U32# 0xFFFFFFFF##
pattern Min = U32# 0##
