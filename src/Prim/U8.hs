--------------------------------------------------------------------
-- | Description : 8-bit Unsigned Integer operations
--------------------------------------------------------------------
{-# language PostfixOperators #-}
module Prim.U8 (U8(U8#,U8), module Prim.U8) where
import Prim.U ()

newtype U8  ∷ T_U where U8#  ∷ U → U8
-- | Narrow a machine 'U' to 8 bits
pattern U8 ∷ U → U8
pattern U8 i ← (coerce → i) where U8 = coerce narrow8Word#
{-# complete U8 #-}

deriving newtype instance (≡) U8
deriving newtype instance (≤) U8
instance ℕ U8 where
  (U8 x) + (U8 y) = U8 (plusWord# x y)
  (U8 x) × (U8 y) = U8 (timesWord# x y)
  (U8 x) / (U8 y) = U8 (quotWord# x y)
  (U8 x) % (U8 y) = U8 (remWord# x y)
  (U8 x) /% (U8 y) = case quotRemWord# x y of (# d, m #) → (# U8 d, U8 m #)
  addC (U8 a) (U8 b) = let c = a + b in (# U8 c , c > coerce Max #)
  subC (U8 a) (U8 b) = case subC a b of (# x, b #) → (# U8 x , b #)
instance 𝔹 U8 where
  (∧) = coerce ((∧) @_ @U)
  (∨) = coerce ((∨) @_ @U)
  (⊕) = coerce ((⊕) @_ @U)
  (¬) = (Max ⊕)
  shiftL# (U8 w) (word2Int# → i) = U8 (uncheckedShiftL# w i)
  shiftL w i = case i ≥ 8## of {T → U8# 0##; F → shiftL# w i}
  shiftR# (U8 w) (word2Int# → i) = U8 (uncheckedShiftRL# w i)
  shiftR w i = case i ≥ 8## of {T → U8# 0##; F → shiftL# w i}
  shift (U8 w) i = case i ≥ 0# of
    T → case i ≥  8# of {T → U8# 0##; F → U8 (uncheckedShiftL# w i)}
    F → case i ≤ -8# of {T → U8# 0##; F → U8 (uncheckedShiftRL# w (negateInt# i))}
  popCnt = coerce popCnt8#; clz = coerce clz8#; ctz = coerce ctz8#
  byteSwap x = x
  bitReverse = coerce bitReverse8#
  pdep = coerce pdep8#; pext = coerce pext8#


pattern Max, Min ∷ U8
pattern Max = U8# 0xFF##
pattern Min = U8# 0##

-- × Bitwise operations
-- | Count the number of set bits
popCnt,clz,ctz ∷ U8 → U8
popCnt = coerce popCnt8#; clz = coerce clz8#; ctz = coerce ctz8#

pext ∷ U64 → U8 → U8
pext = coerce pext8#
pdep ∷ U64 → U8 → U64
pdep = coerce pdep8#

-- | Reverse the order of the bits.
reverse ∷ U8 → U8
reverse = coerce bitReverse8#
