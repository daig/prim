--------------------------------------------------------------------
-- | Description : 8-bit Unsigned Integer operations
--------------------------------------------------------------------
{-# language PostfixOperators #-}
module U8 (U8(U8#,U8), module U8) where
import U ()

deriving newtype instance (≡) U8
deriving newtype instance (≤) U8
deriving newtype instance (⊕) U8
instance (¬) U8 where (¬) (U8 u) = U8 (u ¬)

(+),(-),(×) ∷ U8 → U8 → U8
x + y = U8 (coerce plusWord# x y)
x - y = U8 (coerce minusWord# x y)
x × y = U8 (coerce timesWord# x y)

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
