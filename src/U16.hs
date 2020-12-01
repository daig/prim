--------------------------------------------------------------------
-- | Description : 16-bit Unsigned Integer operations
--------------------------------------------------------------------
module U16 (U16(U16#,U16), module U16) where
import U ()

deriving newtype instance (≡) U16
deriving newtype instance (≤) U16
instance ℕ U16 where
  (U16 x) + (U16 y) = U16 (plusWord# x y)
  (U16 x) × (U16 y) = U16 (timesWord# x y)
  (U16 x) / (U16 y) = U16 (quotWord# x y)
  (U16 x) % (U16 y) = U16 (remWord# x y)
  (U16 x) /% (U16 y) = case quotRemWord# x y of (# d, m #) → (# U16 d, U16 m #)
instance 𝔹 U16 where
  (∧) = coerce ((∧) @_ @U)
  (∨) = coerce ((∨) @_ @U)
  (⊕) = coerce ((⊕) @_ @U)
  (¬) (U16 u) = U16 (u ¬)

pattern Max, Min ∷ U16
pattern Max = U16# 0xFFFF##
pattern Min = U16# 0##

-- × Bitwise operations

-- | Count the number of set bits
popCnt,clz,ctz ∷ U16 → U8
popCnt = coerce popCnt16#; clz = coerce clz16#; ctz = coerce ctz16#

byteSwap ∷ U16 → U16
byteSwap = coerce byteSwap16#
pext ∷ U64 → U16 → U16
pext = coerce pext16#
pdep ∷ U64 → U16 → U64
pdep = coerce pdep16#

-- | Reverse the order of the bits.
reverse ∷ U16 → U16
reverse = coerce bitReverse16#
