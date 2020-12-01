--------------------------------------------------------------------
-- | Description : 32-bit Unsigned Integer operations
--------------------------------------------------------------------
module U32 (U32(U32#,U32),module U32) where
import U ()

deriving newtype instance (≡) U32
deriving newtype instance (≤) U32
instance ℕ U32 where
  (U32 x) + (U32 y) = U32 (plusWord# x y)
  (U32 x) × (U32 y) = U32 (timesWord# x y)
  (U32 x) / (U32 y) = U32 (quotWord# x y)
  (U32 x) % (U32 y) = U32 (remWord# x y)
  (U32 x) /% (U32 y) = case quotRemWord# x y of (# d, m #) → (# U32 d, U32 m #)
instance 𝔹 U32 where
  (∧) = coerce ((∧) @_ @U)
  (∨) = coerce ((∨) @_ @U)
  (⊕) = coerce ((⊕) @_ @U)
  (¬) (U32 u) = U32 (u ¬)

pattern Max, Min ∷ U32
pattern Max =  U32# 0xFFFFFFFF##
pattern Min = U32# 0##

-- × Bitwise operations

-- | Count the number of set bits
popCnt,clz,ctz ∷ U32 → U32
popCnt = coerce popCnt32#; clz = coerce clz32#; ctz = coerce ctz32#

byteSwap ∷ U32 → U32
byteSwap = coerce byteSwap32#

pext ∷ U → U32 → U32
pext = coerce pext32#
pdep ∷ U → U32 → U
pdep = coerce pdep32#

-- | Reverse the order of the bits.
reverse ∷ U32 → U32
reverse = coerce bitReverse32#
