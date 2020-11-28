--------------------------------------------------------------------
-- | Description : 32-bit Unsigned Integer operations
--------------------------------------------------------------------
module U32 (U32(U32#,U32),module U32) where
import U ()

deriving newtype instance (≡) U32
deriving newtype instance (≤) U32
instance 𝔹 U32 where
  (∧) = coerce ((∧) @_ @U)
  (∨) = coerce ((∨) @_ @U)
  (⊕) = coerce ((⊕) @_ @U)
  (¬) (U32 u) = U32 (u ¬)


(+),(-),(×) ∷ U32 → U32 → U32
x + y = U32 (coerce plusWord# x y)
x - y = U32 (coerce minusWord# x y)
x × y = U32 (coerce timesWord# x y)

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
