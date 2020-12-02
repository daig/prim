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
  addC (U16 a) (U16 b) = let c = a + b in (# U16 c , c > coerce Max #)
  subC (U16 a) (U16 b) = case subC a b of (# x, b #) → (# U16 x , b #)
instance 𝔹 U16 where
  (∧) = coerce ((∧) @_ @U)
  (∨) = coerce ((∨) @_ @U)
  (⊕) = coerce ((⊕) @_ @U)
  (¬) (U16 u) = U16 (u ¬)
  shiftL# (U16 w) (word2Int# → i) = U16 (uncheckedShiftL# w i)
  shiftL w i = case i ≥ 16## of {T → U16# 0##; F → shiftL# w i}
  shiftR# (U16 w) (word2Int# → i) = U16 (uncheckedShiftRL# w i)
  shiftR w i = case i ≥ 16## of {T → U16# 0##; F → shiftL# w i}
  shift (U16 w) i = case i ≥ 0# of
    T → case i ≥  16# of {T → U16# 0##; F → U16 (uncheckedShiftL# w i)}
    F → case i ≤ -16# of {T → U16# 0##; F → U16 (uncheckedShiftRL# w (negateInt# i))}
  popCnt = coerce popCnt16#; clz = coerce clz16#; ctz = coerce ctz16#
  byteSwap = coerce byteSwap16#
  bitReverse = coerce bitReverse16#
  pdep = coerce pdep16#; pext = coerce pext16#

-- | Unsigned modular subtraction.
(-) ∷ U → U → U
(-) = minusWord#

pattern Max, Min ∷ U16
pattern Max = U16# 0xFFFF##
pattern Min = U16# 0##
