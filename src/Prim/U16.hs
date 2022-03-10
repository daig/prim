module Prim.U16 where
import Prim.B ()

type U16 = Word16#

pattern U16 ∷ U → U16
pattern U16 i ← (word16ToWord# → i) where U16 = wordToWord16#

instance (≤) U16 where (>) = coerce gtWord16#; (≥) = coerce geWord16#
                       (<) = coerce ltWord16#; (≤) = coerce leWord16#
instance (≡) U16 where (≡) = coerce eqWord16#; (≠) = coerce neWord16#
instance ℕ U16 where
  (+) = plusWord16#; (×) = timesWord16#
  (/) = quotWord16#
  (%) = remWord16#
  (/%) = quotRemWord16#
  addC a b = let c = a + b in (# c , c < a ∧ c < b #)
  subC a b = let c = a - b in (# c , c > a ∧ c > b #)

-- | Unsigned modular subtraction.
(-) ∷ U16 → U16 → U16
(-) = subWord16#

instance 𝔹 U16 where
  (∧) = andWord16#; (∨) = orWord16#; (⊕) = xorWord16#; (¬) = notWord16#
  shiftL# w (word2Int# → i) = uncheckedShiftLWord16# w i
  shiftL w i = case i ≥ 16## of {T → 0##; F → shiftL# w i}
  shiftR# w (word2Int# → i) = uncheckedShiftRLWord16# w i
  shiftR w i = case i ≥ 16## of {T → 0##; F → shiftL# w i}
  shift w i = case i ≥ 0# of
    T → case i ≥  16# of {T → 0##; F → shiftL# w i}
    F → case i ≤ -16# of {T → 0##; F → shiftR# w (negateInt# i)}
  popCnt (U16 u) = popCnt16# u; clz (U16 u) = clz16# u; ctz (U16 u) = ctz16# u
  byteSwap (U16 u) = U16 (byteSwap16# u)
  bitReverse (U16 u) = U16 (bitReverse# u)
  pdep (U16 s) (U16 m) = U16 (pdep# s m); pext (U16 s) (U16 m) = U16 (pext# s m)

pattern Max, Min ∷ U16
pattern Max =  U16 0xFFFF##
pattern Min = U16 0##
