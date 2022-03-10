module Prim.U32 where
import Prim.B ()

type U32 = Word32#

pattern U32 ∷ U → U32
pattern U32 i ← (word32ToWord# → i) where U32 = wordToWord32#

instance (≤) U32 where (>) = coerce gtWord32#; (≥) = coerce geWord32#
                       (<) = coerce ltWord32#; (≤) = coerce leWord32#
instance (≡) U32 where (≡) = coerce eqWord32#; (≠) = coerce neWord32#
instance ℕ U32 where
  (+) = plusWord32#; (×) = timesWord32#
  (/) = quotWord32#
  (%) = remWord32#
  (/%) = quotRemWord32#
  addC a b = let c = a + b in (# c , c < a ∧ c < b #)
  subC a b = let c = a - b in (# c , c > a ∧ c > b #)

-- | Unsigned modular subtraction.
(-) ∷ U32 → U32 → U32
(-) = subWord32#

instance 𝔹 U32 where
  (∧) = andWord32#; (∨) = orWord32#; (⊕) = xorWord32#; (¬) = notWord32#
  shiftL# w (word2Int# → i) = uncheckedShiftLWord32# w i
  shiftL w i = case i ≥ 32## of {T → 0##; F → shiftL# w i}
  shiftR# w (word2Int# → i) = uncheckedShiftRLWord32# w i
  shiftR w i = case i ≥ 32## of {T → 0##; F → shiftL# w i}
  shift w i = case i ≥ 0# of
    T → case i ≥  32# of {T → 0##; F → shiftL# w i}
    F → case i ≤ -32# of {T → 0##; F → shiftR# w (negateInt# i)}
  popCnt (U32 u) = popCnt32# u; clz (U32 u) = clz32# u; ctz (U32 u) = ctz32# u
  byteSwap (U32 u) = U32 (byteSwap32# u)
  bitReverse (U32 u) = U32 (bitReverse# u)
  pdep (U32 s) (U32 m) = U32 (pdep# s m); pext (U32 s) (U32 m) = U32 (pext# s m)

pattern Max, Min ∷ U32
pattern Max =  U32 0xFFFFFFFF##
pattern Min = U32 0##
