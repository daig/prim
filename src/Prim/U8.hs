module Prim.U8 where
import Prim.B ()

type U8 = Word8#

pattern U8 ∷ U → U8
pattern U8 i ← (word8ToWord# → i) where U8 = wordToWord8#

instance (≤) U8 where (>) = coerce gtWord8#; (≥) = coerce geWord8#
                       (<) = coerce ltWord8#; (≤) = coerce leWord8#
instance (≡) U8 where (≡) = coerce eqWord8#; (≠) = coerce neWord8#
instance ℕ U8 where
  (+) = plusWord8#; (×) = timesWord8#
  (/) = quotWord8#
  (%) = remWord8#
  (/%) = quotRemWord8#
  addC a b = let c = a + b in (# c , c < a ∧ c < b #)
  subC a b = let c = a - b in (# c , c > a ∧ c > b #)

-- | Unsigned modular subtraction.
(-) ∷ U8 → U8 → U8
(-) = subWord8#

instance 𝔹 U8 where
  (∧) = andWord8#; (∨) = orWord8#; (⊕) = xorWord8#; (¬) = notWord8#
  shiftL# w (word2Int# → i) = uncheckedShiftLWord8# w i
  shiftL w i = case i ≥ 8## of {T → 0##; F → shiftL# w i}
  shiftR# w (word2Int# → i) = uncheckedShiftRLWord8# w i
  shiftR w i = case i ≥ 8## of {T → 0##; F → shiftL# w i}
  shift w i = case i ≥ 0# of
    T → case i ≥  8# of {T → 0##; F → shiftL# w i}
    F → case i ≤ -8# of {T → 0##; F → shiftR# w (negateInt# i)}
  popCnt (U8 u) = popCnt8# u; clz (U8 u) = clz8# u; ctz (U8 u) = ctz8# u
  byteSwap (U8 u) = U8 (byteSwap8# u)
  bitReverse (U8 u) = U8 (bitReverse# u)
  pdep (U8 s) (U8 m) = U8 (pdep# s m); pext (U8 s) (U8 m) = U8 (pext# s m)

pattern Max, Min ∷ U8
pattern Max =  U8 0xFF##
pattern Min = U8 0##
