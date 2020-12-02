--------------------------------------------------------------------
-- | Description : Primitive boolean type
--------------------------------------------------------------------
module B (B(B#,F,T), module B) where
import I ()

deriving newtype instance (≡) B
deriving newtype instance (≤) B
instance 𝔹 B where
  (∧) = coerce andI#
  (∨) = coerce orI#
  (⊕) = coerce xorI#
  (¬) = (T ⊕)
  shiftL# (I1 x) i = I1 (uncheckedIShiftL# x (word2Int# i))
  shiftL x = \case {0## → x; _ → F}
  shiftR# (I1 x) i = I1 (uncheckedIShiftRL# x (word2Int# i))
  shiftR = shiftL
  shift x = \case {0# → x; _ → F}
  popCnt = \case {F → 0##; T → 1##}
  clz = \case {F → 1##; T → 0##}
  ctz _ = 0##
  byteSwap x = x
  bitReverse x = x
  pdep = (∧); pext = (∧)

pattern B ∷ I → B
pattern B i ← B# i where B i = 0# < i
pattern I1 ∷ I → B
pattern I1 i ← B# i where I1 i = B# (andI# 0# i)
