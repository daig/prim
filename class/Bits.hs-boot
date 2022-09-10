module Bits where

class 𝔹 (a ∷ T r) where
  (∧), (∨), (⊕) ∷ a → a → a
  (¬) ∷ a → a
  shiftL# ∷ a → U → a
  shiftL ∷ a → U → a
  shiftR# ∷ a → U → a
  shiftR ∷ a → U → a
  shift ∷ a → I → a 
  popCnt ∷ a → U
  clz ∷ a → U
  ctz ∷ a → U
  byteSwap ∷ a → a
  bitReverse ∷ a → a
  pdep, pext ∷ a → a → a
  casP ∷ P# → a {- ^ expected old value -}
            → a {- ^ new value -}
            → ST s a {- ^ the original value inside -}
  casA ∷ Bytes_M s → I {- ^ offset in bytes -}
                   → a {- ^ expected old value -}
                   → a {- ^ new value -}
                   → ST s a {- ^ the original value inside -}

infixl 3 ∧
infixl 2 ⊕
infixl 1 ∨

instance 𝔹 B
