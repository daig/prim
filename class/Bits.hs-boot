module Bits where

class Logic (a ∷ T r) where
  (∧), (∨), (⊕) ∷ a → a → a
  (¬) ∷ a → a
class Bits (a ∷ T r) where
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
  casP ∷ P# → a → a → ST s a 
  casA ∷ Bytes_M s → I → a → a → ST s a

infixl 3 ∧
infixl 2 ⊕
infixl 1 ∨

instance Logic B
