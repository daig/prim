module Bits where

class Logic (a ∷ T r) where
  (∧), (∨), (⊕) ∷ a → a → a
  (¬) ∷ a → a
class Bits (a ∷ T r) where
  (<<#) ∷ a → U → a
  (<<) ∷ a → U → a
  (>>#) ∷ a → U → a
  (>>) ∷ a → U → a
  shift ∷ a → I → a 
  bit ∷ U → a
  bit' ∷ a → U → B#
  popCnt ∷ a → U
  clz ∷ a → U
  ctz ∷ a → U
  byteSwap ∷ a → a
  bitReverse ∷ a → a
  pdep, pext ∷ a → a → a

infixl 3 ∧
infixl 2 ⊕
infixl 1 ∨

instance Logic B#
