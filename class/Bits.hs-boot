module Bits where

class Logic (a ∷ T r) where
  (∧), (∨), (⊕) ∷ a → a → a
  (¬) ∷ a → a
type Bits ∷ ∀ {r}. T r → Constraint
class Bits a where
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
instance Logic I
instance Logic I8
instance Logic I16
instance Logic I32
instance Logic I64
instance Logic U
instance Logic U8
instance Logic U16
instance Logic U32
instance Logic U64
