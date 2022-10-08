module Bits where

class Logic (a ∷ T r) where
  (&&), (||), xor ∷ a → a → a
  not ∷ a → a
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

infixl 3 &&
infixl 2 `xor`
infixl 1 ||

instance Logic B#
instance Logic I
instance Logic I1
instance Logic I2
instance Logic I4
instance Logic I8
instance Logic U
instance Logic U1
instance Logic U2
instance Logic U4
instance Logic U8
