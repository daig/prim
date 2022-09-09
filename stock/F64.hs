module F64 where

newtype F64 = F64# Double

(≡) ∷ F64 → F64 → B
(≡) = coerce eqDouble
{-# inline (≡) #-}
