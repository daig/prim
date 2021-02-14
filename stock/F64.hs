module F64 (module F64
           ,Double,B) where

newtype F64 = F64# Double

(≡) ∷ F64 → F64 → B
(≡) = coerce eqDouble
{-# inline (≡) #-}
