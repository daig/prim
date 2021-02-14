module B where
import {-# source #-} I
import {-# source #-} Bits
newtype B ∷ T_I where B# ∷ {unB ∷ I} → B
instance 𝔹 B
