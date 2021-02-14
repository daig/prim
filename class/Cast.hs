{-# language LinearTypes #-}
module Cast where

class Cast (b ∷ T r) (a ∷ T r') where cast ∷ a ⊸ b

instance Cast I U where cast = (λ word2Int#)
instance Cast F32 U where cast = (λ word2Float#)
instance Cast F64 U where cast = (λ word2Double#)

instance Cast U I where cast = (λ int2Word#)
instance Cast F32 I where cast = (λ int2Float#)
instance Cast F64 I where cast = (λ int2Double#)

-- | Truthiness
instance Cast B I where cast = coerce do λ (0# >#)
instance Cast B U where cast = coerce do λ do gtWord# 0##

instance Cast I F64 where cast = (λ double2Int#)
instance Cast F32 F64 where cast = (λ double2Float#)
instance Cast F64 F32 where cast = (λ float2Double#)

instance Cast I8 I where cast = coerce (λ narrow8Int#)
instance Cast I16 I where cast = coerce (λ narrow16Int#)
instance Cast I32 I where cast = coerce (λ narrow32Int#)
instance Cast I64 I where cast = I64

instance Cast U8 U where cast = coerce (λ narrow8Word#)
instance Cast U16 U where cast = coerce (λ narrow16Word#)
instance Cast U32 U where cast = coerce (λ narrow32Word#)
instance Cast U64 U where cast = U64
