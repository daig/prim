{-# language LinearTypes #-}
module Cast where
import Do
import Unsafe.Coerce

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

instance Cast I8 I where cast = λ intToInt8#
instance Cast I16 I where cast = λ intToInt16#
instance Cast I32 I where cast = λ intToInt32#

instance Cast U8 U where cast = λ wordToWord8#
instance Cast U16 U where cast = λ wordToWord16#
instance Cast U32 U where cast = λ wordToWord32#

-- Freezing and Thawing Arrays

-- | Original array should not be used again
instance Cast M_A# A# where cast = λ unsafeCoerce#
instance Cast A# M_A# where cast = λ\m → run do coerce do λ do unsafeFreezeByteArray# (coerce m)
--
-- | Original array should not be used again
instance Cast M_Pinned# Pinned# where cast = λ unsafeCoerce#
instance Cast Pinned# M_Pinned# where cast = λ\m → run do coerce do λ do unsafeFreezeByteArray# (coerce m)

-- | Original array should not be used again
instance Cast (M_A x) (A x) where cast = λ\(SmallArray# a) → M_SmallArray# do run do λ do unsafeThawSmallArray# a
instance Cast (A x) (M_A x) where cast = λ\(M_SmallArray# m) → SmallArray# do run do λ do unsafeFreezeSmallArray# m

-- | Original array should not be used again
--instance Cast M_AA# AA# where cast = coerce#
--instance Cast A# M_A# where cast = λ\m → run do coerce do λ do unsafeFreezeByteArray# (m ≑)

-- | Original array should not be used again
instance Cast (M_A_ I) (A_ I) where cast = λ\(A# a) → M_A# do cast a
instance Cast (A_ I) (M_A_ I) where cast = λ\(M_A# m) → A# do cast m

-- | Original array should not be used again
instance Cast (M_Pinned_ I) (Pinned_ I) where cast = λ\(Pinned# a) → M_Pinned# do cast a
instance Cast (Pinned_ I) (M_Pinned_ I) where cast = λ\(M_Pinned# m) → Pinned# do cast m

-- | Original array should not be used again
instance Cast (M_Arr x) (Arr x) where cast = λ\(Array# a) → M_Array# do run do λ do unsafeThawArray# a
instance Cast (Arr x) (M_Arr x) where cast = λ\(M_Array# m) → Array# do run do λ do unsafeFreezeArray# m

