module Cast where
import Do
import Unsafe.Coerce

class Cast (b ∷ T r) (a ∷ T r') where cast ∷ a → b

instance Cast I U where cast = word2Int#
instance Cast F32 U where cast = word2Float#
instance Cast F64 U where cast = word2Double#

instance Cast U I where cast = int2Word#
instance Cast F32 I where cast = int2Float#
instance Cast F64 I where cast = int2Double#

-- | Truthiness
instance Cast B I where cast = coerce (0# >#)
instance Cast B U where cast = coerce do gtWord# 0##

instance Cast I F64 where cast = double2Int#
instance Cast F32 F64 where cast = double2Float#
instance Cast F64 F32 where cast = float2Double#

instance Cast I8 I where cast = intToInt8#
instance Cast I16 I where cast = intToInt16#
instance Cast I32 I where cast = intToInt32#

instance Cast U8 U where cast = wordToWord8#
instance Cast U16 U where cast = wordToWord16#
instance Cast U32 U where cast = wordToWord32#

-- Freezing and Thawing Arrays

-- | Original array should not be used again
instance Cast (M_A# s) A# where cast = unsafeCoerce#
-- instance Cast A# (M_A# s) where cast m = run (coerce (unsafeFreezeByteArray# (coerce m)))
--
-- | Original array should not be used again
instance Cast (M_Pinned# s) Pinned# where cast = unsafeCoerce#
-- instance Cast Pinned# (M_Pinned# s) where cast m = run (coerce (unsafeFreezeByteArray# (coerce m)))

-- | Original array should not be used again
-- instance Cast (M_A x s) (A x) where cast (SmallArray# a) = M_SmallArray# (run (unsafeThawSmallArray# a))
-- instance Cast (A x) (M_A x s) where cast (M_SmallArray# m) = SmallArray# (run (unsafeFreezeSmallArray# m))

-- | Original array should not be used again
--instance Cast A# (M_A# s) where cast m = run (coerce (unsafeFreezeByteArray# (m ≑)))

-- | Original array should not be used again
-- instance Cast (M_A_ I s) (A_ I) where cast (A# a) = M_A# do cast a
-- instance Cast (A_ I) (M_A_ I s) where cast (M_A# m) = A# do cast m

-- | Original array should not be used again
-- instance Cast (M_Pinned_ I s) (Pinned_ I) where cast (Pinned# a) = M_Pinned# (cast a)
-- instance Cast (Pinned_ I) (M_Pinned_ I s) where cast (M_Pinned# m) = Pinned# (cast m)

-- | Original array should not be used again
--instance Cast (M_Arr x s) (Arr x) where cast (Array# a) = M_Array# (run (unsafeThawArray# a))
--instance Cast (Arr x) (M_Arr x s) where cast (M_Array# m) = Array# (run (unsafeFreezeArray# m))
