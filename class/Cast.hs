module Cast where
import Do as Prim
import Unsafe.Coerce

-- | Nontrivial conversions between types. Use with care!
type Cast :: forall {r :: RuntimeRep} {r' :: RuntimeRep}. T r -> T r' -> C
class Cast b a where cast ∷ a → b

instance Cast I U where cast = word2Int#
instance Cast F32 U where cast = word2Float#
instance Cast F64 U where cast = word2Double#

instance Cast U I where cast = int2Word#
instance Cast F32 I where cast = int2Float#
instance Cast F64 I where cast = int2Double#

-- | Truthiness
instance Cast B I where cast = coerce (0# >#)
instance Cast B U where cast = coerce do gtWord# 0##

instance Cast F32 F64 where cast = double2Float#
instance Cast F64 F32 where cast = float2Double#
instance Cast I F32 where cast = float2Int#
instance Cast I F64 where cast = double2Int#

instance Cast I I8 where cast = int8ToInt#
instance Cast I I16 where cast = int16ToInt#
instance Cast I I32 where cast = int32ToInt#
instance Cast I I64 where cast = int64ToInt#
instance Cast I8 I where cast = intToInt8#
instance Cast I16 I where cast = intToInt16#
instance Cast I32 I where cast = intToInt32#
instance Cast I64 I where cast = intToInt64#

instance Cast U U8 where cast = word8ToWord#
instance Cast U U16 where cast = word16ToWord#
instance Cast U U32 where cast = word32ToWord#
instance Cast U U64 where cast = word64ToWord#
instance Cast U8 U where cast = wordToWord8#
instance Cast U16 U where cast = wordToWord16#
instance Cast U32 U where cast = wordToWord32#
instance Cast U64 U where cast = wordToWord64#


instance Cast I (P_Stable_Name a) where cast = stableNameToInt#

-- Freezing and Thawing Arrays

-- | Original array should not be used again
instance Cast (Bytes_M s) Bytes where cast = unsafeCoerce#
-- | Original array should not be used again
instance Cast Bytes (Bytes_M s) where
  cast (M_UnpinnedByteArray# m) = UnpinnedByteArray# (runST (unsafeFreezeByteArray# (unsafeCoerce# m)))

--
-- | Original array should not be used again
instance Cast (Bytes_Pinned_M s) Bytes_Pinned where cast = unsafeCoerce#
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

-- copyByteArray# src src_ofs dst dst_ofs n

instance Cast Bytes Buffer where
  cast (Bytes_Off_Len# (# x, off, n #)) = runST Prim.do
    mv <- newByteArray# n
    \s -> (# copyByteArray# x off mv 0# n s, (##) #)
    v <- unsafeFreezeByteArray# mv
    return (UnpinnedByteArray# v)
  

instance Cast Buffer Bytes where
  cast (UnpinnedByteArray# x) = Bytes_Off_Len# (# x, 0#, sizeofByteArray# x #)

instance Cast P# Bytes_Pinned where cast = coerce byteArrayContents#

instance Cast I Char where cast = ord#
instance Cast Char I where cast = chr#

-- | This pattern is strongly deprecated
instance Cast P# I where cast = int2Addr#
-- | This pattern is strongly deprecated
instance Cast I P# where cast = addr2Int#
