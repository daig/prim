{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE UnliftedDatatypes #-}
module Cmp where
import Coerce
import {-# source #-} Bits
import Cast
import If

infix 4 >, ≥, <, ≤, ≡, ≠, `cmp`
class (≡) (a ∷ T r) where (≡), (≠) ∷ a → a → B#
class (≡) a ⇒ (≤) (a ∷ T r) where
  (>),(≥),(<),(≤) ∷ a → a → B#
  cmp ∷ a → a → Ordering
  -- | Minimum value
  min ∷ a → a → a
  -- | Maximum value
  max ∷ a → a → a


deriving newtype instance (≡) B#
deriving newtype instance (≤) B#
deriving newtype instance (≡) Ordering

instance (≡) I where
  (≡) = coerce (==#)
  (≠) = coerce (/=#)
instance (≤) I where
  (>) = coerce (>#)
  (≥) = coerce (>=#)
  (<) = coerce (<#)
  (≤) = coerce (<=#)
  cmp a b = Ordering# do (a ># b) +# (a >=# b) -# 1#
  min x y = x ≤ y ? x $ y
  max x y = x ≥ y ? x $ y

instance (≡) I8 where
  (≡) = coerce eqInt8#
  (≠) = coerce neInt8#
instance (≤) I8 where
  (>) = coerce gtInt8#
  (≥) = coerce geInt8#
  (<) = coerce ltInt8#
  (≤) = coerce leInt8#
  cmp a b = Ordering# do gtInt8# a b -# ltInt8# a b
  min x y = x ≤ y ? x $ y
  max x y = x ≥ y ? x $ y

instance (≡) I16 where
  (≡) = coerce eqInt16#
  (≠) = coerce neInt16#
instance (≤) I16 where
  (>) = coerce gtInt16#
  (≥) = coerce geInt16#
  (<) = coerce ltInt16#
  (≤) = coerce leInt16#
  cmp a b = Ordering# do gtInt16# a b -# ltInt16# a b
  min x y = x ≤ y ? x $ y
  max x y = x ≥ y ? x $ y

instance (≡) I32 where
  (≡) = coerce eqInt32#
  (≠) = coerce neInt32#
instance (≤) I32 where
  (>) = coerce gtInt32#
  (≥) = coerce geInt32#
  (<) = coerce ltInt32#
  (≤) = coerce leInt32#
  cmp a b = Ordering# do gtInt32# a b -# ltInt32# a b
  min x y = x ≤ y ? x $ y
  max x y = x ≥ y ? x $ y

instance (≡) I64 where
  (≡) = coerce eqInt64#
  (≠) = coerce neInt64#
instance (≤) I64 where
  (>) = coerce gtInt64#
  (≥) = coerce geInt64#
  (<) = coerce ltInt64#
  (≤) = coerce leInt64#
  cmp a b = Ordering# do gtInt64# a b -# ltInt64# a b
  min x y = x ≤ y ? x $ y
  max x y = x ≥ y ? x $ y

instance (≡) U where
  (≡) = coerce eqWord#
  (≠) = coerce neWord#
instance (≤) U where
  (>) = coerce gtWord#
  (≥) = coerce geWord#
  (<) = coerce ltWord#
  (≤) = coerce leWord#
  cmp a b = Ordering# do gtWord# a b -# ltWord# a b
  min x y = x ≤ y ? x $ y
  max x y = x ≥ y ? x $ y

instance (≡) U8 where
  (≡) = coerce eqWord8#
  (≠) = coerce neWord8#
instance (≤) U8 where
  (>) = coerce gtWord8#
  (≥) = coerce geWord8#
  (<) = coerce ltWord8#
  (≤) = coerce leWord8#
  cmp a b = Ordering# do gtWord8# a b -# ltWord8# a b
  min x y = x ≤ y ? x $ y
  max x y = x ≥ y ? x $ y

instance (≡) U16 where
  (≡) = coerce eqWord16#
  (≠) = coerce neWord16#
instance (≤) U16 where
  (>) = coerce gtWord16#
  (≥) = coerce geWord16#
  (<) = coerce ltWord16#
  (≤) = coerce leWord16#
  cmp a b = Ordering# do gtWord16# a b -# ltWord16# a b
  min x y = x ≤ y ? x $ y
  max x y = x ≥ y ? x $ y

instance (≡) U32 where
  (≡) = coerce eqWord32#
  (≠) = coerce neWord32#
instance (≤) U32 where
  (>) = coerce gtWord32#
  (≥) = coerce geWord32#
  (<) = coerce ltWord32#
  (≤) = coerce leWord32#
  cmp a b = Ordering# do gtWord32# a b -# ltWord32# a b
  min x y = x ≤ y ? x $ y
  max x y = x ≥ y ? x $ y

instance (≡) U64 where
  (≡) = coerce eqWord64#
  (≠) = coerce neWord64#
instance (≤) U64 where
  (>) = coerce gtWord64#
  (≥) = coerce geWord64#
  (<) = coerce ltWord64#
  (≤) = coerce leWord64#
  cmp a b = Ordering# do gtWord64# a b -# ltWord64# a b
  min x y = x ≤ y ? x $ y
  max x y = x ≥ y ? x $ y

instance (≡) Char where
  (≡) = coerce eqChar#
  (≠) = coerce neChar#
instance (≤) Char where
  (>) = coerce gtChar#
  (≥) = coerce geChar#
  (<) = coerce ltChar#
  (≤) = coerce leChar#
  cmp a b = Ordering# do gtChar# a b -# ltChar# a b
  min x y = x ≤ y ? x $ y
  max x y = x ≥ y ? x $ y
deriving newtype instance (≡) Char8
deriving newtype instance (≤) Char8

instance (≡) F32 where
  (≡) = coerce eqFloat#
  (≠) = coerce neFloat#
instance (≤) F32 where
  (>) = coerce gtFloat#
  (≥) = coerce geFloat#
  (<) = coerce ltFloat#
  (≤) = coerce leFloat#
  cmp a b = Ordering# do gtFloat# a b -# ltFloat# a b
  min x y = x ≤ y ? x $ y
  max x y = x ≥ y ? x $ y
instance (≡) F64 where
  (≡) = coerce (==##)
  (≠) = coerce (/=##)
instance (≤) F64 where
  (>) = coerce (>##)
  (≥) = coerce (>=##)
  (<) = coerce (<##)
  (≤) = coerce (<=##)
  cmp a b = Ordering# do (a >## b) +# (a >=## b) -# 1#
  min x y = x ≤ y ? x $ y
  max x y = x ≥ y ? x $ y

-- | _Value_ equality
instance (≡) Bytes where
  a ≡ b = let n = coerce sizeofByteArray# a in
          case n ==# coerce sizeofByteArray# b of
                 0# -> F#
                 1# -> coerce compareByteArrays# a 0# b 0# n ≡ 0#
  as ≠ bs = (¬) (as ≡ bs)
-- | _Reference_ equality
instance (≡) Bytes_Pinned where
  (≡) = coerce sameByteArray#
  as ≠ bs = (¬) (as ≡ bs)
-- | _Reference_ equality
instance (≡) (Bytes_Pinned_M s) where
  (≡) = coerce sameMutableByteArray#
  as ≠ bs = (¬) (as ≡ bs)
  
-- | _Reference_ equality
instance (≡) (A_Box_Small x) where
  (≡) = coerce (sameSmallArray# @x)
  as ≠ bs = (¬) (as ≡ bs)
-- | _Reference_ equality
instance (≡) (A_Box_Small_M x s) where
  (≡) = coerce (sameSmallMutableArray# @_ @x)
  as ≠ bs = (¬) (as ≡ bs)
-- | _Reference_ equality
instance (≡) (A_Box x) where
  (≡) = coerce (sameArray# @x)
  as ≠ bs = (¬) (as ≡ bs)
-- | _Reference_ equality
instance (≡) (A_Box_M x s) where
  (≡) = coerce (sameMutableArray# @_ @x)
  as ≠ bs = (¬) (as ≡ bs)
-- | _Reference_ equality
instance (≡) (P_Box s x) where
  (≡) = coerce (sameMutVar# @s @x)
  as ≠ bs = (¬) (as ≡ bs)
-- | _Reference_ equality
instance (≡) (P_Async x) where
  (≡) = coerce (sameTVar# @_ @x)
  as ≠ bs = (¬) (as ≡ bs)
-- | _Reference_ equality
instance (≡) (P_Sync x) where
  (≡) = coerce (sameMVar# @_ @x)
  as ≠ bs = (¬) (as ≡ bs)
-- | _Reference_ equality
instance (≡) (P_Stable x) where
  (≡) = coerce (eqStablePtr# @x)
  as ≠ bs = (¬) (as ≡ bs)
-- | _Reference_ equality
instance (≡) (P_Stable_Name a) where
  (≡) = coerce eqStableName#
  as ≠ bs = (¬) (as ≡ bs)
-- | _Value_ equality
instance (≡) Buffer where
  a ≡ b = case cmp a b of EQ → T#; _ → F#
  as ≠ bs = (¬) (as ≡ bs)
-- | _Reference_ equality
instance (≡) Buffer_Pinned where
  PinnedBytes_Off_Len# (# a, i, n #) ≡ PinnedBytes_Off_Len# (# b , j, m #)
    = B# ((i ==# j) `andI#` (n ==# m) `andI#` sameByteArray# a b)
  as ≠ bs = (¬) (as ≡ bs)
  
instance (≤) Buffer where
  Bytes_Off_Len# (# a, i, n #) `cmp` Bytes_Off_Len# (# b , j, m #)
    = case cmp n m of
        EQ → Ordering# (compareByteArrays# a i b j n)
        LT → case Ordering# (compareByteArrays# a i b j n) of {EQ → LT; o → o}
        GT → case Ordering# (compareByteArrays# a i b j m) of {EQ → GT; o → o}
  a < b = cmp a b ≡ LT
  a > b = cmp a b ≡ GT
  a ≥ b = cmp a b ≠ LT
  a ≤ b = cmp a b ≠ GT
  min x y = x ≤ y ? x $ y
  max x y = x ≥ y ? x $ y

instance (≡) Addr# where (≡) = coerce eqAddr# ; (≠) = coerce neAddr#
instance (≤) Addr# where
  (>) = coerce gtAddr#
  (≥) = coerce geAddr#
  (<) = coerce ltAddr#
  (≤) = coerce leAddr#
  cmp a b = Ordering# do gtAddr# a b -# ltAddr# a b
  min x y = x ≤ y ? x $ y
  max x y = x ≥ y ? x $ y
deriving via Addr# instance (≡) (P_Unbox x)
deriving via Addr# instance (≤) (P_Unbox x)
