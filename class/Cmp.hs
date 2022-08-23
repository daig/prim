{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE UnliftedDatatypes #-}
module Cmp where
import {-# source #-} Bits

infix 4 >, ≥, <, ≤, ≡, ≠, `cmp`
class (≡) (a ∷ T r) where (≡), (≠) ∷ a → a → B
class (≡) a ⇒ (≤) (a ∷ T r) where
  (>),(≥),(<),(≤) ∷ a → a → B
  cmp ∷ a → a → Ordering


deriving newtype instance (≡) B
deriving newtype instance (≤) B

instance (≡) I where
  (≡) = coerce (==#)
  (≠) = coerce (/=#)
instance (≤) I where
  (>) = coerce (>#)
  (≥) = coerce (>=#)
  (<) = coerce (<#)
  (≤) = coerce (<=#)
  cmp a b = Ordering# do (a ># b) +# (a >=# b) Prelude.-# 1#

instance (≡) I8 where
  (≡) = coerce eqInt8#
  (≠) = coerce neInt8#
instance (≤) I8 where
  (>) = coerce gtInt8#
  (≥) = coerce geInt8#
  (<) = coerce ltInt8#
  (≤) = coerce leInt8#
  cmp a b = Ordering# do (gtInt8# a b) +# (geInt8# a b) Prelude.-# 1#

instance (≡) I16 where
  (≡) = coerce eqInt16#
  (≠) = coerce neInt16#
instance (≤) I16 where
  (>) = coerce gtInt16#
  (≥) = coerce geInt16#
  (<) = coerce ltInt16#
  (≤) = coerce leInt16#
  cmp a b = Ordering# do (gtInt16# a b) +# (geInt16# a b) Prelude.-# 1#

instance (≡) I32 where
  (≡) = coerce eqInt32#
  (≠) = coerce neInt32#
instance (≤) I32 where
  (>) = coerce gtInt32#
  (≥) = coerce geInt32#
  (<) = coerce ltInt32#
  (≤) = coerce leInt32#
  cmp a b = Ordering# do (gtInt32# a b) +# (geInt32# a b) Prelude.-# 1#

instance (≡) I64 where
  (≡) = coerce eqInt64#
  (≠) = coerce neInt64#
instance (≤) I64 where
  (>) = coerce gtInt64#
  (≥) = coerce geInt64#
  (<) = coerce ltInt64#
  (≤) = coerce leInt64#
  cmp a b = Ordering# do (gtInt64# a b) +# (geInt64# a b) Prelude.-# 1#

instance (≡) U where
  (≡) = coerce eqWord#
  (≠) = coerce neWord#
instance (≤) U where
  (>) = coerce gtWord#
  (≥) = coerce geWord#
  (<) = coerce ltWord#
  (≤) = coerce leWord#
  cmp a b = Ordering# do (gtWord# a b) +# (geWord# a b) Prelude.-# 1#

instance (≡) U8 where
  (≡) = coerce eqWord8#
  (≠) = coerce neWord8#
instance (≤) U8 where
  (>) = coerce gtWord8#
  (≥) = coerce geWord8#
  (<) = coerce ltWord8#
  (≤) = coerce leWord8#
  cmp a b = Ordering# do (gtWord8# a b) +# (geWord8# a b) Prelude.-# 1#

instance (≡) U16 where
  (≡) = coerce eqWord16#
  (≠) = coerce neWord16#
instance (≤) U16 where
  (>) = coerce gtWord16#
  (≥) = coerce geWord16#
  (<) = coerce ltWord16#
  (≤) = coerce leWord16#
  cmp a b = Ordering# do (gtWord16# a b) +# (geWord16# a b) Prelude.-# 1#

instance (≡) U32 where
  (≡) = coerce eqWord32#
  (≠) = coerce neWord32#
instance (≤) U32 where
  (>) = coerce gtWord32#
  (≥) = coerce geWord32#
  (<) = coerce ltWord32#
  (≤) = coerce leWord32#
  cmp a b = Ordering# do (gtWord32# a b) +# (geWord32# a b) Prelude.-# 1#

instance (≡) U64 where
  (≡) = coerce eqWord64#
  (≠) = coerce neWord64#
instance (≤) U64 where
  (>) = coerce gtWord64#
  (≥) = coerce geWord64#
  (<) = coerce ltWord64#
  (≤) = coerce leWord64#
  cmp a b = Ordering# do (gtWord64# a b) +# (geWord64# a b) Prelude.-# 1#

instance (≡) Char where
  (≡) = coerce eqChar#
  (≠) = coerce neChar#
instance (≤) Char where
  (>) = coerce gtChar#
  (≥) = coerce geChar#
  (<) = coerce ltChar#
  (≤) = coerce leChar#
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
instance (≡) F64 where
  (≡) = coerce (==##)
  (≠) = coerce (/=##)
instance (≤) F64 where
  (>) = coerce (>##)
  (≥) = coerce (>=##)
  (<) = coerce (<##)
  (≤) = coerce (<=##)

instance (≡) (M_A# s) where
  (≡) = coerce sameMutableByteArray#
  as ≠ bs = (¬) (as ≡ bs)
instance (≡) (M_A_Small x s) where
  (≡) = coerce (sameSmallMutableArray# @_ @x)
  as ≠ bs = (¬) (as ≡ bs)
instance (≡) (M_A x s) where
  (≡) = coerce (sameMutableArray# @_ @x)
  as ≠ bs = (¬) (as ≡ bs)
instance (≡) (P s x) where
  (≡) = coerce (sameMutVar# @s @x)
  as ≠ bs = (¬) (as ≡ bs)
instance (≡) (P_Async s x) where
  (≡) = coerce (sameTVar# @s @x)
  as ≠ bs = (¬) (as ≡ bs)
instance (≡) (P_Sync s x) where
  (≡) = coerce (sameMVar# @s @x)
  as ≠ bs = (¬) (as ≡ bs)
instance (≡) (P_Stable x) where
  (≡) = coerce (eqStablePtr# @x)
  as ≠ bs = (¬) (as ≡ bs)
instance (≡) V# where
  a ≡ b = case cmp a b of EQ → T; _ → F
  
instance (≤) V# where
  V# (# UnpinnedByteArray# a, i, n #) `cmp` V# (# UnpinnedByteArray# b , j, m #)
    = let mn = case n < m of {T → n; _ → m}
      in Ordering# do compareByteArrays# a i b j mn
