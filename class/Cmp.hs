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

instance (≡) U where
  (≡) = coerce eqWord#
  (≠) = coerce neWord#
instance (≤) U where
  (>) = coerce gtWord#
  (≥) = coerce geWord#
  (<) = coerce ltWord#
  (≤) = coerce leWord#

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
instance (≡) (M_A x s) where
  (≡) = coerce (sameSmallMutableArray# @_ @x)
  as ≠ bs = (¬) (as ≡ bs)
instance (≡) (M_Arr x s) where
  (≡) = coerce (sameMutableArray# @_ @x)
  as ≠ bs = (¬) (as ≡ bs)
instance (≡) (P x) where
  (≡) = coerce (sameMutVar# @_ @x)
  as ≠ bs = (¬) (as ≡ bs)
instance (≡) (P_Async x) where
  (≡) = coerce (sameTVar# @_ @x)
  as ≠ bs = (¬) (as ≡ bs)
instance (≡) (P_Sync x) where
  (≡) = coerce (sameMVar# @_ @x)
  as ≠ bs = (¬) (as ≡ bs)
instance (≡) (P_Stable x) where
  (≡) = coerce (eqStablePtr# @x)
  as ≠ bs = (¬) (as ≡ bs)
instance (≡) (# A# ,I , I #) where
  a ≡ b = case cmp a b of EQ → T; _ → F
  
instance (≤) (# A# ,I , I #) where
  (# UnpinnedByteArray# a, i, n #) `cmp` (# UnpinnedByteArray# b , j, m #)
    = case cmp m n of
        EQ → Ordering# do compareByteArrays# a i b i m
        o → o
