{-# language LinearTypes #-}
module Cmp where

infix 4 >, ≥, <, ≤, ≡, ≠, `cmp`
class (≡) (a ∷ T r) where (≡), (≠) ∷ a ⊸ a ⊸ B
class (≡) a ⇒ (≤) (a ∷ T r) where
  (>),(≥),(<),(≤) ∷ a ⊸ a ⊸ B
  cmp ∷ a ⊸ a ⊸ Ordering


deriving newtype instance (≡) B
deriving newtype instance (≤) B

instance (≡) I where
  (≡) = coerce (λ\i → λ\j → i ==# j)
  (≠) = coerce (λ\i → λ\j → i /=# j)
instance (≤) I where
  (>) = coerce (λ\i → λ\j → i ># j)
  (≥) = coerce (λ\i → λ\j → i >=# j)
  (<) = coerce (λ\i → λ\j → i <# j)
  (≤) = coerce (λ\i → λ\j → i <=# j)
  cmp = λ\a → λ\b → Ordering# do
    (a ># b) +# (a >=# b) Prelude.-# 1#

instance (≡) U where
  (≡) = coerce (λ\i → λ do eqWord# i)
  (≠) = coerce (λ\i → λ do neWord# i)
instance (≤) U where
  (>) = coerce (λ\i → λ do gtWord# i)
  (≥) = coerce (λ\i → λ do geWord# i)
  (<) = coerce (λ\i → λ do ltWord# i)
  (≤) = coerce (λ\i → λ do leWord# i)

instance (≡) Char where
  (≡) = coerce (λ\i → λ do eqChar# i)
  (≠) = coerce (λ\i → λ do neChar# i)
instance (≤) Char where
  (>) = coerce (λ\i → λ do gtChar# i)
  (≥) = coerce (λ\i → λ do geChar# i)
  (<) = coerce (λ\i → λ do ltChar# i)
  (≤) = coerce (λ\i → λ do leChar# i)
deriving newtype instance (≡) Char8
deriving newtype instance (≤) Char8

instance (≡) F32 where
  (≡) = coerce (λ\i → λ do eqFloat# i)
  (≠) = coerce (λ\i → λ do neFloat# i)
instance (≤) F32 where
  (>) = coerce (λ\i → λ do gtFloat# i)
  (≥) = coerce (λ\i → λ do geFloat# i)
  (<) = coerce (λ\i → λ do ltFloat# i)
  (≤) = coerce (λ\i → λ do leFloat# i)
instance (≡) F64 where
  (≡) = coerce (λ\i → λ\j → i ==## j)
  (≠) = coerce (λ\i → λ\j → i /=## j)
instance (≤) F64 where
  (>) = coerce (λ\i → λ\j → i >## j)
  (≥) = coerce (λ\i → λ\j → i >=## j)
  (<) = coerce (λ\i → λ\j → i <## j)
  (≤) = coerce (λ\i → λ\j → i <=## j)

instance (≡) M_A# where
  (≡) = coerce (λ\a → λ\b → sameMutableByteArray# a b)
  as ≠ bs = (¬) (as ≡ bs) where
instance (≡) (M_A x) where
  (≡) = coerce (λ\a → λ\b → sameSmallMutableArray# @_ @x a b)
  as ≠ bs = (¬) (as ≡ bs) where
instance (≡) (M_Arr x) where
  (≡) = coerce (λ\a → λ\b → sameMutableArray# @_ @x a b)
  as ≠ bs = (¬) (as ≡ bs) where
instance (≡) M_AA#  where
  (≡) = coerce (λ\a → λ\b → sameMutableArrayArray# a b)
  as ≠ bs = (¬) (as ≡ bs) where
deriving newtype instance (≡) (M_AA x)
instance (≡) (P x) where
  (≡) = coerce (λ\a → λ\b → sameMutVar# @_ @x a b)
  as ≠ bs = (¬) (as ≡ bs) where
instance (≡) (P_Async x) where
  (≡) = coerce (λ\a → λ\b → sameTVar# @_ @x a b)
  as ≠ bs = (¬) (as ≡ bs) where
instance (≡) (P_Sync x) where
  (≡) = coerce (λ\a → λ\b → sameMVar# @_ @x a b)
  as ≠ bs = (¬) (as ≡ bs) where
instance (≡) (P_Stable x) where
  (≡) = coerce (λ\a → λ\b → eqStablePtr# @x a b)
  as ≠ bs = (¬) (as ≡ bs) where
instance (≡) (# A# ,I , I #) where
  (≡) = λ\a → λ\b → case cmp a b of EQ → T; _ → F
  
instance (≤) (# A# ,I , I #) where
  cmp = λ\(# UnpinnedByteArray# a, i, n #) → λ\(# UnpinnedByteArray# b , j, m #)
    → case cmp m n of
        EQ → Ordering# do compareByteArrays# a i b i m
        o → o
