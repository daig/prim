{-# language InstanceSigs #-}
module Array.Copy where
import Prim
import Action

type Copy ∷ ∀ {rx} {rsrc} {rdst}. (T rx → T rsrc) → (T rx → T rdst) → ★ → TC
class Copy src dst s where
  -- | Copy the elements from the source to the destination.
  -- Both must fully contain the specified ranges and not overlap in memory,
  -- but this is not checked.
  --
  -- Warning: this can fail with an unchecked exception.
  (~>) ∷ ∀ x. Elt dst x ⇒ src x → dst x → ST_ s

instance Copy AR'## (AR# s) s where
  AR'_Off_Len# (# a, i, n #) ~> AR_Off# (# b, j #) = copyArray# a i b j n
instance Copy (AR## s) (AR# s) s where
  AR_Off_Len# (# a, i, n #) ~> AR_Off# (# b, j #) = copyMutableArray# a i b j n

instance Copy Ar'## (Ar# s) s where
  Ar'_Off_Len# (# a, i, n #) ~> Ar_Off# (# b, j #) = copySmallArray# a i b j n
instance Copy (Ar## s) (Ar# s) s where
  Ar_Off_Len# (# a, i, n #) ~> Ar_Off# (# b, j #) = copySmallMutableArray# a i b j n

instance Copy A'## (A# s) s where
  (~>) ∷ ∀ x. Prim x ⇒ A'## x → A# s x → ST_ s
  Bytes'_Off_Len# (# a, size @x → i, size @x → n #) ~> Bytes_Off# (# b, size @x → j #) = copyByteArray# a i b j n

instance Copy A'_## (A_# s) s where
  (~>) ∷ ∀ x. Prim x ⇒ A'_## x → A_# s x → ST_ s
  Pinned'_Off_Len# (# a, size @x → i, size @x → n #) ~> Pinned_Off# (# b, size @x → j #) = copyByteArray# a i b j n

instance Copy A'## (A_# s) s where
  (~>) ∷ ∀ x. Prim x ⇒ A'## x → A_# s x → ST_ s
  Bytes'_Off_Len# (# a, size @x → i, size @x → n #) ~> Pinned_Off# (# b, size @x → j #) = copyByteArray# a i b j n

instance Copy A'_## (A# s) s where
  (~>) ∷ ∀ x. Prim x ⇒ A'_## x → A# s x → ST_ s
  Pinned'_Off_Len# (# a, size @x → i, size @x → n #) ~> Bytes_Off# (# b, size @x → j #) = copyByteArray# a i b j n

instance Copy (A## s) (A# s) s where
  (~>) ∷ ∀ x. Prim x ⇒ A## s x → A# s x → ST_ s
  Bytes_Off_Len# (# a, size @x → i, size @x → n #) ~> Bytes_Off# (# b, size @x → j #) = copyMutableByteArray# a i b j n

instance Copy A'## (P s) s where
  (~>) ∷ ∀ x. Prim x ⇒ A'## x → P s x → ST_ s
  Bytes'_Off_Len# (# src, size @x → i, size @x → n #) ~> P# dst = copyByteArrayToAddr# src i dst n

instance Copy A'_## (P s) s where
  (~>) ∷ ∀ x. Prim x ⇒ A'_## x → P s x → ST_ s
  Pinned'_Off_Len# (# src, size @x → i, size @x → n #) ~> P# dst = copyByteArrayToAddr# src i dst n

instance Copy (A## s) (P s) s where
  (~>) ∷ ∀ x. Prim x ⇒ A## s x → P s x → ST_ s
  Bytes_Off_Len# (# src, size @x → i, size @x → n #) ~> P# dst = copyMutableByteArrayToAddr# src i dst n

instance Copy (A_## s) (P s) s where
  (~>) ∷ ∀ x. Prim x ⇒ A_## s x → P s x → ST_ s
  Pinned_Off_Len# (# src, size @x → i, size @x → n #) ~> P# dst = copyMutableByteArrayToAddr# src i dst n

instance Copy P'## (A# s) s where
  (~>) ∷ ∀ x. Prim x ⇒ P'## x → A# s x → ST_ s
  P'_Len# (# src, size @x → n #) ~> Bytes_Off# (# dst, i #) = copyAddrToByteArray# src dst i n

instance Copy P'## (A_# s) s where
  (~>) ∷ ∀ x. Prim x ⇒ P'## x → A_# s x → ST_ s
  P'_Len# (# src, size @x → n #) ~> Pinned_Off# (# dst, i #) = copyAddrToByteArray# src dst i n

instance Copy (P## s) (A# s) s where
  (~>) ∷ ∀ x. Prim x ⇒ P## s x → A# s x → ST_ s
  P_Len# (# src, size @x → n #) ~> Bytes_Off# (# dst, i #) = copyAddrToByteArray# src dst i n

instance Copy (P## s) (A_# s) s where
  (~>) ∷ ∀ x. Prim x ⇒ P## s x → A_# s x → ST_ s
  P_Len# (# src, size @x → n #) ~> Pinned_Off# (# dst, i #) = copyAddrToByteArray# src dst i n

-- TODO: add units to sizes eg elts and bytes @Count@
