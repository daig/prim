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

instance Copy AR_## (AR# s) s where
  AR__Off_Len# (# a, i, n #) ~> AR_Off# (# b, j #) = copyArray# a i b j n
instance Copy (AR## s) (AR# s) s where
  AR_Off_Len# (# a, i, n #) ~> AR_Off# (# b, j #) = copyMutableArray# a i b j n

instance Copy Ar_## (Ar# s) s where
  Ar__Off_Len# (# a, i, n #) ~> Ar_Off# (# b, j #) = copySmallArray# a i b j n
instance Copy (Ar## s) (Ar# s) s where
  Ar_Off_Len# (# a, i, n #) ~> Ar_Off# (# b, j #) = copySmallMutableArray# a i b j n

instance Copy A_## (A# s) s where
  (~>) ∷ ∀ x. Prim x ⇒ A_## x → A# s x → ST_ s
  Bytes'_Off_Len# (# a, size @x → i, size @x → n #) ~> Bytes_Off# (# b, size @x → j #) = copyByteArray# a i b j n

instance Copy Pinned_## (Pinned# s) s where
  (~>) ∷ ∀ x. Prim x ⇒ Pinned_## x → Pinned# s x → ST_ s
  Pinned__Off_Len# (# a, size @x → i, size @x → n #) ~> Pinned_Off# (# b, size @x → j #) = copyByteArray# a i b j n

instance Copy A_## (Pinned# s) s where
  (~>) ∷ ∀ x. Prim x ⇒ A_## x → Pinned# s x → ST_ s
  Bytes'_Off_Len# (# a, size @x → i, size @x → n #) ~> Pinned_Off# (# b, size @x → j #) = copyByteArray# a i b j n

instance Copy Pinned_## (A# s) s where
  (~>) ∷ ∀ x. Prim x ⇒ Pinned_## x → A# s x → ST_ s
  Pinned__Off_Len# (# a, size @x → i, size @x → n #) ~> Bytes_Off# (# b, size @x → j #) = copyByteArray# a i b j n

instance Copy (A## s) (A# s) s where
  (~>) ∷ ∀ x. Prim x ⇒ A## s x → A# s x → ST_ s
  Bytes_Off_Len# (# a, size @x → i, size @x → n #) ~> Bytes_Off# (# b, size @x → j #) = copyMutableByteArray# a i b j n

instance Copy A_## (P s) s where
  (~>) ∷ ∀ x. Prim x ⇒ A_## x → P s x → ST_ s
  Bytes'_Off_Len# (# src, size @x → i, size @x → n #) ~> P# dst = copyByteArrayToAddr# src i dst n

instance Copy Pinned_## (P s) s where
  (~>) ∷ ∀ x. Prim x ⇒ Pinned_## x → P s x → ST_ s
  Pinned__Off_Len# (# src, size @x → i, size @x → n #) ~> P# dst = copyByteArrayToAddr# src i dst n

instance Copy (A## s) (P s) s where
  (~>) ∷ ∀ x. Prim x ⇒ A## s x → P s x → ST_ s
  Bytes_Off_Len# (# src, size @x → i, size @x → n #) ~> P# dst = copyMutableByteArrayToAddr# src i dst n

instance Copy (Pinned## s) (P s) s where
  (~>) ∷ ∀ x. Prim x ⇒ Pinned## s x → P s x → ST_ s
  Pinned_Off_Len# (# src, size @x → i, size @x → n #) ~> P# dst = copyMutableByteArrayToAddr# src i dst n

instance Copy P_## (A# s) s where
  (~>) ∷ ∀ x. Prim x ⇒ P_## x → A# s x → ST_ s
  P__Len# (# src, size @x → n #) ~> Bytes_Off# (# dst, i #) = copyAddrToByteArray# src dst i n

instance Copy P_## (Pinned# s) s where
  (~>) ∷ ∀ x. Prim x ⇒ P_## x → Pinned# s x → ST_ s
  P__Len# (# src, size @x → n #) ~> Pinned_Off# (# dst, i #) = copyAddrToByteArray# src dst i n

instance Copy (P## s) (A# s) s where
  (~>) ∷ ∀ x. Prim x ⇒ P## s x → A# s x → ST_ s
  P_Len# (# src, size @x → n #) ~> Bytes_Off# (# dst, i #) = copyAddrToByteArray# src dst i n

instance Copy (P## s) (Pinned# s) s where
  (~>) ∷ ∀ x. Prim x ⇒ P## s x → Pinned# s x → ST_ s
  P_Len# (# src, size @x → n #) ~> Pinned_Off# (# dst, i #) = copyAddrToByteArray# src dst i n

-- TODO: add units to sizes eg elts and bytes @Count@
