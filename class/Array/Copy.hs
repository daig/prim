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

instance Copy Slice (Ref s) s where
  Array_Off_Len# (# a, i, n #) ~> MutableArray_Off# (# b, j #) = copyArray# a i b j n
instance Copy (MutableSlice s) (Ref s) s where
  MutableArray_Off_Len# (# a, i, n #) ~> MutableArray_Off# (# b, j #) = copyMutableArray# a i b j n

instance Copy SmallSlice (SmallRef s) s where
  SmallArray_Off_Len# (# a, i, n #) ~> SmallMutableArray_Off# (# b, j #) = copySmallArray# a i b j n
instance Copy (SmallMutableSlice s) (SmallRef s) s where
  SmallMutableArray_Off_Len# (# a, i, n #) ~> SmallMutableArray_Off# (# b, j #) = copySmallMutableArray# a i b j n

instance Copy UnboxedSlice (UnboxedRef s) s where
  (~>) ∷ ∀ x. Prim x ⇒ UnboxedSlice x → UnboxedRef s x → ST_ s
  Bytes_Off_Len# (# a, size @x → i, size @x → n #) ~> MBytes_Off# (# b, size @x → j #) = copyByteArray# a i b j n

instance Copy PinnedSlice (PinnedRef s) s where
  (~>) ∷ ∀ x. Prim x ⇒ PinnedSlice x → PinnedRef s x → ST_ s
  PinnedBytes_Off_Len# (# a, size @x → i, size @x → n #) ~> MPinnedBytes_Off# (# b, size @x → j #) = copyByteArray# a i b j n

instance Copy UnboxedSlice (PinnedRef s) s where
  (~>) ∷ ∀ x. Prim x ⇒ UnboxedSlice x → PinnedRef s x → ST_ s
  Bytes_Off_Len# (# a, size @x → i, size @x → n #) ~> MPinnedBytes_Off# (# b, size @x → j #) = copyByteArray# a i b j n

instance Copy PinnedSlice (UnboxedRef s) s where
  (~>) ∷ ∀ x. Prim x ⇒ PinnedSlice x → UnboxedRef s x → ST_ s
  PinnedBytes_Off_Len# (# a, size @x → i, size @x → n #) ~> MBytes_Off# (# b, size @x → j #) = copyByteArray# a i b j n

instance Copy (UnboxedMutableSlice s) (UnboxedRef s) s where
  (~>) ∷ ∀ x. Prim x ⇒ UnboxedMutableSlice s x → UnboxedRef s x → ST_ s
  MBytes_Off_Len# (# a, size @x → i, size @x → n #) ~> MBytes_Off# (# b, size @x → j #) = copyMutableByteArray# a i b j n

instance Copy UnboxedSlice (ForeignMutableArray# s) s where
  (~>) ∷ ∀ x. Prim x ⇒ UnboxedSlice x → ForeignMutableArray# s x → ST_ s
  Bytes_Off_Len# (# src, size @x → i, size @x → n #) ~> Addr# dst = copyByteArrayToAddr# src i dst n

instance Copy PinnedSlice (ForeignMutableArray# s) s where
  (~>) ∷ ∀ x. Prim x ⇒ PinnedSlice x → ForeignMutableArray# s x → ST_ s
  PinnedBytes_Off_Len# (# src, size @x → i, size @x → n #) ~> Addr# dst = copyByteArrayToAddr# src i dst n

instance Copy (UnboxedMutableSlice s) (ForeignMutableArray# s) s where
  (~>) ∷ ∀ x. Prim x ⇒ UnboxedMutableSlice s x → ForeignMutableArray# s x → ST_ s
  MBytes_Off_Len# (# src, size @x → i, size @x → n #) ~> Addr# dst = copyMutableByteArrayToAddr# src i dst n

instance Copy (PinnedMutableSlice s) (ForeignMutableArray# s) s where
  (~>) ∷ ∀ x. Prim x ⇒ PinnedMutableSlice s x → ForeignMutableArray# s x → ST_ s
  PinnedMBytes_Off_Len# (# src, size @x → i, size @x → n #) ~> Addr# dst = copyMutableByteArrayToAddr# src i dst n

instance Copy ForeignSlice (UnboxedRef s) s where
  (~>) ∷ ∀ x. Prim x ⇒ ForeignSlice x → UnboxedRef s x → ST_ s
  Addr_Len# (# src, size @x → n #) ~> MBytes_Off# (# dst, i #) = copyAddrToByteArray# src dst i n

instance Copy ForeignSlice (PinnedRef s) s where
  (~>) ∷ ∀ x. Prim x ⇒ ForeignSlice x → PinnedRef s x → ST_ s
  Addr_Len# (# src, size @x → n #) ~> MPinnedBytes_Off# (# dst, i #) = copyAddrToByteArray# src dst i n

instance Copy (ForeignMutableSlice s) (UnboxedRef s) s where
  (~>) ∷ ∀ x. Prim x ⇒ ForeignMutableSlice s x → UnboxedRef s x → ST_ s
  MAddr_Len# (# src, size @x → n #) ~> MBytes_Off# (# dst, i #) = copyAddrToByteArray# src dst i n

instance Copy (ForeignMutableSlice s) (PinnedRef s) s where
  (~>) ∷ ∀ x. Prim x ⇒ ForeignMutableSlice s x → PinnedRef s x → ST_ s
  MAddr_Len# (# src, size @x → n #) ~> MPinnedBytes_Off# (# dst, i #) = copyAddrToByteArray# src dst i n

-- TODO: add units to sizes eg elts and bytes @Count@
