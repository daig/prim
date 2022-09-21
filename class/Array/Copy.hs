module Array.Copy where

class Copy (src ∷ T r) (dst ∷ T r') s where
  -- | Copy the elements from the source to the destination.
  -- Both must fully contain the specified ranges and not overlap in memory,
  -- but this is not checked.
  --
  -- Warning: this can fail with an unchecked exception.
  copy ∷ src
       → I -- ^ Source Offset (bytes)
       → dst
       → I -- ^ Destination Offset (bytes)
       → I -- ^ Number of elements to copy
       → ST_ s

instance Copy (A_Box x) (A_Box_M x s) s where
 copy = coerce (copyArray# @x)
instance Copy (A_Box_M x s) (A_Box_M x s) s where
 copy = coerce (copyMutableArray# @_ @x)

instance Copy Bytes (Bytes_M s) s where
 copy = coerce copyByteArray#
instance Copy (Bytes_M s) (Bytes_M s) s where
 copy = coerce copyMutableByteArray#
instance Copy Bytes (P_M# s)  s where
 copy (coerce → src) i (coerce → dst) j n = copyByteArrayToAddr# src i (plusAddr# dst j) n
instance Copy (Bytes_M s) (P_M# s) s where
 copy (coerce → src) i (coerce → dst) j n = copyMutableByteArrayToAddr# src i (plusAddr# dst j) n
instance Copy Addr# (Bytes_M s) s where
 copy src i dst j n = coerce copyAddrToByteArray# (plusAddr# src i) dst j n
instance Copy (P_M# s) (Bytes_M s) s where
 copy (coerce → src) i (coerce → dst) j n = copyAddrToByteArray# (plusAddr# src i) dst j n

instance Copy (A_Unbox x) (A_Unbox_M x s) s where
 copy = coerce copyByteArray#
instance Copy (A_Unbox_M x s) (A_Unbox_M x s) s where
 copy = coerce copyMutableByteArray#
instance Copy (A_Unbox x) (P_Unbox_M x s)  s where
 copy (coerce → src) i (coerce → dst) j n = copyByteArrayToAddr# src i (plusAddr# dst j) n
instance Copy (A_Unbox_M x s) (P_Unbox_M x s) s where
 copy (coerce → src) i (coerce → dst) j n = copyMutableByteArrayToAddr# src i (plusAddr# dst j) n
instance Copy (P_Unbox x) (A_Unbox_M x s) s where
 copy (P# src) i dst j n = coerce copyAddrToByteArray# (plusAddr# src i) dst j n

instance Copy Bytes_Pinned (Bytes_Pinned_M s) s where
 copy = coerce copyByteArray#
instance Copy (Bytes_Pinned_M s) (Bytes_Pinned_M s) s where
 copy = coerce copyMutableByteArray#
instance Copy Bytes_Pinned (P_M# s) s where
 copy src i (coerce → dst) j n = coerce copyByteArrayToAddr# src i (plusAddr# dst j) n
instance Copy (Bytes_Pinned_M s) (P_M# s) s where
 copy src i (coerce → dst) j n = coerce copyMutableByteArrayToAddr# src i (plusAddr# dst j) n
instance Copy Addr# (Bytes_Pinned_M s) s where
 copy src i dst j n = coerce copyAddrToByteArray# (plusAddr# src i) dst j n

instance Copy (A_Unbox_Pinned x) (A_Unbox_Pinned_M x s) s where
 copy = coerce copyByteArray#
instance Copy (A_Unbox_Pinned_M x s) (A_Unbox_Pinned_M x s) s where
 copy = coerce copyMutableByteArray#
instance Copy (A_Unbox_Pinned x) (P_Unbox_M x s) s where
 copy src i (coerce → dst) j n = coerce copyByteArrayToAddr# src i (plusAddr# dst j) n
instance Copy (A_Unbox_Pinned_M x s) (P_Unbox x) s where
 copy src i (P# dst) j n = coerce copyMutableByteArrayToAddr# src i (plusAddr# dst j) n
instance Copy (P_Unbox x) (A_Unbox_Pinned_M x s) s where
 copy (P# src) i dst j n = coerce copyAddrToByteArray# (plusAddr# src i) dst j n

-- | A.Small
instance Copy (A_Box_Small a) (A_Box_Small_M a s) s where copy = coerce (copySmallArray# @a)
-- | A.Small
instance Copy (A_Box_Small_M a s) (A_Box_Small_M a s) s where copy = coerce (copySmallMutableArray# @_ @a)
