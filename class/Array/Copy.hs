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

instance Copy (Array# x) (MutableArray# s x) s where
 copy = coerce (copyArray# @x)
instance Copy (MutableArray# s x) (MutableArray# s x) s where
 copy = coerce (copyMutableArray# @_ @x)

instance Copy ByteArray# (MutableByteArray# s) s where
 copy = coerce copyByteArray#
instance Copy (MutableByteArray# s) (MutableByteArray# s) s where
 copy = coerce copyMutableByteArray#
instance Copy ByteArray# Addr# s where
 copy (coerce → src) i (coerce → dst) j n = copyByteArrayToAddr# src i (plusAddr# dst j) n
instance Copy (MutableByteArray# s) Addr# s where
 copy (coerce → src) i (coerce → dst) j n = copyMutableByteArrayToAddr# src i (plusAddr# dst j) n
instance Copy Addr# (MutableByteArray# s) s where
 copy src i dst j n = coerce copyAddrToByteArray# (plusAddr# src i) dst j n

instance Copy (UnboxedArray# x) (UnboxedMutableArray# s x) s where
 copy = coerce copyByteArray#
instance Copy (UnboxedMutableArray# s x) (UnboxedMutableArray# s x) s where
 copy = coerce copyMutableByteArray#
instance Copy (UnboxedArray# x) (ForeignMutableArray# s x)  s where
 copy (coerce → src) i (coerce → dst) j n = copyByteArrayToAddr# src i (plusAddr# dst j) n
instance Copy (UnboxedMutableArray# s x) (ForeignMutableArray# s x) s where
 copy (coerce → src) i (coerce → dst) j n = copyMutableByteArrayToAddr# src i (plusAddr# dst j) n
instance Copy (ForeignArray# x) (UnboxedMutableArray# s x) s where
 copy (ConstAddr# src) i dst j n = coerce copyAddrToByteArray# (plusAddr# src i) dst j n

instance Copy (PinnedArray# x) (PinnedMutableArray# s x) s where
 copy = coerce copyByteArray#
instance Copy (PinnedMutableArray# s x) (PinnedMutableArray# s x) s where
 copy = coerce copyMutableByteArray#
instance Copy (PinnedArray# x) (ForeignMutableArray# s x) s where
 copy src i (coerce → dst) j n = coerce copyByteArrayToAddr# src i (plusAddr# dst j) n
instance Copy (PinnedMutableArray# s x) (ForeignArray# x) s where
 copy src i (ConstAddr# dst) j n = coerce copyMutableByteArrayToAddr# src i (plusAddr# dst j) n
instance Copy (ForeignArray# x) (PinnedMutableArray# s x) s where
 copy (ConstAddr# src) i dst j n = coerce copyAddrToByteArray# (plusAddr# src i) dst j n

-- | A.Small
instance Copy (SmallArray# a) (SmallMutableArray# s a) s where copy = coerce (copySmallArray# @a)
-- | A.Small
instance Copy (SmallMutableArray# s a) (SmallMutableArray# s a) s where copy = coerce (copySmallMutableArray# @_ @a)
