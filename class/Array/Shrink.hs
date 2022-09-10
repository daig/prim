module Array.Shrink where
import Array

class Array a ⇒ Shrink (a ∷ T_) where shrink ∷ M a s → I → ST_ s

instance Shrink (A_Box_Small x) where shrink = coerce (shrinkSmallMutableArray# @_ @x)
instance Shrink (A_Unbox x) where shrink = coerce shrinkMutableByteArray#
instance Shrink Bytes_Pinned where shrink = coerce shrinkMutableByteArray#
