-- | Description : /Unlifted/ arrays. Containing strictly evaluated pointers.
-- Unlike "Array.Byte", these store pointers, but like it, they're guarenteed not to be bottom.
-- Eg. We cannot store an 'Addr', because it might be null, but we can store "Array.Byte",
-- and more "Array.Array"
module Array.Array where
import A

type A = ArrayArray#
type MA = MutableArrayArray#

new ∷ I → ST# s (MA s)
new = newArrayArray#

instance (≡) (MA s) where (≡) = coerce sameMutableArrayArray#

instance Freeze## A where freeze## = unsafeFreezeArrayArray#
instance Freeze# A where
  freeze# a off n s = case new n s of
    (# s' , ma #) → case copy a off ma 0# n s' of s'' → freeze## ma s''
-- | This is just a cast
instance Thaw## A where thaw## a s = (# s , unsafeCoerce# a #)
instance Thaw# A where
  thaw# a off n s = case new n s of
    (# s' , ma #) → case copy a off ma 0# n s' of s'' → (# s'' , ma #)

instance Size A where size = size
instance Size (MA s) where size = sizeofMutableArrayArray#
instance Copy A (MA s) s where copy = copyArrayArray#
instance Copy (MA s) (MA s) s where copy = copyMutableArrayArray#
