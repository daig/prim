-- | Description : /Unlifted/ arrays. Containing strictly evaluated pointers.
-- Unlike "Array.Byte", these store pointers, but like it, they're guarenteed not to be bottom.
-- Eg. We cannot store a 'P', because it might be null, but we can store "A.Byte",
-- and more "A.Unlifted"
module Refs where
import A

type A = ArrayArray#
type MA = MutableArrayArray#

instance (≡) (MA s) where
  (≡) = coerce sameMutableArrayArray#
  as ≠ bs = (¬) (as ≡ bs)

-- | @thaw##@ is just a cast
--   @new@ size in elements. Uninitialized entries point recursively to the array itself.
--   @lenM#@ is safe
instance 𝔸 A where
  new# = newArrayArray#
  freeze## = unsafeFreezeArrayArray#
  freeze# a off n s = case new# n s of
    (# s' , ma #) → case copy a off ma 0# n s' of s'' → freeze## ma s''
  thaw## a s = (# s , unsafeCoerce# a #)
  thaw# a off n s = case new# n s of
    (# s' , ma #) → case copy a off ma 0# n s' of s'' → (# s'' , ma #)
  len = sizeofArrayArray#
  lenM# = sizeofMutableArrayArray#
  lenM ma = \s → (# s , sizeofMutableArrayArray# ma #)

instance Copy A (MA s) s where copy = copyArrayArray#
instance Copy (MA s) (MA s) s where copy = copyMutableArrayArray#
