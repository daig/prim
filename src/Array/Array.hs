-- | Description : /Unlifted/ arrays. Containing strictly evaluated pointers.
-- Unlike "Array.Byte", these store pointers, but like it, they're guarenteed not to be bottom.
-- Eg. We cannot store an 'Addr', because it might be null, but we can store "Array.Byte",
-- and more "Array.Array"
module Array.Array where

type A = ArrayArray#
type MA = MutableArrayArray#

new ∷ I → ST# s (MA s)
new = newArrayArray#

instance (≡) (MA s) where (≡) = coerce sameMutableArrayArray#

freeze## ∷ MA s → ST# s A
freeze## = unsafeFreezeArrayArray#

size ∷ A → I
size = sizeofArrayArray#

sizeMA ∷ MA s → I
sizeMA = sizeofMutableArrayArray#

copy# ∷ A → I → MA s → I → I → ST_# s
copy# = copyArrayArray#

copyMA# ∷ MA s → I → MA s → I → I → ST_# s
copyMA# = copyMutableArrayArray#
