-- | /Unlifted/ arrays, containing strictly evaluated pointers.
-- Unlike "Array.Byte", these store pointers, but like it, they're guarenteed not to be bottom.
-- Eg. We cannot store an 'Addr', because it might be null, but we can store "Array.Byte.Array",
-- and more "Array.Array"
module Array.Array where

type A = ArrayArray#
type M = MutableArrayArray#

new ∷ I64 → ST s (M s)
new = newArrayArray#

eq ∷ M s → M s → B
eq = sameMutableArrayArray#

freeze## ∷ M s → ST s A
freeze## = unsafeFreezeArrayArray#

size ∷ A → I64
size = sizeofArrayArray#

sizeM ∷ M s → I64
sizeM = sizeofMutableArrayArray#

copy# ∷ A → I64 → M s → I64 → I64 → ST_ s
copy# = copyArrayArray#

copyM# ∷ M s → I64 → M s → I64 → I64 → ST_ s
copyM# = copyMutableArrayArray#
