module Array.I8 where
import Array.Byte

index# ∷ A
       → I64 -- ^ Offset in elments
       → I8
index# = indexInt8Array#

read# ∷ M s → I64 → ST s I8
read# = readInt8Array#

write# ∷ M s → I64 → I8 → ST_ s
write# = writeInt8Array#
