module Array.I16 where
import Array.Byte

index# ∷ A
       → I64 -- Offset in elments
       → I16
index# = indexInt16Array#

index## ∷ A
        → I64 -- Offset in bytes
        → I16
index## = indexWord8ArrayAsInt16#

read# ∷ M s → I64 → ST s I16
read# = readInt16Array#

write# ∷ M s → I64 → I16 → ST_ s
write# = writeInt16Array#
