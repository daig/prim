module Array.U32 where
import Array.Byte

index# ∷ A
       → I64 -- Offset in elments
       → U32
index# = indexWord32Array#

index## ∷ A
        → I64 -- Offset in bytes
        → U32
index## = indexWord8ArrayAsWord32#

read# ∷ M s → I64 → ST s U32
read# = readWord32Array#

write# ∷ M s → I64 → U32 → ST_ s
write# = writeWord32Array#
