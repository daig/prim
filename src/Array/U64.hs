module Array.U64 where
import Array.Byte

index# ∷ A → I {- ^ Offset in elments -} → U64
index# = indexWord64Array#

index## ∷ A → I {- ^ Offset in bytes -} → U64
index## = indexWord8ArrayAsWord64#

read# ∷ MA s → I → ST# s U64
read# = readWord8Array#

write# ∷ MA s → I → U64 → ST_# s
write# = writeWord8Array#
