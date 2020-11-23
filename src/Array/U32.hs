module Array.U32 where
import Array.Byte

index# ∷ A → I {- ^ Offset in elments -} → U32
index# = coerce indexWord32Array#

index## ∷ A → I {- ^ Offset in bytes -} → U32
index## = coerce indexWord8ArrayAsWord32#

read# ∷ MA s → I → ST# s U32
read# = coerce readWord32Array#

write# ∷ MA s → I → U32 → ST_# s
write# = coerce writeWord32Array#
