module Array.U8 where
import Array.Byte

index# ∷ A → I → U8
index# = coerce indexWord8Array#

read# ∷ MA s → I → ST# s U8
read# = coerce readWord8Array#

write# ∷ MA s → I → U8 → ST_# s
write# = coerce writeWord8Array#
