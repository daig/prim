module Array.U16 where
import Array.Byte

index# ∷ A → I {- ^ Offset in elments -} → U16
index# = coerce indexWord16Array#

index## ∷ A → I {- ^ Offset in bytes -} → U16
index## = coerce indexWord8ArrayAsWord16#


read# ∷ MA s → I → ST# s U16
read# = coerce readWord16Array#

write# ∷ MA s → I → U16 → ST_# s
write# = coerce writeWord16Array#
