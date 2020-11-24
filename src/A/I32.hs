-- | Description : Packed 'Array.Byte' Arrays of 'I32'
module A.I32 where
import A

index# ∷ A → I {- ^ Offset in elments -} → I32
index# = coerce indexInt32Array#

index## ∷ A → I {- ^ Offset in bytes -} → I32
index## = coerce indexWord8ArrayAsInt32#

read# ∷ MA s → I → ST# s I32
read# = coerce readInt32Array#

write# ∷ MA s → I → I32 → ST_# s
write# = coerce writeInt32Array#
