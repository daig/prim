-- | Description : Packed 'Array.Byte' Arrays of 'I8'
module Array.I8 where
import Array.Byte

index# ∷ A → I {- ^ Offset in elments -} → I8
index# = coerce indexInt8Array#

read# ∷ MA s → I → ST# s I8
read# = coerce readInt8Array#

write# ∷ MA s → I → I8 → ST_# s
write# = coerce writeInt8Array#
